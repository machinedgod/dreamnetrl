{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Control.Lens              (makeLenses, use, uses, view, (.=), assign,
                                  (%=), (+=), (-=))
import Control.Monad             (void, when)
import Control.Monad.Free        (Free)
import Control.Monad.State       (StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.Functor              (($>))
import Data.Bool                 (bool)
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2))

import qualified Data.Map    as M    ((!), lookup, empty, fromList)
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.DesignData
import Dreamnet.GameState
import qualified Dreamnet.Input as Input
import Dreamnet.World
import Dreamnet.Conversation

import Dreamnet.CoordVector
import Dreamnet.TileData        (ttype, readBoolProperty, readWordProperty,
                                 readStringProperty)
import Dreamnet.TileMap
import Dreamnet.WorldMap
import Dreamnet.Entity
import Dreamnet.ScrollWindow
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.Visibility
import Dreamnet.Character
import Dreamnet.ObjectMonad

--------------------------------------------------------------------------------

type Name = String

--------------------------------------------------------------------------------

data Game = Game {
      _g_world        ∷ World Visibility
    , _g_gameState    ∷ GameState
    , _g_keepRunning  ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
    , _g_conversant   ∷ Maybe (Character Item ConversationNode)
    , _g_conversation ∷ ConversationNode
    , _g_scrollWindow ∷ ScrollData
    , _g_choiceWindow ∷ ChoiceData

    -- TODO take this out, eventually
    , _g_carlasComputer ∷ ComputerM ()
    , _g_carlasFramebuffer ∷ String

    , _g_watchTime ∷ Int
    , _g_watchButton ∷ Int
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← initRenderer
    m   ← loadTileMap (view dd_startingMap dd)
    sw  ← createScrollData
    cw  ← createChoiceData
    pure Game {
        _g_world        = newWorld
                              (fromTileMap m objectFromTile)
                              (playerPerson <$> [ "Carla"
                                                , "Raj"
                                                , "Delgado"
                                                ])
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversant         = Nothing
      , _g_conversation       = End
      , _g_scrollWindow       = sw
      , _g_choiceWindow       = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."

      , _g_watchTime   = 1234
      , _g_watchButton = 0
    }
    where -- TODO Set materials!
        objectFromTile t@(ttype → "Base")     = Object (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t) 0                         "<base>" M.empty
        objectFromTile t@(ttype → "Door")     = Object (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) 4                         ("Just a common door. They're " <> bool "closed." "opened." (1 `readBoolProperty` t)) M.empty
        objectFromTile t@(ttype → "Stairs")   = Object (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    1                         ("If map changing would've been coded in, you would use these to go " <> bool "down." "up." (1 `readBoolProperty` t)) M.empty
        objectFromTile t@(ttype → "Prop")     = Object (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) (4 `readWordProperty` t)  ("A " <> (1 `readStringProperty` t) <> ".") M.empty
        objectFromTile t@(ttype → "Person")   = Object  '@'            "blue"                      False                    True                    3                         ("Its " <> (1 `readStringProperty` t) <> ".") (M.fromList [ ("name", 1 `readStringProperty` t), ("alliance", 2 `readStringProperty` t)])
        objectFromTile   (ttype → "Spawn")    = Object  '.'            "concrete"                  True                     True                    0                         "Spawn point. You really should not be able to examine this?" M.empty -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
        objectFromTile t@(ttype → "Camera")   = Object (view t_char t) "green light"               True                     True                    1                         "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care." (M.fromList [ ("level", "0"), ("alliance", 1 `readStringProperty` t)])
        objectFromTile t@(ttype → "Computer") = Object (view t_char t) "metal"                     False                    True                    1                         "Your machine. You wonder if Devin mailed you about the job." M.empty
        objectFromTile t@(ttype → "Item")     = Object (view t_char t) "blue plastic"              True                     True                    0                         ("A " <> (1 `readStringProperty` t) <> ".") M.empty
        objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???
        playerPerson  n = Object '@' "metal" False True 3 ("Its " <> n <> ".") (M.fromList [("name", n), ("alliance", "player")])

--------------------------------------------------------------------------------

launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

-- TODO This'll die when renderer gets refactored a bit
doRender ∷ RendererF () → StateT Game C.Curses ()
doRender r = use g_rendererData >>=
    lift . (`runRenderer` r) >>=
    assign g_rendererData . snd
        
--------------------------------------------------------------------------------

dreamnet ∷ DesignData → IO ()
dreamnet dd = C.runCurses $ do
    -- Init curses
    C.setRaw     True
    C.setEcho    False
    C.defaultWindow >>= (`C.setKeypad` True)
    void $ C.setCursorMode C.CursorInvisible
    g ← newGame dd
    void $ flip execStateT g $ do
        g_world %= snd . runWorld (updateVisible $> Normal)
        renderNormal
        loopTheLoop dd


loopTheLoop ∷ DesignData → StateT Game C.Curses ()
loopTheLoop dd = do
    r ← use g_keepRunning
    when r $ do
        use g_gameState >>= \case
            Normal       → lift Input.nextWorldEvent       >>= processNormal
            Examination  → lift Input.nextUiEvent          >>= processExamination
            Operation    → lift Input.nextInteractionEvent >>= processOperation
            Hud          → lift Input.nextHudEvent         >>= processHud
            Conversation → lift Input.nextUiEvent          >>= processConversation
            InventoryUI  → lift Input.nextUiEvent          >>= processInventoryUI
            CharacterUI  → lift Input.nextUiEvent          >>= processCharacterUI
        lift C.render
        loopTheLoop dd
    where
        processNormal ∷ Input.WorldEvent → StateT Game C.Curses ()
        processNormal Input.Quit = do
            g_keepRunning .= False
        processNormal Input.SwitchToHud = do
            g_gameState .= Hud
            renderNormal
        processNormal (Input.Move v) = do
            g_world %= snd . runWorld (moveSelected v >> updateVisible $> Normal)
            renderNormal
        processNormal Input.Wait = do
            g_world %= snd . runWorld (setStatus "Waiting..." >> updateVisible $> Normal)
            --runProgram v (operationProgramForSymbol (view o_symbol o) $ AiTick)
            renderNormal
        processNormal (Input.SelectTeamMember 0) = do
            g_world %= snd . runWorld (selectCharacter "Carla" $> Normal)
            uses (g_world.w_active.e_object.o_state) (M.! "name") >>= \n → g_world.w_status .= n <> " selected."
            renderNormal
        processNormal (Input.SelectTeamMember 1) = do
            g_world %= snd . runWorld (selectCharacter "Raj" $> Normal)
            uses (g_world.w_active.e_object.o_state) (M.! "name") >>= \n → g_world.w_status .= n <> " selected."
            renderNormal
        processNormal (Input.SelectTeamMember 2) = do
            g_world %= snd . runWorld (selectCharacter "Delgado" $> Normal)
            uses (g_world.w_active.e_object.o_state) (M.! "name") >>= \n → g_world.w_status .= n <> " selected."
            renderNormal
        processNormal (Input.SelectTeamMember _) =
            pure ()
        processNormal Input.Get = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "There's nothing here."
                Just (v, o) →
                    renderMessage $ "Picking up " <> show o <> " from " <> show v
        processNormal Input.UseHeld = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "Nothing there."
                Just (_, _) →
                    renderMessage "Need to figure out how to run programs for held Objects."
                    --programAt v >>= maybe (pure ()) (\prg → runProgram v (prg OperateOn))
                    --renderNormal
        processNormal Input.Examine = do
            examineText ← obtainTarget >>= \case
                Just t  → pure $ view o_description (snd t)
                Nothing → uses (g_world.w_map) desc
            g_scrollWindow %= moveWindow (V2 2 1)
            g_scrollWindow %= resizeWindow (V2 40 10)
            g_scrollWindow %= setText examineText
            use g_scrollWindow >>= lift . renderScrollWindow
            g_gameState .= Examination
        processNormal Input.Operate = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "There's nothing here."
                Just (v, o) → do
                    runProgram v (operationProgramForSymbol (view o_symbol o) $ Operate)
                    use g_gameState >>= \case
                        Normal →
                            renderNormal
                        _      → do
                            renderMessage "Need to implement rendering of other states!"
                            g_gameState .= Normal
        processNormal Input.Talk = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "Trying to talk to someone, but there's no one there."
                Just (v, o) → do
                    runProgram v (operationProgramForSymbol (view o_symbol o) $ Talk)
                    use g_gameState >>= \case
                        Conversation → do
                            let ch = characterForName dd . (M.! "name") . view o_state $ o
                            g_conversant .= Just ch
                            g_conversation .= view ch_conversation ch
                            g_scrollWindow %= setTitle (view ch_name ch)
                            use g_conversation >>= \case
                                (ChoiceNode opts _) → g_choiceWindow %= setOptions opts
                                (TalkNode s _)      → g_scrollWindow %= setText s
                                (ListenNode s _)    → g_scrollWindow %= setText s
                                _ → pure ()
                            use g_conversation >>= renderConversation
                        _ →
                            renderNormal
        processNormal Input.InventorySheet = do
            is ← uses (g_world.w_active.e_object.o_state) (fmap show . equippedContainers . characterForName dd . (M.! "name"))
            g_scrollWindow %= setTitle "Inventory sheet"
            g_scrollWindow %= setLines is
            use g_scrollWindow >>= lift . renderScrollWindow
            g_gameState .= InventoryUI
        processNormal Input.CharacterSheet = do
            g_scrollWindow %= setTitle "Character sheet"
            use g_scrollWindow >>= lift .  renderScrollWindow
            g_gameState .= CharacterUI

        processExamination ∷ Input.UIEvent → StateT Game C.Curses ()
        processExamination Input.MoveUp = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processExamination Input.MoveDown = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processExamination _ = do
            use g_scrollWindow >>= lift . clearScrollWindow
            g_gameState .= Normal
            renderNormal

        processHud ∷ Input.HUDEvent → StateT Game C.Curses ()
        processHud Input.SwitchToNormal = do
            g_gameState .= Normal
            renderNormal
        processHud Input.WatchUp = do
            g_watchTime += 1
            renderNormal
        processHud Input.WatchDown = do
            g_watchTime -= 1
            renderNormal
        processHud Input.WatchNextButton = do
            g_watchButton %= (`mod` 3) . (+1)
            renderNormal
        processHud Input.WatchPrevButton = do
            g_watchButton %= (`mod` 3) . subtract 1
            renderNormal
        processHud Input.WatchButtonPush = do
            use g_watchButton >>= renderMessage . ("Pushing button: " <>) . show
            renderNormal


        processConversation ∷ Input.UIEvent → StateT Game C.Curses ()
        processConversation Input.MoveUp = do
            use g_conversation >>= \case
                (ChoiceNode _ _) → g_choiceWindow %= selectPrevious
                _                → g_scrollWindow %= scrollUp
            use g_conversation >>= renderConversation
        processConversation Input.MoveDown = do
            use g_conversation >>= \case
                (ChoiceNode _ _) → g_choiceWindow %= selectNext
                _                → g_scrollWindow %= scrollDown
            use g_conversation >>= renderConversation
        processConversation Input.SelectChoice = use g_conversation >>= \case
            n@(ChoiceNode _ _) → do
                use g_choiceWindow >>= assign g_conversation . pick n . commit
                use g_conversation >>= \case
                    (ChoiceNode opts _) → do
                        g_choiceWindow %= setOptions opts
                        use g_conversation >>= renderConversation
                    (TalkNode s _) → do
                        initName ← uses (g_world.w_active.e_object.o_state) (M.! "name")
                        g_scrollWindow %= setTitle initName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    (ListenNode s _) → do
                        otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                        g_scrollWindow %= setTitle otherName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    End → do
                        g_conversant .= Nothing
                        g_gameState .= Normal
                        use g_scrollWindow >>= lift . clearScrollWindow
                        renderNormal
            (TalkNode _ _) → do
                g_conversation %= advance
                use g_conversation >>= \case
                    (ChoiceNode opts _) → do
                        g_choiceWindow %= setOptions opts
                        use g_conversation >>= renderConversation
                    (TalkNode s _) → do
                        initName ← uses (g_world.w_active.e_object.o_state) (M.! "name")
                        g_scrollWindow %= setTitle initName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    (ListenNode s _) → do
                        otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                        g_scrollWindow %= setTitle otherName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    End → do
                        g_conversant .= Nothing
                        g_gameState .= Normal
                        use g_scrollWindow >>= lift . clearScrollWindow
                        renderNormal
            (ListenNode _ _) → do
                g_conversation %= advance
                use g_conversation >>= \case
                    (ChoiceNode opts _) → do
                        g_choiceWindow %= setOptions opts
                        use g_conversation >>= renderConversation
                    (TalkNode s _) → do
                        initName ← uses (g_world.w_active.e_object.o_state) (M.! "name")
                        g_scrollWindow %= setTitle initName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    (ListenNode s _) → do
                        otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                        g_scrollWindow %= setTitle otherName
                        g_scrollWindow %= setText s
                        use g_conversation >>= renderConversation
                    End → do
                        g_conversant .= Nothing
                        g_gameState .= Normal
                        use g_scrollWindow >>= lift . clearScrollWindow
                        renderNormal
            End → do  -- TODO we shall never end up here, but this is because there's an event listening stuck at the beginning of conversation state management. This needs to change.
                g_conversant .= Nothing
                g_gameState .= Normal
                use g_scrollWindow >>= lift . clearScrollWindow
                renderNormal
        processConversation Input.Back =
            pure ()

        processInventoryUI ∷ Input.UIEvent → StateT Game C.Curses ()
        processInventoryUI Input.MoveUp = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processInventoryUI Input.MoveDown = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processInventoryUI _ = do
            use g_scrollWindow >>= lift . clearScrollWindow
            g_gameState .= Normal
            renderNormal

        processCharacterUI ∷ Input.UIEvent → StateT Game C.Curses ()
        processCharacterUI Input.MoveUp = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processCharacterUI Input.MoveDown = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processCharacterUI _ = do
            use g_scrollWindow >>= lift . clearScrollWindow
            g_gameState .= Normal
            renderNormal

        processOperation ∷ Input.InteractionEvent → StateT Game C.Curses ()
        processOperation (Input.PassThrough c) = do
            updateComputer c
            qr ← uses g_carlasComputer (view cd_requestedQuit . computerData)
            if qr
              then g_gameState .= Normal
              else pure ()
              --else do
              --     comp ← use g_carlasComputer
              --     fbr  ← use g_carlasFramebuffer
                   --doRender (renderComputer fbr (computerData comp))


runProgram ∷ V2 Int → Free ObjectF () → StateT Game C.Curses ()
runProgram v prg = do
    o ← uses (g_world.w_map) (last . valuesAt v)
    (gs, w') ← uses g_world (runWorld (snd <$> runObjectMonadWorld prg v o >>= \gs → updateVisible *> pure gs)) -- TODO run program should probably return useful values for interactions???
    g_gameState .= gs
    g_world .= w'


-- TODO this is pretty bad, because programs can change symbols! Find a better way!
operationProgramForSymbol ∷ Char → InteractionType → Free ObjectF ()
operationProgramForSymbol '+'  = door
operationProgramForSymbol '\'' = door
operationProgramForSymbol '&'  = computer
operationProgramForSymbol '@'  = person
operationProgramForSymbol '*'  = camera
operationProgramForSymbol _    = generic


characterForName ∷ DesignData → String → Character Item ConversationNode
characterForName dd name =
    let maybeChar = M.lookup name (view dd_characters dd)
    in  (fromMaybe (view dd_defaultRedshirt dd) maybeChar)


--pickAChoice ∷ GameState → Input.UIEvent → StateT Game C.Curses Word
--pickAChoice gs Input.MoveUp = do
--    g_choiceWindow %= selectPrevious
--    use g_choiceWindow >>= lift . drawChoiceWindow
--    lift (Input.nextEvent gs) >>= pickAChoice gs
--pickAChoice gs Input.MoveDown = do
--    g_choiceWindow %= selectNext
--    use g_choiceWindow >>= lift . drawChoiceWindow
--    lift (Input.nextEvent gs) >>= pickAChoice gs
--pickAChoice _ Input.SelectChoice =
--    uses g_choiceWindow commit
--pickAChoice gs _ = do
--    lift (Input.nextEvent gs) >>= pickAChoice gs

-- TODO reuse code for aiming weapons
--switchAim ∷ Maybe (Object → Bool) → StateT Game C.Curses ()
--switchAim (Just nof) = do
--    pp ← use  (g_world.w_active.e_position)
--    os ← uses (g_world.w_map) (interestingObjects pp 2 nof)
--    ca ← use  g_aim
--    case ca of
--        Just a → g_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
--        _      → g_aim .= headMay os
--switchAim Nothing = g_aim .= Nothing


-- TODO reuse code for aiming weapons
--allButTheBase ∷ Object → Bool
--allButTheBase o
--    | view o_symbol o == '.' = False
--    | otherwise              = True


obtainTarget ∷ StateT Game C.Curses (Maybe (V2 Int, Object))
obtainTarget = do
    renderMessage "Select direction:"
    lift C.render

    ap ← use (g_world.w_active.e_position)
    t  ← lift Input.nextTargetSelectionEvent
    case t of
        (V2 0 0) → pure Nothing
        v →
            uses (g_world.w_map) (filter coolSymbols . valuesAt (ap + v)) >>=
                \case
                    [] → pure Nothing
                    l  → pure (Just (ap + v, last l))  -- TODO find a way to deal with noninteresting objects (<base>)
    where
        coolSymbols o = or $ fmap (view o_symbol o ==) [ '@', '*', '+', '\'' ] -- TODO BAAAD, but its going to be refactored when I figure out better way

--------------------------------------------------------------------------------

updateComputer ∷ Char → StateT Game C.Curses ()
updateComputer '\n' = do
    uses g_carlasComputer (*> commitInput) >>= assign g_carlasFramebuffer . computerOutput
    g_carlasComputer %= (\l → l *> commitInput $> ())
updateComputer '\b' = g_carlasComputer %= (*> backspace)
updateComputer c    = g_carlasComputer %= (*> input c)


--withAimOrElse ∷ (V2 Int → [Object] → StateT Game C.Curses a) → StateT Game C.Curses a → StateT Game C.Curses a
--withAimOrElse f e = fromMaybe e <=< runMaybeT $ do
--    v  ← MaybeT (use g_aim)
--    os ← uses (g_world.w_map) (valuesAt v)
--    pure (f v os)
--
--
--withAim ∷ (V2 Int → [Object] → StateT Game C.Curses ()) → StateT Game C.Curses ()
--withAim f = withAimOrElse f (pure ())

--------------------------------------------------------------------------------

renderNormal ∷ StateT Game C.Curses ()
renderNormal = do
    w  ← uses (g_world.w_map) width
    d  ← uses (g_world.w_map.wm_data) (fmap last)
    v  ← use (g_world.w_vis)
    s  ← use (g_world.w_status)
    hudmode ← uses g_gameState (==Hud) 
    wt  ← use g_watchTime
    wb  ← use g_watchButton
    
    doRender $ do
        drawMap (view o_symbol) (view o_material) w d v >>= updateMain
        drawHud hudmode wt wb s >>= updateHud


renderMessage ∷ String → StateT Game C.Curses ()
renderMessage msg = do
    hudmode ← uses g_gameState (==Hud) 
    wt  ← use g_watchTime
    wb  ← use g_watchButton
    doRender $ drawHud hudmode wt wb msg >>= updateHud


renderConversation ∷ ConversationNode → StateT Game C.Curses ()
renderConversation (ChoiceNode _ _) =
    use g_choiceWindow >>= lift . drawChoiceWindow
renderConversation (TalkNode _ _) = do
    use g_scrollWindow >>= lift . clearScrollWindow
    lift (positionFor 0) >>= (\p → g_scrollWindow %= moveWindow p)
    lift conversationSize >>= (\s → g_scrollWindow %= resizeWindow s)
    use g_scrollWindow >>= lift . renderScrollWindow
renderConversation (ListenNode _ _) = do
    use g_scrollWindow >>= lift . clearScrollWindow
    lift (positionFor 1) >>= (\p → g_scrollWindow %= moveWindow p)
    lift conversationSize >>= (\s → g_scrollWindow %= resizeWindow s)
    use g_scrollWindow >>= lift . renderScrollWindow
renderConversation End =
    pure ()

