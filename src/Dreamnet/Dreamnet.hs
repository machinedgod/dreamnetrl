{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Safe                      (succSafe, predSafe, at)
import Control.Lens              (makeLenses, use, uses, view, views, (.=),
                                  assign, (%=), (+=), (-=), (%~))
import Control.Monad             (void, when)
import Control.Monad.Free        (Free)
import Control.Monad.State       (StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.Functor              (($>))
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2))

import qualified UI.NCurses  as C
import qualified Data.Vector as V    (fromList)
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import qualified Dreamnet.Input as Input
import Dreamnet.World
import Dreamnet.Conversation
import Dreamnet.TileMap
import Dreamnet.TileData
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Entity
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.Visibility
import Dreamnet.Character
import Dreamnet.ObjectMonad

import Design.DesignAPI
import Design.ObjectPrograms
import Design.GameCharacters

--------------------------------------------------------------------------------

type Name = String

--------------------------------------------------------------------------------
--setText ∷ String → ScrollData → ScrollData
--setText s sd =
--    let ls = intercalate [""] $ lines' (fromIntegral $ lineWidth sd) length " " . words <$> lines s
--    in  setLines ls sd
--
--
--setLines ∷ [String] → ScrollData → ScrollData
--setLines ls sd = (sd_startLine .~ 0) . (sd_lines .~ fmap extendToWidth ls) $ sd
--    where
--        extendToWidth s = s <> replicate (fromIntegral (lineWidth sd) - length s) ' '


--------------------------------------------------------------------------------

-- TODO apply DeGoes principle here: extract everything with a neat api, that
-- then runs and produces WorldAPI state values
data Game = Game {
      _g_turn         ∷ Word
    , _g_world        ∷ World States Visibility -- TODO could "States" here be parametric?
    , _g_gameState    ∷ GameState
    , _g_keepRunning  ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
    , _g_conversant   ∷ Maybe DreamnetCharacter
    , _g_conversation ∷ ConversationNode
    , _g_choiceWindow ∷ ChoiceData

    -- TODO take this out, eventually
    , _g_carlasComputer ∷ ComputerM ()
    , _g_carlasFramebuffer ∷ String

    , _g_hudTeamSelector ∷ Int
    , _g_watchAlarmTime ∷ Int
    , _g_watchButton ∷ Int
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← initRenderer
    cw  ← createChoiceData
    pure Game {
        _g_turn  = 0
      , _g_world = newWorld
                       (fromTileMap (view dd_startingMap dd) objectFromTile)
                       (playerPerson . (`characterForName` view dd_characters dd) <$>
                            [ "Carla"
                            , "Devin"
                            , "Hideo"
                            , "Phillipe"
                            , "Qaayenaat"
                            , "Annabelle"
                            ])
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversant   = Nothing
      , _g_conversation = End
      , _g_choiceWindow = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."

      , _g_hudTeamSelector = 0
      , _g_watchAlarmTime  = 1234
      , _g_watchButton     = 0
    }
    where
        -- 1) This *could* all be just a single thing. Object type really does not matter here.
        -- 2) Actually, it does, because Object carries a specific state, later used by object programs
        objectFromTile ∷ Tile → Object States
        objectFromTile t@(ttype → "Base") =
            let m  = "concrete"
                p  = 1 `readBoolProperty` t
                s  = 2 `readBoolProperty` t
                h  = 0
                st = Empty
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Door") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = 1 `readBoolProperty` t
                h  = 5
                st = Prop "Door"
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Stairs") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = True
                h  = 1
                st = Prop "Stairs"
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Prop") =
            let m  = 4 `readStringProperty` t
                p  = 2 `readBoolProperty` t
                s  = 3 `readBoolProperty` t
                h  = 5 `readWordProperty` t
                st = Prop (1 `readStringProperty` t)
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Person") = 
            let m  = "blue"
                p  = False
                s  = True
                h  = 3
                st = Person $ characterForName (1 `readStringProperty` t) (view dd_characters dd)
            in  Object '@' m p s h st
        objectFromTile (ttype → "Spawn") = -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
            objectFromTile (Tile '.' (V.fromList [ "Base", "True", "True" ]))
        objectFromTile t@(ttype → "Camera") =
            let m  = "green light"
                p  = True
                s  = True
                h  = 1
                st = Camera (Faction $ 1 `readStringProperty` t) 0
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Computer") =
            let m  = "metal"
                p  = False
                s  = True
                h  = 1
                st = Empty
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Item") = 
            let m  = "blue plastic"
                p  = True
                s  = True
                h  = 0
                st = Prop (1 `readStringProperty` t)
            in  Object (view t_char t) m p s h st
        objectFromTile t =
            error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object '@' "metal" False True 3 . Person

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
        g_world.w_status .= ""
        use g_gameState >>= \case
            Normal       → lift Input.nextWorldEvent       >>= processNormal dd
            Examination  → lift Input.nextUiEvent          >>= processExamination
            Operation    → lift Input.nextInteractionEvent >>= processOperation
            HudTeam      → lift Input.nextUiEvent          >>= processHudTeam dd
            HudMessages  → lift Input.nextUiEvent          >>= processHudMessages
            HudWatch     → lift Input.nextUiEvent          >>= processHudWatch
            Conversation → lift Input.nextUiEvent          >>= processConversation
            InventoryUI  → lift Input.nextUiEvent          >>= processInventoryUI
            CharacterUI  → lift Input.nextUiEvent          >>= processCharacterUI
        lift C.render
        loopTheLoop dd


processNormal ∷ DesignData → Input.WorldEvent → StateT Game C.Curses ()
processNormal _ Input.Quit = do
    g_keepRunning .= False
processNormal _ Input.SwitchToHud = do
    g_gameState .= HudTeam
    renderNormal
processNormal _ (Input.Move v) = do
    g_world %= snd . runWorld (moveActive v >> updateVisible)
    increaseTurn
    renderNormal
processNormal _ Input.Wait = do
    g_world %= snd . runWorld (setStatus "Waiting..." >> updateVisible)
    --runProgram v (operationProgramForSymbol (view o_symbol o) $ AiTick)
    increaseTurn
    renderNormal
processNormal _ Input.HigherStance = do
    g_world %= snd . runWorld
        (changeActive (o_state %~ (\(Person ch) → Person (ch_stance %~ predSafe $ ch))))
    renderNormal
processNormal _ Input.LowerStance = do
    g_world %= snd . runWorld
        (changeActive (o_state %~ (\(Person ch) → Person (ch_stance %~ succSafe $ ch))))
    renderNormal
processNormal _ Input.Get = do
    obtainTarget >>= \case
        Nothing → do
            increaseTurn
            renderMessage "There's nothing here."
        Just (v, o) → do
            -- Copy to characters arm
            -- Remove from the world
            g_world %= snd . runWorld
                (changeActive (o_state %~ (\(Person ch) → Person $ pickUp o ch)) >>
                 deleteObject v o
                )
            increaseTurn
            renderNormal
processNormal _ Input.UseHeld = do
    obtainTarget >>= \case
        Nothing → do
            renderMessage "Nothing there."
            increaseTurn
        Just (_, _) → do
            renderMessage "Need to figure out how to run programs for held Objects."
            --programAt v >>= maybe (pure ()) (\prg → runProgram v (prg OperateOn))
            --renderNormal
            increaseTurn -- TODO as much as the device wants!
processNormal dd Input.Examine = do
    obtainTarget >>= \case
        Just (v, o)  → do
            onHerself ← uses (g_world.w_active.e_position) (==v)
            if onHerself
                then do
                    examineText ← uses (g_world.w_map) desc
                    g_rendererData.rd_scrollData .= newScrollData (V2 2 1) (V2 60 20) Nothing examineText
                    g_gameState .= Examination
                    renderInformation
                else do
                    runProgram dd v (programForObject o Examine)
                    use g_gameState >>= \case
                        Examination → do
                            use (g_world.w_status) >>= assign (g_rendererData.rd_scrollData) . newScrollData (V2 2 1) (V2 60 20) Nothing
                            g_world.w_status .= ""
                            renderInformation
                        _ → renderNormal
        Nothing → do
            renderMessage "There's nothing there."
            renderNormal
    increaseTurn
processNormal dd Input.Operate = do
    obtainTarget >>= \case
        Nothing → do
            renderMessage "There's nothing here."
            increaseTurn
        Just (v, o) → do
            runProgram dd v (programForObject o Operate)
            use g_gameState >>= \case
                Normal → do
                    renderNormal
                _      → do
                    renderMessage "Need to implement rendering of other states!"
                    g_gameState .= Normal
            increaseTurn -- TODO as much as operation program wants!
processNormal dd Input.Talk = do
    obtainTarget >>= \case
        Nothing → do
            renderMessage "Trying to talk to someone, but there's no one there."
            increaseTurn
        Just (v, o) → do
            runProgram dd v (programForObject o Talk)
            increaseTurn
            use g_gameState >>= \case
                Conversation → do
                    let ch = views o_state (\(Person ch') → ch') $ o
                    g_conversant .= Just ch
                    g_conversation .= view ch_conversation ch

                    use g_conversation >>= \case
                        (ChoiceNode opts _) → g_choiceWindow %= setOptions opts
                        (TalkNode s _) → do
                            pos ← lift (positionFor 0)
                            siz ← lift conversationSize
                            g_rendererData.rd_scrollData .= newScrollData pos siz (Just (view ch_name ch)) s
                        (ListenNode s _) → do
                            pos ← lift (positionFor 1)
                            siz ← lift conversationSize
                            g_rendererData.rd_scrollData .= newScrollData pos siz (Just (view ch_name ch)) s
                        _ → pure ()
                    use g_conversation >>= renderConversation
                _ →
                    renderNormal
processNormal _ Input.InventorySheet = do
    g_rendererData.rd_scrollData .= newScrollData (V2 1 1) (V2 60 30) (Just "Inventory sheet") ""
    g_gameState .= InventoryUI
    renderInformation
processNormal dd Input.CharacterSheet = do
    g_gameState .= CharacterUI
    doRender $ drawCharacterSheet (characterForName "Carla" (view dd_characters dd)) >>= updateUi


processExamination ∷ Input.UIEvent → StateT Game C.Curses ()
processExamination Input.MoveUp = do
    g_rendererData.rd_scrollData %= scrollUp
    renderInformation
processExamination Input.MoveDown = do
    g_rendererData.rd_scrollData %= scrollDown
    renderInformation
processExamination _ = do
    doRender $ updateUi clear
    g_gameState .= Normal
    renderNormal


processHudTeam ∷ DesignData → Input.UIEvent → StateT Game C.Curses ()
processHudTeam _ Input.TabNext = do
    g_gameState .= HudMessages
    renderNormal
processHudTeam _ Input.TabPrevious = do
    g_gameState .= HudWatch
    renderNormal
processHudTeam _ Input.MoveUp = do
    g_hudTeamSelector %= max 0 . subtract 3
    renderNormal
processHudTeam _ Input.MoveDown = do
    l ← uses (g_world.w_team) (fromIntegral . length)
    g_hudTeamSelector %= min l . (+3)
    renderNormal
processHudTeam _ Input.MoveLeft = do
    g_hudTeamSelector %= max 0 . subtract 1
    renderNormal
processHudTeam _ Input.MoveRight = do
    l ← uses (g_world.w_team) (fromIntegral . length)
    g_hudTeamSelector %= min l . (+1)
    renderNormal
processHudTeam dd Input.SelectChoice = do
    g_gameState .= CharacterUI
    tm ← use (g_world.w_active) >>= \cal → uses (g_world.w_team) (cal:)
    ech ← uses g_hudTeamSelector (at tm)
    doRender $ drawEquipmentDoll (views (e_object.o_state) (\(Person ch) → ch) ech) >>= updateUi
    --doRender $ drawCharacterSheet (views (e_object.o_state) (\(Person ch) → ch) ech) >>= updateUi
processHudTeam _ Input.Back = do
    g_gameState .= Normal
    renderNormal


processHudMessages ∷ Input.UIEvent → StateT Game C.Curses ()
processHudMessages Input.TabNext = do
    g_gameState .= HudWatch
    renderNormal
processHudMessages Input.TabPrevious = do
    g_gameState .= HudTeam
    renderNormal
processHudMessages Input.MoveUp = do
    -- TODO scroll
    renderNormal
processHudMessages Input.MoveDown = do
    -- TODO scroll
    renderNormal
processHudMessages Input.SelectChoice = do
    -- TODO use scroll window to show log
    renderNormal
processHudMessages Input.Back = do
    g_gameState .= Normal
    renderNormal
processHudMessages _ =
    pure ()


processHudWatch ∷ Input.UIEvent → StateT Game C.Curses ()
processHudWatch Input.TabNext = do
    g_gameState .= HudTeam
    renderNormal
processHudWatch Input.TabPrevious = do
    g_gameState .= HudMessages
    renderNormal
processHudWatch Input.MoveUp = do
    g_watchAlarmTime += 1
    renderNormal
processHudWatch Input.MoveDown = do
    g_watchAlarmTime -= 1
    renderNormal
processHudWatch Input.MoveLeft = do
    g_watchButton %= (`mod` 3) . subtract 1
    renderNormal
processHudWatch Input.MoveRight = do
    g_watchButton %= (`mod` 3) . (+1)
    renderNormal
processHudWatch Input.SelectChoice = do
    use g_watchButton >>= renderMessage . ("Pushing button: " <>) . show
    renderNormal
processHudWatch Input.Back = do
    g_gameState .= Normal
    renderNormal


processConversation ∷ Input.UIEvent → StateT Game C.Curses ()
processConversation Input.MoveUp = do
    use g_conversation >>= \case
        (ChoiceNode _ _) → g_choiceWindow %= selectPrevious
        _                → g_rendererData.rd_scrollData %= scrollUp
    use g_conversation >>= renderConversation
processConversation Input.MoveDown = do
    use g_conversation >>= \case
        (ChoiceNode _ _) → g_choiceWindow %= selectNext
        _                → g_rendererData.rd_scrollData %= scrollDown
    use g_conversation >>= renderConversation
processConversation Input.SelectChoice = use g_conversation >>= \case
    n@(ChoiceNode _ _) → do
        use g_choiceWindow >>= assign g_conversation . pick n . commit
        use g_conversation >>= \case
            (ChoiceNode opts _) → do
                g_choiceWindow %= setOptions opts
                use g_conversation >>= renderConversation
            (TalkNode s _) → do
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                pos ← lift (positionFor 0)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just initName) s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                pos ← lift (positionFor 1)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just otherName) s
                use g_conversation >>= renderConversation
            End → do
                g_conversant .= Nothing
                doRender $ updateUi clear
                g_gameState .= Normal
                renderNormal
    (TalkNode _ _) → do
        g_conversation %= advance
        use g_conversation >>= \case
            (ChoiceNode opts _) → do
                g_choiceWindow %= setOptions opts
                use g_conversation >>= renderConversation
            (TalkNode s _) → do
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                pos ← lift (positionFor 0)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just initName) s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                pos ← lift (positionFor 1)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just otherName) s
                use g_conversation >>= renderConversation
            End → do
                g_conversant .= Nothing
                doRender $ updateUi clear
                g_gameState .= Normal
                renderNormal
    (ListenNode _ _) → do
        g_conversation %= advance
        use g_conversation >>= \case
            (ChoiceNode opts _) → do
                g_choiceWindow %= setOptions opts
                use g_conversation >>= renderConversation
            (TalkNode s _) → do
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                pos ← lift (positionFor 0)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just initName) s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                pos ← lift (positionFor 1)
                siz ← lift conversationSize
                g_rendererData.rd_scrollData .= newScrollData pos siz (Just otherName) s
                use g_conversation >>= renderConversation
            End → do
                g_conversant .= Nothing
                doRender $ updateUi clear
                g_gameState .= Normal
                renderNormal
    End → do  -- TODO we shall never end up here, but this is because there's an event listening stuck at the beginning of conversation state management. This needs to change.
        g_conversant .= Nothing
        doRender $ updateUi clear
        g_gameState .= Normal
        renderNormal
processConversation _ =
    pure ()


processInventoryUI ∷ Input.UIEvent → StateT Game C.Curses ()
processInventoryUI Input.MoveUp = do
    g_rendererData.rd_scrollData %= scrollUp
    renderInformation
processInventoryUI Input.MoveDown = do
    g_rendererData.rd_scrollData %= scrollDown
    renderInformation
processInventoryUI _ = do
    doRender $ updateUi clear
    g_gameState .= Normal
    renderNormal


processCharacterUI ∷ Input.UIEvent → StateT Game C.Curses ()
processCharacterUI Input.Back = do
    doRender $ updateUi clear
    g_gameState .= Normal
    renderNormal
processCharacterUI _ =
    pure ()


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

--------------------------------------------------------------------------------

increaseTurn ∷ StateT Game C.Curses ()
increaseTurn = g_turn += 1


runProgram ∷ DesignData → V2 Int → Free ObjectF () → StateT Game C.Curses ()
runProgram dd v prg = do
    o ← uses (g_world.w_map) (last . valuesAt v)
    (gs, w') ← uses g_world (runWorld (snd <$> runObjectMonadWorld dd prg v o >>= \gs → updateVisible *> pure gs)) -- TODO run program should probably return useful values for interactions???
    g_gameState .= gs
    g_world .= w'


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


obtainTarget ∷ StateT Game C.Curses (Maybe (V2 Int, Object States))
obtainTarget = do
    renderMessage "Select direction (h/j/k/l/y/u/b/n/.):"
    lift C.render

    ap ← use (g_world.w_active.e_position)
    t  ← lift Input.nextTargetSelectionEvent
    uses (g_world.w_map) (valuesAt (ap + t)) >>=
    --uses (g_world.w_map) (filter (not . base) . valuesAt (ap + t)) >>=
        \case
            []  → error "Obtaining target on non-existent tile :-O"
            --[_] → pure Nothing
            l   → pure (Just (ap + t, last l))
    --where
    --    base o = or $ fmap (view o_symbol o ==) [ '@', '*', '+', '\'' ]

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

    at ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch) >>= pure . \case
            "Carla"   → 0
            "Delgado" → 1
            "Raj"     → 3
            "570rm"   → 4
            _         → error "Impossible active selection!"
    ts ← use g_hudTeamSelector
    gs ← use g_gameState
    t  ← use g_turn
    --wat ← use g_watchAlarmTime
    wb ← use g_watchButton
    tm ← use (g_world.w_team) >>= \t → uses (g_world.w_active) (fmap (views (e_object.o_state)  (\(Person ch) → ch)) . (:t))
    
    doRender $ do
        drawMap (view o_symbol) (view o_material) w d v >>= updateMain
        drawHud gs ts at tm t wb >>= updateHud
        drawStatus gs s >>= updateHud


renderInformation ∷ StateT Game C.Curses ()
renderInformation = do
    doRender $ drawInformation >>= updateUi


renderMessage ∷ String → StateT Game C.Curses ()
renderMessage msg = do
    gs ← use g_gameState
    doRender $ drawStatus gs msg >>= updateHud


renderConversation ∷ ConversationNode → StateT Game C.Curses ()
renderConversation (ChoiceNode _ _) =
    use g_choiceWindow >>= lift . drawChoiceWindow
renderConversation (TalkNode _ _) = do
    doRender $ updateUi clear
    renderInformation
renderConversation (ListenNode _ _) = do
    doRender $ updateUi clear
    renderInformation
renderConversation End =
    pure ()

