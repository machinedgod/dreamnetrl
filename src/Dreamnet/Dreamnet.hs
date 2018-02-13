{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Control.Lens              (makeLenses, use, uses, view, (.=), assign, (%=))
import Control.Monad             (void, when)
import Control.Monad.Free        (Free)
import Control.Monad.State       (StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.Functor              (($>))
import Data.Bool                 (bool)
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2))

import qualified Data.Map    as M    (lookup)
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.DesignData
import Dreamnet.GameState
import qualified Dreamnet.Input as Input
import Dreamnet.World
import Dreamnet.Conversation

import Dreamnet.CoordVector
import Dreamnet.TileData        (ttype, readBoolProperty, readStringProperty)
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
      _g_world        ∷ World Visibility (Character Item ConversationNode)
    , _g_gameState    ∷ GameState
    , _g_keepRunning  ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
    , _g_conversant   ∷ Maybe (Character Item ConversationNode)
    , _g_conversation ∷ ConversationNode
    , _g_conversationWindow ∷ C.Window
    , _g_scrollWindow ∷ ScrollData
    , _g_choiceWindow ∷ ChoiceData

    -- TODO take this out, eventually
    , _g_carlasComputer ∷ ComputerM ()
    , _g_carlasFramebuffer ∷ String
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← initRenderer
    m   ← loadTileMap (view dd_startingMap dd)
    cvw ← createConversationWindow
    sw  ← createScrollData
    cw  ← createChoiceData
    pure Game {
        _g_world        = newWorld
                              (fromTileMap m objectFromTile)
                              [ newCharacter "Carla"   End
                              , newCharacter "Raj"     End
                              , newCharacter "Delgado" End
                              ]
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversant         = Nothing
      , _g_conversation       = End
      , _g_conversationWindow = cvw
      , _g_scrollWindow       = sw
      , _g_choiceWindow       = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."
    }
    where -- TODO Set materials!
        objectFromTile t@(ttype → "Base")     = Object (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t)   "<base>"
        objectFromTile t@(ttype → "Door")     = Object (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) $ "Just a common door. They're " <> bool "closed." "opened." (1 `readBoolProperty` t)
        objectFromTile t@(ttype → "Stairs")   = Object (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    $ "If map changing would've been coded in, you would use these to go " <> bool "down." "up." (1 `readBoolProperty` t)
        objectFromTile t@(ttype → "Prop")     = Object (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) $ "A " <> (1 `readStringProperty` t) <> "."
        objectFromTile t@(ttype → "Person")   = Object  '@'            "blue"                      False                    True                    $ "Its " <> (1 `readStringProperty` t) <> "."
        objectFromTile   (ttype → "Spawn")    = Object  '.'            "concrete"                  True                     True                    $ "Spawn point. You really should not be able to examine this?" -- TODO shitty hardcoding, spawns should probably be generalized somehow!
        objectFromTile t@(ttype → "Camera")   = Object (view t_char t) "green light"               True                     True                      "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care." --                                        "A camera is frantically following your motion as you move around the room and blinking a little red LED. You pessimistically assume you must've been detected!"
        objectFromTile t@(ttype → "Computer") = Object (view t_char t) "metal"                     False                    True                      "Your machine. You wonder if Devin mailed you about the job."
        objectFromTile t@(ttype → "Item")     = Object (view t_char t) "blue plastic"              True                     True                    $ "A " <> (1 `readStringProperty` t) <> "."
        objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

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
        -- States can morph into other states
        -- We need an 'init' function to be called when state is first changed
        --
        -- If state switch would fire an event type instead of just
        -- being returned by the world, then the pipeline would still hold!
        --
        -- Problem is that update (maybe even render) components would need
        -- access to the input pipeline then, which means World has to be aware
        -- of at least certain portions of the Game.

        use g_gameState >>= \case
            Normal       → use g_gameState >>= lift . Input.nextEvent >>= processNormal
            Examination  → use g_gameState >>= lift . Input.nextEvent >>= processExamination
            Conversation → processConversation
            InventoryUI  → use g_gameState >>= lift . Input.nextEvent >>= processInventoryUI
            CharacterUI  → use g_gameState >>= lift . Input.nextEvent >>= processCharacterUI
            Operation    → use g_gameState >>= lift . Input.nextEvent >>= processOperation
        lift C.render
        loopTheLoop dd
    where
        processNormal ∷ Input.Event → StateT Game C.Curses ()
        processNormal Input.Quit = do
            g_keepRunning .= False
        processNormal (Input.WorldEv (Input.Move v)) = do
            g_world %= snd . runWorld (moveSelected v >> updateVisible $> Normal)
            renderNormal
        processNormal (Input.WorldEv Input.Wait) = do
            g_world %= snd . runWorld (setStatus "Waiting..." >> updateVisible $> Normal)
            renderNormal
        processNormal (Input.WorldEv (Input.SelectTeamMember 0)) = do
            g_world %= snd . runWorld (selectCharacter (byName "Carla") $> Normal)
            renderNormal
        processNormal (Input.WorldEv (Input.SelectTeamMember 1)) = do
            g_world %= snd . runWorld (selectCharacter (byName "Raj") $> Normal)
            renderNormal
        processNormal (Input.WorldEv (Input.SelectTeamMember 2)) = do
            g_world %= snd . runWorld (selectCharacter (byName "Delgado") $> Normal)
            renderNormal
        processNormal (Input.WorldEv Input.Get) = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "There's nothing here."
                Just (v, o) →
                    renderMessage $ "Picking up " <> show o <> " from " <> show v
        processNormal (Input.WorldEv Input.UseHeld) = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "Nothing there."
                Just (_, _) →
                    renderMessage "Need to figure out how to run programs for held Objects."
                    --programAt v >>= maybe (pure ()) (\prg → runProgram v (prg OperateOn))
                    --renderNormal
        processNormal (Input.WorldEv Input.Examine) = do
            examineText ← obtainTarget >>= \case
                Just t  → pure $ view o_description (snd t)
                Nothing → uses (g_world.w_map) desc
            g_scrollWindow %= setText examineText
            use g_scrollWindow >>= lift . renderScrollWindow
            g_gameState .= Examination
        processNormal (Input.WorldEv Input.Operate) = do
            obtainTarget >>= \case
                Nothing → do
                    renderMessage "There's nothing here."
                Just (v, o) → do
                    runProgram v (operationProgramForSymbol (view o_symbol o) $ Operate)
                    use g_gameState >>= \case
                        Normal → do
                            renderNormal
                        _      → do
                            renderMessage "Need to implement rendering of other states!"
                            g_gameState .= Normal
        processNormal (Input.WorldEv Input.Talk) = do
            obtainTarget >>= \case
                Nothing →
                    renderMessage "Trying to talk to someone, but there's no one there."
                Just (v, o) → do
                    runProgram v (operationProgramForSymbol (view o_symbol o) $ Talk)
                    use g_gameState >>= \case
                        Conversation → do
                            g_conversant .= fmap (characterForName dd) (nameForPosition v)
                            uses g_conversant (fmap (view ch_conversation)) >>= assign g_conversation . fromMaybe End
                            initName  ← use  (g_world.w_active.e_object.ch_name)
                            otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                            use g_conversation >>= \case
                                (TalkNode text _) → do
                                    use g_conversationWindow >>= lift . clearConversationWindow
                                    use g_conversationWindow >>= lift . drawConversationWindow 0 initName text
                                (ListenNode text _) → do
                                    use g_conversationWindow >>= lift . clearConversationWindow
                                    use g_conversationWindow >>= lift . drawConversationWindow 1 otherName text
                                (ChoiceNode opts _) → do
                                    g_choiceWindow %= setOptions opts
                                    use g_choiceWindow >>= lift . drawChoiceWindow
                                End → do
                                    use g_conversationWindow >>= lift . clearConversationWindow
                                    use g_conversationWindow >>= lift . drawConversationWindow 0 initName "Hello?"
                                    g_conversant .= Nothing
                                    g_gameState .= Normal
                        _ → do
                            renderNormal
        processNormal (Input.WorldEv Input.InventorySheet) = do
            is ← uses (g_world.w_active.e_object) (fmap show . equippedContainers)
            g_scrollWindow %= setLines is
            use g_scrollWindow >>= lift . renderScrollWindow
            g_gameState .= InventoryUI
        processNormal (Input.WorldEv Input.CharacterSheet) = do
            g_scrollWindow %= setText "Character sheet"
            use g_scrollWindow >>= lift .  renderScrollWindow
            g_gameState .= CharacterUI
        processNormal _ =
            pure ()  -- TODO Invalid event in invalid state. Oups!

        processExamination ∷ Input.Event → StateT Game C.Curses ()
        processExamination (Input.UIEv Input.MoveUp) = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processExamination (Input.UIEv Input.MoveDown) = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processExamination _ = do
            use g_scrollWindow >>= lift . clearScrollWindow
            g_gameState .= Normal
            renderNormal

        processConversation ∷ StateT Game C.Curses ()
        processConversation = do
            e ← use g_gameState >>= lift . Input.nextEvent
            initName  ← use  (g_world.w_active.e_object.ch_name)
            otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
            g_conversation %= advance -- Can't advance here, because maybe its going to be a choice. Maybe I need a *choice* state, aside conversation state? Or some generic UI state?
            use g_conversation >>= \case
                (TalkNode text _) → do
                    use g_conversationWindow >>= lift . clearConversationWindow
                    use g_conversationWindow >>= lift . drawConversationWindow 0 initName text
                (ListenNode text _) → do
                    use g_conversationWindow >>= lift . clearConversationWindow
                    use g_conversationWindow >>= lift . drawConversationWindow 1 otherName text
                n@(ChoiceNode opts _) → do
                    g_choiceWindow %= setOptions opts
                    pickAChoice Conversation e >>= assign g_conversation . pick n
                End → do
                    g_conversant .= Nothing
                    g_gameState .= Normal
                    use g_conversationWindow >>= lift . clearConversationWindow
                    renderNormal

        processInventoryUI ∷ Input.Event → StateT Game C.Curses ()
        processInventoryUI (Input.UIEv Input.MoveUp) = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processInventoryUI (Input.UIEv Input.MoveDown) = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processInventoryUI _ = do
            g_gameState .= Normal

        processCharacterUI ∷ Input.Event → StateT Game C.Curses ()
        processCharacterUI (Input.UIEv Input.MoveUp) = do
            g_scrollWindow %= scrollUp
            use g_scrollWindow >>= lift . renderScrollWindow
        processCharacterUI (Input.UIEv Input.MoveDown) = do
            g_scrollWindow %= scrollDown
            use g_scrollWindow >>= lift . renderScrollWindow
        processCharacterUI _ = do
            g_gameState .= Normal

        processOperation ∷ Input.Event → StateT Game C.Curses ()
        processOperation (Input.PassThrough c) = do
            updateComputer c
            qr ← uses g_carlasComputer (view cd_requestedQuit . computerData)
            if qr
              then g_gameState .= Normal
              else do
                   comp ← use g_carlasComputer
                   fbr  ← use g_carlasFramebuffer
                   doRender (renderComputer fbr (computerData comp))
        processOperation _ = do
            g_gameState .= Normal




runProgram ∷ V2 Int → Free ObjectF () → StateT Game C.Curses ()
runProgram v prg = do
    o ← uses (g_world.w_map) (last . valuesAt v)
    (gs, w') ← uses g_world (runWorld (snd <$> runObjectMonadWorld prg v o)) -- TODO run program should probably return useful values for interactions???
    g_gameState .= gs
    g_world .= w'


operationProgramForSymbol ∷ Char → InteractionType → Free ObjectF ()
operationProgramForSymbol '+'  = door
operationProgramForSymbol '\'' = door
operationProgramForSymbol '$'  = computer
operationProgramForSymbol '@'  = person
operationProgramForSymbol _    = generic


nameForPosition ∷ V2 Int → Maybe String
nameForPosition (V2 14 6)  = Just "Moe"
nameForPosition (V2 17 13) = Just "Johnny"
nameForPosition (V2 19 13) = Just "Sally"
nameForPosition _          = Nothing


characterForName ∷ DesignData → String → Character Item ConversationNode
characterForName dd name =
    let maybeChar = M.lookup name (view dd_characters dd)
    in  (fromMaybe (view dd_defaultRedshirt dd) maybeChar)


pickAChoice ∷ GameState → Input.Event → StateT Game C.Curses Word
pickAChoice gs (Input.UIEv Input.MoveUp) = do
    g_choiceWindow %= selectPrevious
    use g_choiceWindow >>= lift . drawChoiceWindow
    lift (Input.nextEvent gs) >>= pickAChoice gs
pickAChoice gs (Input.UIEv Input.MoveDown) = do
    g_choiceWindow %= selectNext
    use g_choiceWindow >>= lift . drawChoiceWindow
    lift (Input.nextEvent gs) >>= pickAChoice gs
pickAChoice _ (Input.UIEv Input.SelectChoice) =
    uses g_choiceWindow commit
pickAChoice gs _ = do
    lift (Input.nextEvent gs) >>= pickAChoice gs

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
        v        → do
            os ← uses (g_world.w_map) (drop 1 . valuesAt (ap + v))
            case os of
                [] → pure Nothing
                l  → pure (Just (ap + v, last l))  -- TODO find a way to deal with noninteresting objects (<base>)

--------------------------------------------------------------------------------

byName ∷ String → (Character a b) → Bool
byName n = (==n) . view ch_name


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
    p  ← use (g_world.w_active.e_position)
    t  ← uses (g_world.w_team) (fmap (view e_position))
    s  ← use (g_world.w_status)
    
    doRender $ do
        sequence [ drawMap (view o_symbol) (view o_material) w d v
                 , drawPlayer p
                 , drawTeam t
                 --, maybe (pure (pure ())) drawAim ma
                 ]
                 >>= updateMain . foldl1 (>>)
        drawHud s >>= updateHud


renderMessage ∷ String → StateT Game C.Curses ()
renderMessage msg = doRender (drawHud msg >>= updateHud)


renderComputer ∷ (MonadRender r) ⇒ String → ComputerData → r ()
renderComputer a cd  = updateInteraction $ do
    drawAnswer
    drawPrompt

    C.moveCursor 2 3
    C.drawString (view cd_input cd <> "                                                          ") 
    where
        drawAnswer  = C.moveCursor 1 1 *> C.drawString (a <> "                                        ")
        drawPrompt  = C.moveCursor 2 1 *> C.drawString "> "


