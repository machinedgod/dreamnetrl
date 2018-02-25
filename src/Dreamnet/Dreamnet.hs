{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Safe                      (succSafe, predSafe)
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
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import qualified Dreamnet.Input as Input
import Dreamnet.World
import Dreamnet.Conversation

import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Entity
import Dreamnet.ScrollWindow
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.Visibility
import Dreamnet.Character
import Dreamnet.ObjectMonad

import DesignData

--------------------------------------------------------------------------------

type Name = String

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
    , _g_scrollWindow ∷ ScrollData
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
    sw  ← createScrollData
    cw  ← createChoiceData
    pure Game {
        _g_turn  = 0
      , _g_world = newWorld
                       (fromTileMap (view dd_startingMap dd) objectFromTile)
                       (playerPerson <$> [ "Carla"
                                         , "Delgado"
                                         --, "Raj"
                                         --, "570rm"
                                         ])
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversant   = Nothing
      , _g_conversation = End
      , _g_scrollWindow = sw
      , _g_choiceWindow = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."

      , _g_hudTeamSelector = 0
      , _g_watchAlarmTime  = 1234
      , _g_watchButton     = 0
    }

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
            HudTeam      → lift Input.nextUiEvent          >>= processHudTeam
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
                    g_scrollWindow %= moveWindow (V2 2 1)
                    g_scrollWindow %= resizeWindow (V2 40 10)
                    g_scrollWindow %= setTitle Nothing
                    g_scrollWindow %= setText examineText
                    g_gameState .= Examination
                    use g_scrollWindow >>= lift . renderScrollWindow
                else do
                    runProgram dd v (programForObject o Examine)
                    use g_gameState >>= \case
                        Examination → do
                            g_scrollWindow %= moveWindow (V2 2 1)
                            g_scrollWindow %= resizeWindow (V2 40 10)
                            use (g_world.w_status) >>= \et → g_scrollWindow %= setText et
                            g_world.w_status .= ""
                            use g_scrollWindow >>= lift . renderScrollWindow
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
                    g_scrollWindow %= setTitle (Just (view ch_name ch))
                    use g_conversation >>= \case
                        (ChoiceNode opts _) → g_choiceWindow %= setOptions opts
                        (TalkNode s _)      → g_scrollWindow %= setText s
                        (ListenNode s _)    → g_scrollWindow %= setText s
                        _ → pure ()
                    use g_conversation >>= renderConversation
                _ →
                    renderNormal
processNormal _ Input.InventorySheet = do
    g_scrollWindow %= setTitle (Just "Inventory sheet")
    g_scrollWindow %= setText ""
    g_gameState .= InventoryUI
    use g_scrollWindow >>= lift . renderScrollWindow
processNormal _ Input.CharacterSheet = do
    g_scrollWindow %= setTitle (Just "Character sheet")
    g_scrollWindow %= setText ""
    g_gameState .= CharacterUI
    use g_scrollWindow >>= lift .  renderScrollWindow


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


processHudTeam ∷ Input.UIEvent → StateT Game C.Curses ()
processHudTeam Input.TabNext = do
    g_gameState .= HudMessages
    renderNormal
processHudTeam Input.TabPrevious = do
    g_gameState .= HudWatch
    renderNormal
processHudTeam Input.MoveUp = do
    g_hudTeamSelector %= max 0 . subtract 3
    renderNormal
processHudTeam Input.MoveDown = do
    l ← uses (g_world.w_team) (fromIntegral . length)
    g_hudTeamSelector %= min l . (+3)
    renderNormal
processHudTeam Input.MoveLeft = do
    g_hudTeamSelector %= max 0 . subtract 1
    renderNormal
processHudTeam Input.MoveRight = do
    l ← uses (g_world.w_team) (fromIntegral . length)
    g_hudTeamSelector %= min l . (+1)
    renderNormal
processHudTeam Input.SelectChoice = do
    renderMessage "Showing charcter sheet!"
    renderNormal
processHudTeam Input.Back = do
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
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                g_scrollWindow %= setTitle (Just initName)
                g_scrollWindow %= setText s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                g_scrollWindow %= setTitle (Just otherName)
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
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                g_scrollWindow %= setTitle (Just initName)
                g_scrollWindow %= setText s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                g_scrollWindow %= setTitle (Just otherName)
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
                initName ← uses (g_world.w_active.e_object.o_state) (\(Person ch) → view ch_name ch)
                g_scrollWindow %= setTitle (Just initName)
                g_scrollWindow %= setText s
                use g_conversation >>= renderConversation
            (ListenNode s _) → do
                otherName ← uses g_conversant (fromMaybe "<CONVERSANT IS NOTHING>" . fmap (view ch_name))
                g_scrollWindow %= setTitle (Just otherName)
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
processConversation _ =
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
    renderMessage "Select direction:"
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


renderMessage ∷ String → StateT Game C.Curses ()
renderMessage msg = do
    gs ← use g_gameState
    doRender $ drawStatus gs msg >>= updateHud


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

