{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Dreamnet.Dreamnet
where

import Control.Lens        (makeLenses, use, uses, view, views, (.=), (%=))
import Control.Monad       (void, when)
import Control.Monad.State (StateT, lift, execStateT)
import Data.Semigroup      ((<>))
import Data.Functor        (($>))
import Linear              (V2)

import UI.NCurses.Class
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre

import Dreamnet.DesignData
import Dreamnet.GameState
import Dreamnet.Input
import Dreamnet.World
import Dreamnet.Conversation

import Dreamnet.TileMap
import Dreamnet.WorldMap
import Dreamnet.Entity
import Dreamnet.ScrollWindow
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.MapObject
import Dreamnet.Visibility
import Dreamnet.Character

--------------------------------------------------------------------------------

data Game = Game {
      _g_world ∷ World Object Visibility (Character Item ConversationNode)
    , _g_gameState ∷ GameState
    , _g_keepRunning ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
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
                              (fromTileMap m (objectFromTile dd) Unknown)
                              [ newCharacter "Carla"   End
                              , newCharacter "Raj"     End
                              , newCharacter "Delgado" End
                              ]
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversationWindow = cvw
      , _g_scrollWindow       = sw
      , _g_choiceWindow       = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."
    }


-- TODO if I do the newtype back again, I don't have to lift everywhere to run curses code
instance MonadCurses (StateT Game C.Curses) where
    liftCurses = lift

--------------------------------------------------------------------------------

launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

switchGameState ∷ GameState → StateT Game C.Curses ()
switchGameState gs = do
    ogs ← use g_gameState
    onStateSwitch ogs gs
    g_gameState .= gs


-- This'll die when renderer gets refactored a bit
doRender ∷ RendererF () → StateT Game C.Curses ()
doRender r = do
    re       ← use g_rendererData
    (x, re') ← lift (runRenderer re r)
    g_rendererData .= re'
    pure x
        

onStateSwitch ∷ GameState → GameState → StateT Game C.Curses ()
onStateSwitch Normal (Examination s) = do
    g_scrollWindow %= setText s
    sw ← use g_scrollWindow
    lift $
        renderScrollWindow sw

onStateSwitch Normal InventoryUI = do
    is ← uses (g_world.w_selected.e_object) (fmap show . equippedContainers)
    g_scrollWindow %= setLines is
    sw ← use g_scrollWindow
    lift $
        renderScrollWindow sw

onStateSwitch Normal CharacterUI = do
    g_scrollWindow %= setText "Character sheet"
    sw ← use g_scrollWindow
    lift $
        renderScrollWindow sw

onStateSwitch (Examination _) Normal = do
    sw ← use g_scrollWindow
    lift $ clearScrollWindow sw
    renderNormal

onStateSwitch Normal (Conversation ch cn) = do
    renderConversation ch cn
    renderNormal
onStateSwitch (Conversation _ _) Normal = do
    w ← use g_conversationWindow
    lift $ clearConversationWindow w
    renderNormal
onStateSwitch _ _ = pure ()

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
        loopTheLoop


loopTheLoop ∷ StateT Game C.Curses ()
loopTheLoop = do
    r ← use g_keepRunning
    when r $ do
        e ← use g_gameState >>= runInput . nextEvent
        
        when (e == Quit) $
            g_keepRunning .= False

        -- States can morph into other states
        -- We need an 'init' function to be alled when state is first changed
        --
        -- If state switch would fire an event type instead of just
        -- being returned by the world, then the pipeline would still hold!
        --
        -- Problem is that update (maybe even render) components would need
        -- access to the input pipeline then, which means World has to be aware
        -- of at least certain portions of the Game.
        --
        -- Maybe onStateSwitch would fire an event, to begin with?

        s ← use g_gameState
        case s of
            Normal → do
                let (WorldEv we) = e
                (gs, w') ← uses g_world (runWorld (updateWorld we))
                switchGameState gs
                g_world .= w'
                case gs of
                    Normal → renderNormal
                    _      → pure ()

            Conversation _ (ChoiceNode l _) → do
                let (UIEv uie) = e
                g_choiceWindow %= setOptions l
                updateConversationChoice uie
                (Conversation n nc) ← use g_gameState
                renderConversation n nc
            Conversation ch cs → do
                let (UIEv uie) = e
                let nc = advance cs
                case nc of
                    End → switchGameState Normal
                    ChoiceNode l _ → do
                        g_choiceWindow %= setOptions l
                        updateConversationChoice uie
                        switchGameState $ Conversation ch nc
                    _   → switchGameState $ Conversation ch nc
                renderConversation ch nc

            Examination _ → do
                let (UIEv uie) = e
                case uie of
                    MoveUp → do
                        g_scrollWindow %= scrollUp
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    MoveDown → do
                        g_scrollWindow %= scrollDown
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    _ → switchGameState Normal

            InventoryUI → do
                let (UIEv uie) = e 
                case uie of
                    MoveUp → do
                        g_scrollWindow %= scrollUp
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    MoveDown → do
                        g_scrollWindow %= scrollDown
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    _ → switchGameState Normal

            CharacterUI → do
                let (UIEv uie) = e 
                case uie of
                    MoveUp → do
                        g_scrollWindow %= scrollUp
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    MoveDown → do
                        g_scrollWindow %= scrollDown
                        sw ← use g_scrollWindow
                        lift (renderScrollWindow sw)
                    _ → switchGameState Normal
                    

            Interaction → do
                let (PassThrough c) = e
                updateComputer c
                qr ← uses g_carlasComputer (view cd_requestedQuit . computerData)
                if qr
                  then switchGameState Normal
                  else do
                       comp ← use g_carlasComputer
                       fbr  ← use g_carlasFramebuffer
                       doRender (renderComputer fbr (computerData comp))

        lift C.render
        loopTheLoop 


allButTheBase ∷ Object → Bool
allButTheBase Base{} = False
allButTheBase _      = True


updateWorld ∷ (Monad w, WorldAPI Object Visibility (Character i c) w) ⇒ WorldEvent → w GameState
updateWorld (Move v) = do
    setStatus ""
    moveSelected v
    switchAim (pure allButTheBase)
    updateVisible
    updateAi *> alarmStateCheck
    pure Normal
updateWorld (Aim v) = do -- TODO should spend time?
    setStatus ""
    moveAim v 
    pure Normal
updateWorld NextAim = switchAim (pure allButTheBase) $> Normal
updateWorld Examine = Examination <$> examine
updateWorld Interact = do
    setStatus ""
    s ← interactOrElse (\v os → objectInteraction v (last os)) (pure Normal)
    switchAim Nothing
    updateVisible
    updateAi *> alarmStateCheck
    pure s
updateWorld UseHeld = interactOrElse doIt (pure Normal)
    where
        doIt v os = do
            setStatus "*pew pew*"
            deleteObject v (last os)
            pure Normal
updateWorld Get = do
    setStatus ""
    o ← get
    setStatus (maybe  "There's nothing there." ("Picked up " <>) o)
    switchAim Nothing
    updateAi *> alarmStateCheck
    pure Normal
updateWorld Wait = do
    setStatus "Waiting..."
    updateVisible
    updateAi *> alarmStateCheck
    pure Normal
updateWorld InventorySheet = pure InventoryUI
updateWorld CharacterSheet = pure CharacterUI
updateWorld (SelectTeamMember 0) = selectByName "Carla"
updateWorld (SelectTeamMember 1) = selectByName "Raj"
updateWorld (SelectTeamMember 2) = selectByName "Delgado"
updateWorld (SelectTeamMember _) = pure Normal


selectByName ∷ (Monad w, WorldAPI Object Visibility (Character i c) w) ⇒ String → w GameState
selectByName n = do
    switchAim Nothing
    selectCharacter byName
    pure Normal
    where
        byName = (==n) . view ch_name


alarmStateCheck ∷ (Monad w, WorldAPI Object b c w) ⇒ w ()
alarmStateCheck = do
    wm ← worldMap
    let isAlarm = views wm_data (foldr cameraActivated False) wm
    when isAlarm $ setStatus "***** ALARM *****"
    where
        cameraActivated ol p = (p ||) $ or $ flip fmap ol $
                                  \case
                                    (Camera x) → x > 7
                                    _          → False


objectInteraction ∷ (Applicative w, WorldAPI Object b c w) ⇒ V2 Int → Object → w GameState
objectInteraction v d@(Door o) = changeObject_ v d (Door (not o)) $> Normal
objectInteraction _ Computer   = pure Interaction
objectInteraction _ (Person c) = pure (Conversation <$> view ch_name <*> view ch_conversation $ c)
objectInteraction _ o          = setStatus ("Tried interaction with: " <> show o) $> Normal


updateConversationChoice ∷ UIEvent → StateT Game C.Curses ()
updateConversationChoice MoveUp       = g_choiceWindow %= selectPrevious
updateConversationChoice MoveDown     = g_choiceWindow %= selectNext
updateConversationChoice SelectChoice = do
    i ← uses g_choiceWindow commit
    (Conversation ch cs) ← use g_gameState
    let nc = pick i cs
    switchGameState (Conversation ch nc)
    renderConversation ch nc -- <FUUUGLY!!!!!
updateConversationChoice Back = pure ()


updateComputer ∷ Char → StateT Game C.Curses ()
updateComputer '\n' = do
    o ← uses g_carlasComputer (*> commitInput)
    g_carlasFramebuffer .= computerOutput o
    g_carlasComputer %= (\l → l *> commitInput $> ())
updateComputer '\b' = g_carlasComputer %= (*> backspace)
updateComputer c    = g_carlasComputer %= (*> input c)


renderNormal ∷ StateT Game C.Curses ()
renderNormal = do
    m  ← use (g_world.w_map)
    p  ← use (g_world.w_selected.e_position)
    t  ← uses (g_world.w_team) (fmap (view e_position))
    ma ← use (g_world.w_aim)
    s  ← use (g_world.w_status)
    
    mats ← use (g_rendererData.rd_styles.s_materials)
    def  ← use (g_rendererData.rd_styles.s_visibilityVisible)
    doRender $ do
        drawMap objectToChar (objectToMat mats def) m
        drawPlayer p
        drawTeam t
        drawHud s
        maybe (pure ()) drawAim ma


renderConversation ∷ String → ConversationNode → StateT Game C.Curses ()
renderConversation _ (TalkNode s _)   = do
    w ← use g_conversationWindow
    lift $ do
        clearConversationWindow w 
        drawConversationWindow 0 "Carla" s w
renderConversation n (ListenNode s _) = do
    w ← use g_conversationWindow
    lift $ do
        clearConversationWindow w
        drawConversationWindow 1 n s w
renderConversation _ (ChoiceNode _ _) = do
    w ← use g_conversationWindow
    cw ← use g_choiceWindow
    lift $ do
        clearConversationWindow w
        drawChoiceWindow cw   -- TODO MOVE this to Actual choice node, to use the fucking model!
renderConversation _ _ = pure () -- We'll never end up here


renderComputer ∷ (MonadRender r) ⇒ String → ComputerData → r ()
renderComputer a cd  = use rd_interactionWindow >>= \w → updateWindow w $ do
    drawAnswer
    drawPrompt

    C.moveCursor 2 3
    C.drawString (view cd_input cd <> "                                                          ") 
    where
        drawAnswer  = C.moveCursor 1 1 *> C.drawString (a <> "                                        ")
        drawPrompt  = C.moveCursor 2 1 *> C.drawString "> "


