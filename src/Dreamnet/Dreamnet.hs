{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Dreamnet.Dreamnet
where

import Prelude hiding (interact, take)
import Control.Lens hiding (re)
import Control.Monad.State hiding (get)
import Data.Semigroup

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
import Dreamnet.ScrollWindow
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.UI.ConversationView

--------------------------------------------------------------------------------

data Game = Game {
      _g_world ∷ World
    , _g_gameState ∷ GameState
    , _g_keepRunning ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
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
    --m   ← loadTileMap "res/apartment0"
    m   ← loadTileMap "res/bar"
    sw  ← createScrollData
    cw  ← createChoiceData
    return $ Game {
        _g_world        = (newWorld (fromTileMap dd m))
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_scrollWindow = sw
      , _g_choiceWindow = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."
    }


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
    return x
        

onStateSwitch ∷ GameState → GameState → StateT Game C.Curses ()
onStateSwitch Normal (Examination s) = do
    g_scrollWindow %= setText s
    sw ← use g_scrollWindow
    lift $ do
        renderScrollWindow sw

onStateSwitch Normal InventoryUI = do
    is ← uses (g_world.w_playerCharacter.ch_inventory) (fmap (view i_name))
    g_scrollWindow %= setLines is
    sw ← use g_scrollWindow
    lift $ do
        renderScrollWindow sw

onStateSwitch Normal CharacterUI = do
    g_scrollWindow %= setText "Character sheet"
    sw ← use g_scrollWindow
    lift $ do
        renderScrollWindow sw

onStateSwitch (Examination _) Normal = do
    sw ← use g_scrollWindow
    lift $ clearScrollWindow sw
    renderNormal

onStateSwitch Normal (Conversation ch cn) = do
    renderConversation ch cn
    renderNormal
onStateSwitch (Conversation _ _) Normal = do
    doRender $ clearConversationWindow 0 *> clearConversationWindow 1
    renderNormal
onStateSwitch _ _ = return ()


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
        g_world %= snd . runWorld (updateVisible *> return Normal)
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
                when (gs == Normal) $
                    renderNormal

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
                comp ← use g_carlasComputer
                fbr  ← use g_carlasFramebuffer
                doRender (renderComputer fbr (computerData comp))

        lift $ C.render
        loopTheLoop 


updateWorld ∷ (MonadWorld w) ⇒ WorldEvent → w GameState
updateWorld (Move v) = do
    movePlayer v
    switchAim
    updateVisible
    return Normal
updateWorld NextAim = switchAim *> return Normal
updateWorld Examine = Examination <$> examine
updateWorld Interact = do
    s ← interactOrElse objectInteraction (return Normal)
    w_aim .= Nothing
    updateVisible
    return s
updateWorld Get = get >>= \case
        Just i → do
            w_status .= "Picked up " <> (i^.i_name)
            w_aim .= Nothing
            return Normal
        Nothing → do
            w_status .= "There's nothing there."
            w_aim .= Nothing
            return Normal
updateWorld InventorySheet = return InventoryUI
updateWorld CharacterSheet = return CharacterUI


updateConversationChoice ∷ UIEvent → StateT Game C.Curses ()
updateConversationChoice MoveUp       = g_choiceWindow %= selectPrevious
updateConversationChoice MoveDown     = g_choiceWindow %= selectNext
updateConversationChoice SelectChoice = do
    i ← uses g_choiceWindow commit
    (Conversation ch cs) ← use g_gameState
    let nc = pick i cs
    switchGameState (Conversation ch nc)
    renderConversation ch nc -- <FUUUGLY!!!!!
updateConversationChoice Back = return ()


updateComputer ∷ Char → StateT Game C.Curses ()
updateComputer '\n' = do
    o ← uses g_carlasComputer (*> commitInput)
    g_carlasFramebuffer .= computerOutput o
    g_carlasComputer %= (\l → l *> commitInput *> return ())
updateComputer '\b' = g_carlasComputer %= (*> backspace)
updateComputer c    = g_carlasComputer %= (*> input c)


renderNormal ∷ StateT Game C.Curses ()
renderNormal = do
    m  ← use (g_world.w_map)
    p  ← use (g_world.w_playerPos)
    ma ← use (g_world.w_aim)
    s  ← use (g_world.w_status)
    doRender $ do
        drawMap m
        drawPlayer p
        drawHud s
        maybe (return ()) drawAim ma


renderConversation ∷ String → ConversationNode → StateT Game C.Curses ()
renderConversation _ (TalkNode s _) = doRender $ clearConversationWindow 1 *> drawConversationWindow 0 "Carla" s
renderConversation n (ListenNode s _) = doRender $ clearConversationWindow 0 *> drawConversationWindow 1 n s
renderConversation _ (ChoiceNode _ _) = do
    cw ← use g_choiceWindow
    doRender $ clearConversationWindow 1
    liftCurses $ drawChoiceWindow cw   -- TODO MOVE this to Actual choice node, to use the fucking model!
renderConversation _ _ = return () -- We'll never end up here


renderComputer ∷ (MonadRender r) ⇒ String → ComputerData → r ()
renderComputer a cd  = use rd_interactionWindow >>= \w → updateWindow w $ do
    drawAnswer
    drawPrompt

    C.moveCursor 2 3
    C.drawString (view cd_input cd <> "                                                          ") 
    where
        drawAnswer  = C.moveCursor 1 1 *> C.drawString (a <> "                                        ")
        drawPrompt  = C.moveCursor 2 1 *> C.drawString "> "


