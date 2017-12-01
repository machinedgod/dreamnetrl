{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Dreamnet
where

import Prelude hiding (interact, take)
import Control.Lens
import Control.Monad.State hiding (get)

import qualified Data.Vector as Vec

import qualified UI.NCurses  as Curses
import qualified Config.Dyre as Dyre

import Dreamnet.DesignData
import Dreamnet.GameState
import Dreamnet.Input
import Dreamnet.World
import Dreamnet.Game
import Dreamnet.Conversation

import Dreamnet.ScrollModel
import Dreamnet.ChoiceModel
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.UI.ChoiceBox
import Dreamnet.UI.ConversationView
import Dreamnet.UI.InformationWindow

--------------------------------------------------------------------------------

launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

dreamnet ∷ DesignData → IO ()
dreamnet d = Curses.runCurses $ do
    -- Init curses
    Curses.setRaw     True
    Curses.setEcho    False
    Curses.defaultWindow >>= (`Curses.setKeypad` True)
    void $ Curses.setCursorMode Curses.CursorInvisible
    g ← newGame
    runGame g $ do
        doUpdate $ updateVisible >> return Normal
        doRender $ renderNormal >> swap
        loopTheLoop


loopTheLoop ∷ (MonadGame m) ⇒ m ()
loopTheLoop = do
    r ← use g_keepRunning
    when r $ do
        e ← doInput
        
        when (e == Quit)
            stopGameLoop

        s ← gameState
        case s of
            Normal → doUpdate (let (WorldEv we) = e in updateWorld we)

            Conversation _ (ChoiceNode _ _) → let (UIEv uie) = e in updateConversationChoice uie
            Conversation _ cs → doConversation (advance cs)

            Examination _ → let (UIEv uie) = e in updateScroll uie

            InventoryUI → let (UIEv uie) = e in updateScroll uie
            CharacterUI → let (UIEv uie) = e in updateScroll uie

            Interaction → let (PassThrough c) = e in updateComputer c

        s' ← gameState
        comp ← use g_carlasComputer
        fbr  ← use g_carlasFramebuffer
        doRender $ do
            case s' of
                Normal            → renderNormal
                Examination _     → drawCenteredWindow
                Conversation n cn → renderConversation n cn
                Interaction       → renderComputer fbr (computerData comp) 
                InventoryUI       → drawCenteredWindow
                CharacterUI       → drawCenteredWindow
            swap
        loopTheLoop 


updateWorld ∷ (MonadWorld w) ⇒ WorldEvent → w GameState
updateWorld (Move v) = do
    movePlayer v
    switchAim
    updateVisible
    return Normal
updateWorld NextAim = switchAim >> return Normal
updateWorld Examine = Examination <$> examine
updateWorld Interact = do
    s ← interactOrElse objectInteraction (return Normal)
    w_aim .= Nothing
    updateVisible
    return s
updateWorld Get = get >>= \case
        Just i → do
            w_status .= "Picked up " ++ (i^.i_name)
            w_aim .= Nothing
            return Normal
        Nothing → do
            w_status .= "There's nothing there."
            w_aim .= Nothing
            return Normal
updateWorld InventorySheet = return InventoryUI
updateWorld CharacterSheet = return CharacterUI


-- Returned bool: close window?
updateScroll ∷ (MonadGame g) ⇒ UIEvent → g ()
updateScroll MoveUp   = g_rendererData.rd_scrollModel %= (>> scrollUp)
updateScroll MoveDown = g_rendererData.rd_scrollModel %= (>> scrollDown)
updateScroll _        = switchGameState Normal >> doRender clearCenteredWindow


updateConversationChoice ∷ (MonadGame g) ⇒ UIEvent → g ()
updateConversationChoice MoveUp       = g_rendererData.rd_choiceModel.cm_currentSelection -= 1
updateConversationChoice MoveDown     = g_rendererData.rd_choiceModel.cm_currentSelection += 1
updateConversationChoice SelectChoice = selection >>= doConversation . pick
    where
        selection = use (g_rendererData.rd_choiceModel.cm_currentSelection)
updateConversationChoice Back = return ()


updateComputer ∷ (MonadGame g) ⇒ Char → g ()
updateComputer '\n' = do
    o ← uses g_carlasComputer (>> commitInput)
    g_carlasFramebuffer .= computerOutput o
    g_carlasComputer %= (\l → l >> commitInput >> return ())
updateComputer '\b' = g_carlasComputer %= (>> backspace)
updateComputer c    = g_carlasComputer %= (>> input c)


renderNormal ∷ RendererF ()
renderNormal = do
    drawMap
    drawPlayer
    drawAim
    drawHud


renderConversation ∷ (MonadRender r) ⇒ String → ConversationNode → r ()
renderConversation _ (TalkNode s _)    = clearConversationWindow 1 >> drawConversationWindow 0 "Carla" s
renderConversation n (ListenNode s _)  = clearConversationWindow 0 >> drawConversationWindow 1 n s
renderConversation _ (ChoiceNode ls _) = clearConversationWindow 1 >> use (rd_choiceModel.cm_currentSelection) >>= (`drawChoice` (Vec.fromList ls))   -- TODO MOVE this to Actual choice node, to use the fucking model!
renderConversation _ _                 = return () -- We'll never end up here


renderComputer ∷ (MonadRender r) ⇒ String → ComputerData → r ()
renderComputer a cd  = use rd_interactionWindow >>= \w → updateWindow w $ do
    drawAnswer
    drawPrompt

    Curses.moveCursor 2 3
    Curses.drawString (view cd_input cd ++ "                                                          ") 
    where
        drawAnswer  = Curses.moveCursor 1 1 >> Curses.drawString (a ++ "                                        ")
        drawPrompt  = Curses.moveCursor 2 1 >> Curses.drawString "> "
