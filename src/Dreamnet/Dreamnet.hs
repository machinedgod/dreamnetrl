{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Dreamnet
where

import Prelude hiding (interact, take)
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.State hiding (get)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Data.Bool  (bool)
import Data.Char  (ord)
import Data.List  (elemIndex, nub, unfoldr, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Set    as Set
import qualified Data.Vector as Vec
import qualified Data.Map    as Map
import Linear

import qualified UI.NCurses  as Curses
import qualified Config.Dyre as Dyre

import Dreamnet.DesignData
import Dreamnet.GameState
import qualified Dreamnet.TileMap as TMap
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
    Curses.setCursorMode Curses.CursorInvisible
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

            Conversation _ (ChoiceNode s _) → let (UIEv uie) = e in updateConversationChoice uie
            Conversation _ cs → doConversation (advance cs)

            Examination s → let (UIEv uie) = e in updateScroll uie

            InventoryUI → let (UIEv uie) = e in updateScroll uie
            CharacterUI → let (UIEv uie) = e in updateScroll uie

            Interaction → let (PassThrough c) = e in updateComputer c

            _ → return () -- We have no other updates coded in ATM

        s' ← gameState
        doRender $ do
            case s' of
                Normal            → renderNormal
                Examination _     → drawCenteredWindow
                Conversation n cn → renderConversation n cn
                Interaction       → renderComputerScreen
                InventoryUI       → drawCenteredWindow
                CharacterUI       → drawCenteredWindow
                _                 → return ()
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
updateComputer c = return ()


renderNormal ∷ RendererF ()
renderNormal = do
    drawMap
    --drawObjects
    drawPlayer
    drawAim
    drawHud


renderConversation ∷ (MonadRender r) ⇒ String → ConversationNode → r ()
renderConversation n (TalkNode s _)    = clearConversationWindow 1 >> drawConversationWindow 0 "Carla" s
renderConversation n (ListenNode s _)  = clearConversationWindow 0 >> drawConversationWindow 1 n s
renderConversation n (ChoiceNode ls _) = clearConversationWindow 1 >> use (rd_choiceModel.cm_currentSelection) >>= (`drawChoice` (Vec.fromList ls))   -- TODO MOVE this to Actual choice node, to use the fucking model!
renderConversation n _                 = return () -- We'll never end up here


renderComputerScreen ∷ (MonadRender r) ⇒ r ()
renderComputerScreen = use rd_interactionWindow >>= \w → updateWindow w $ do
    Curses.clear        
    Curses.moveCursor 1 1
    Curses.drawString "Ready."
    Curses.moveCursor 2 1
    Curses.drawString "> "

