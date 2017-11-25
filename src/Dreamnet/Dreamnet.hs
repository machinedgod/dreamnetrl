{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Dreamnet
where

import Prelude hiding (interact)
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.State
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

import Dreamnet.GameState
import qualified Dreamnet.TileMap as TMap
import Dreamnet.Input
import Dreamnet.World
import Dreamnet.Game
import Dreamnet.Conversation

import Dreamnet.Renderer
import Dreamnet.UI.ChoiceBox
import Dreamnet.UI.ConversationView
import Dreamnet.UI.InformationWindow

--------------------------------------------------------------------------------

data DesignData = DesignData {
      something ∷ Int
    , somethingElse ∷ String
    }


defaultDesignData ∷ DesignData
defaultDesignData = DesignData 5 "Hi"


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
    r ← view g_keepRunning
    when r $ do
        e ← doInput
        let (WorldEv we)      = e
            (UIEv uie)        = e
            (PassThrough pte) = e

        when (e == Quit)
            stopGameLoop

        s ← view g_gameState
        case s of
            Normal → doUpdate (updateWorld we)

            Conversation (ChoiceNode s _) → case uie of
                MoveUp   → choiceUp
                MoveDown → choiceDown
                SelectChoice → do
                    sel ← view (g_choiceModel.cm_currentSelection)
                    doConversation (pick sel)
                Back     → return ()
            Conversation cs → doConversation (advance cs)

            Examination _ → case uie of
                MoveUp       → scrollUp
                MoveDown     → scrollDown
                SelectChoice → switchGameState Normal
                Back         → switchGameState Normal
 

            _ → return () -- We have no other updates coded in ATM


        s' ← view g_gameState
        cm ← view g_choiceModel
        doRender $ do
            case s' of
                Normal          → renderNormal
                Examination s   → drawCenteredWindow "Examine" s
                Interaction     → return ()
                Conversation cn → renderConversation cm cn
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
    return s


-- Returned bool: close window?
updateScroll ∷ (MonadGame g) ⇒ UIEvent → g ()
updateScroll MoveUp   = scrollUp
updateScroll MoveDown = scrollDown
updateScroll _        = switchGameState Normal


renderNormal ∷ RendererF ()
renderNormal = do
    drawMap
    drawObjects
    drawPlayer
    drawAim
    drawHud


renderConversation ∷ (MonadRender r) ⇒ ChoiceModel → ConversationNode → r ()
renderConversation cm (TalkNode s _)    = clearConversationWindow 1 >> drawConversationWindow 0 "Carla" s
renderConversation cm (ListenNode s _)  = clearConversationWindow 0 >> drawConversationWindow 1 "Moe"   s
renderConversation cm (ChoiceNode ls _) = clearConversationWindow 1 >> drawChoice (cm^.cm_currentSelection) (Vec.fromList ls)
renderConversation _ _                  = return () -- We'll never end up here
