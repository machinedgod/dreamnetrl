{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Dreamnet.Game
( MonadGame(..)
, Game
, g_keepRunning
, g_world
, g_gameState
, g_scrollModel
, g_choiceModel
, newGame
, runGame

, ScrollModel(..)
, sm_scrollLine
) where

import Prelude hiding (interact)
import Control.Monad.State
import Control.Lens

import qualified Data.Vector as Vec

import Dreamnet.GameState
import Dreamnet.TileMap
import Dreamnet.World
import Dreamnet.Renderer
import Dreamnet.Input
import Dreamnet.Conversation

import Dreamnet.UI.ChoiceBox
import Dreamnet.UI.ConversationView

import UI.NCurses.Class
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

newtype ScrollModel = ScrollModel {
      _sm_scrollLine ∷ Word
    }

makeLenses ''ScrollModel

--------------------------------------------------------------------------------

-- TODO this API should help build event->update->render pipelines
--      and ease switching between them, not deal with specifics of the
--      game.
class (MonadReader Game m) ⇒ MonadGame m where
    switchGameState ∷ GameState → m ()
    stopGameLoop ∷ m ()

    doInput  ∷ m Event
    doUpdate ∷ WorldM GameState → m ()
    doRender ∷ RendererF () → m ()
    -- Too specific?
    doConversation ∷ ConversationM ConversationNode → m ()

    -- SHaky!
    scrollUp ∷ m ()
    scrollDown ∷ m ()
    choiceUp ∷ m ()
    choiceDown ∷ m ()



data Game = Game {
      _g_world ∷ World
    , _g_gameState ∷ GameState
    , _g_keepRunning ∷ Bool
    , _g_rendererData ∷ RendererData

    -- TODO these should *probably* get out of here, but where else?
    , _g_scrollModel ∷ ScrollModel
    , _g_choiceModel ∷ ChoiceModel
    }

makeLenses ''Game

newGame ∷ Curses.Curses Game
newGame = do
    rdf ← initRenderer
    m   ← loadTileMap "res/bar"
    return $ Game (newWorld m) Normal True rdf (ScrollModel 0) (ChoiceModel Vec.empty 0)

--------------------------------------------------------------------------------

-- I *could* nest update, render and input monad stack in here,
-- Would that be a better idea than how its working now?
newtype GameM a = GameM { runGameM ∷ StateT Game Curses.Curses a }
                deriving (Functor, Applicative, Monad, MonadState Game, MonadCurses)


instance MonadReader Game GameM where
    ask       = get 
    local _ _ = error "No idea how to implement this!"
    --local f mr = evalStateT mr . r <$> get


instance MonadGame GameM where
    switchGameState gs = g_gameState .= gs
    stopGameLoop = g_keepRunning .= False
    doInput = do
        ps ← use g_gameState
        runInput (nextEvent ps)
    doUpdate um = do
        w ← use g_world
        let (gs, w') = runWorld um w
        g_world .= w'
        g_gameState .= gs
        --case gs of
        --    Conversation c → g_gameState .= Conversation (c `runConversation` advance c)
        --    _ → return ()
    doRender r = do
        w  ← use g_world
        re ← use g_rendererData
        liftCurses $ runRenderer re w r
    doConversation cm = use g_gameState >>= \case
        Conversation c → do
            let nc = c `runConversation` cm
            case nc of
                End → do
                    g_gameState .= Normal
                    doRender $ clearConversationWindow 0 >> clearConversationWindow 1
                _   → g_gameState .= Conversation nc
        _ → return ()
    scrollUp   = g_scrollModel.sm_scrollLine -= 1
    scrollDown = g_scrollModel.sm_scrollLine += 1
    choiceUp   = g_choiceModel.cm_currentSelection -= 1
    choiceDown = g_choiceModel.cm_currentSelection += 1


runGame ∷ Game → GameM () → Curses.Curses ()
runGame g gf = void $ execStateT (runGameM gf) g

