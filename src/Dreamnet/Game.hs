{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Game
( MonadGame(..)
, GameF
, Game
, g_world
, g_keepRunning
, newGame
, runGame
) where

import Prelude hiding (interact)
import Control.Monad.State
import Control.Lens

import Dreamnet.World
import Dreamnet.Renderer
import Dreamnet.Input

import UI.NCurses.Class
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

class (MonadState Game m) ⇒ MonadGame m where
    doInput  ∷ m ()
    doUpdate ∷ WorldF () → m ()
    doRender ∷ RendererF () → m ()


data Game = Game {
      _g_input ∷ Event
    , _g_world ∷ World
    , _g_keepRunning ∷ Bool

    , _g_createRenderData ∷ (World → RendererData)
    }

makeLenses ''Game

newGame ∷ (MonadIO m) ⇒ (World → RendererData) → m Game
newGame rdf = do
    m ← loadMap "res/map1"
    return $ Game Start (newWorld m) True rdf

--------------------------------------------------------------------------------

-- I *could* nest update, render and input monads in here.
-- Would that be a better idea than how its working now?
newtype GameF a = GameF { runGameF ∷ StateT Game Curses.Curses a }
                deriving (Functor, Applicative, Monad, MonadState Game, MonadCurses)


instance MonadGame GameF where
    doInput = do
        e ← runInput nextEvent
        case e of
            Quit → g_keepRunning .= False
            e    → g_input .= e
    doUpdate um = do
        e ← use g_input
        g_world %= runWorld um e
    doRender r = do
        w  ← use g_world
        re ← use g_createRenderData
        liftCurses $ runRenderer (re w) r


runGame ∷ Game → GameF () → Curses.Curses ()
runGame g gf = void $ execStateT (runGameF gf) g

