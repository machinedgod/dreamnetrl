{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Game
( MonadGame(..)
, Game
, g_keepRunning
, g_world
, g_rendererData
, newGame
, runGame

, g_carlasComputer
, g_carlasFramebuffer
) where

import Prelude hiding (interact)
import Control.Monad.State
import Control.Lens hiding (re)

import Dreamnet.GameState
import Dreamnet.TileMap
import Dreamnet.WorldMap
import Dreamnet.World
import Dreamnet.Renderer
import Dreamnet.Input

import UI.NCurses.Class
import qualified UI.NCurses as Curses

import Dreamnet.ScrollModel          (setText, setLines)
import Dreamnet.ComputerModel

--------------------------------------------------------------------------------

-- TODO this API should help build event->update->render pipelines
--      and ease switching between them, not deal with specifics of the
--      game.
class (MonadState Game m) ⇒ MonadGame m where
    switchGameState ∷ GameState → m ()
    gameState       ∷ m GameState
    stopGameLoop    ∷ m ()

    doInput  ∷ m Event
    doUpdate ∷ WorldM GameState → m ()
    doRender ∷ RendererF a → m a


data Game = Game {
      _g_world ∷ World
    , _g_gameState ∷ GameState
    , _g_keepRunning ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- TODO take this out, eventually
    , _g_carlasComputer ∷ ComputerM ()
    , _g_carlasFramebuffer ∷ String
    }

makeLenses ''Game


newGame ∷ Curses.Curses Game
newGame = do
    rdf ← initRenderer
    --m   ← loadTileMap "res/apartment0"
    m   ← loadTileMap "res/bar"
    return $ Game (newWorld (fromTileMap m)) Normal True rdf newComputer "Ready."

--------------------------------------------------------------------------------

-- I *could* nest update, render and input monad stack in here,
-- Would that be a better idea than how its working now?
newtype GameM a = GameM { runGameM ∷ StateT Game Curses.Curses a }
                deriving (Functor, Applicative, Monad, MonadState Game, MonadCurses)


instance MonadCurses (StateT Game Curses.Curses) where
    liftCurses = lift


instance MonadGame GameM where
    switchGameState gs = do
        ogs ← use g_gameState
        when (gs /= ogs) $ do
            onStateSwitch ogs gs
            g_gameState .= gs
    gameState = use g_gameState
    stopGameLoop = g_keepRunning .= False
    doInput = do
        ps ← gameState
        runInput (nextEvent ps)
    doUpdate um = do
        (gs, w') ← uses g_world (runWorld um)
        switchGameState gs
        g_world .= w'
    doRender r = do
        w  ← use g_world
        re ← use g_rendererData
        (x, re') ← liftCurses (runRenderer re w r)
        g_rendererData .= re'
        return x


onStateSwitch ∷ (MonadGame g) ⇒ GameState → GameState → g ()
onStateSwitch Normal (Examination s) = g_rendererData.rd_scrollModel %= (*>setText s)
onStateSwitch Normal InventoryUI     = do
    is ← uses (g_world.w_playerCharacter.ch_inventory) (fmap (view i_name))
    g_rendererData.rd_scrollModel %= (*>setLines is)
onStateSwitch Normal CharacterUI     = g_rendererData.rd_scrollModel %= (*>setText "Character sheet")
onStateSwitch _ _ = return ()


runGame ∷ Game → GameM () → Curses.Curses ()
runGame g gf = void $ execStateT (runGameM gf) g

