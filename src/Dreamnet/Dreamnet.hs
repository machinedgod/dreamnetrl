{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.List  (elemIndex, nub, unfoldr)
import Data.Maybe (fromMaybe)
import qualified Data.Set            as Set
import qualified Data.Vector         as Vec
import Linear

import qualified UI.NCurses  as Curses
import qualified Config.Dyre as Dyre

import Dreamnet.Input
import Dreamnet.Renderer
import Dreamnet.World
import Dreamnet.Game

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

    rdf ← initRenderer
    g   ← newGame rdf
    runGame g loopTheLoop
    where
        loopTheLoop ∷ (MonadGame m) ⇒ m ()
        loopTheLoop = do
            doInput
            r ← use g_keepRunning
            when r $ do
                doUpdate update
                doRender render
                loopTheLoop 
            
update ∷ WorldF ()
update = do
    e ← ask
    case e of
        Open   → openDoors
        Close  → closeDoors
        Move v → moveActive v
        Aim  v → moveAim v

        _      → return ()

    updateVisible
    where
        openDoors  = interact $ \v o → case o of
            ClosedDoor → changeObject v OpenedDoor
            _          → return ()
        closeDoors = interact $ \v o → case o of
            OpenedDoor → changeObject v ClosedDoor
            _          → return ()
        moveActive v = do
             movePlayer v
             updateAim



render ∷ RendererF ()
render = do
    drawMap
    drawPlayer
    drawAim
    swap

