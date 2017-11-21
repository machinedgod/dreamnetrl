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

import qualified Dreamnet.TileMap as TMap
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
            doUpdate updateVisible
            doRender render
            doInput
            r ← use g_keepRunning
            when r $ do
                doUpdate update
                doRender render
                loopTheLoop 
 

update ∷ WorldF ()
update = do
    w_status .= replicate 80 ' '
    e ← ask
    case e of
        Move v   → movePlayer v >> switchAim
        NextAim  → switchAim
        Interact → interactWithAim >> w_aim .= Nothing
        _        → return ()

    updateVisible
    where
        -- TODO pull specific object data from some map or store it next to a tile
        interactWithAim = interact $ \v o → case o of
            Computer    → w_status .= "Using a computer"
            Person      → w_status .= "Talking to someone"
            Door o      → changeObject_ v (Door (not o))
            Container t → w_status .= "Inspecting the " ++ show t ++ " for stuff..."
            Dispenser t → w_status .= "Dispensing items from the " ++ show t
            Stairs t    → w_status .= "These lead to " ++ show t
            Prop t      → case t of
                              TMap.Table → w_status .= "Nothing to do with this table."
                              TMap.Chair → do
                                w_status .= "You sit down and chill out..."
                                w_playerPos .= v



render ∷ RendererF ()
render = do
    drawMap
    drawPlayer
    drawAim
    view (re_world.w_status) >>= messagePrint
    swap

