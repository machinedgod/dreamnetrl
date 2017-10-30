{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Dreamnet
( launchDreamnet
, defaultDesignData
) where

import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.State
import Data.Char (ord)
import Linear

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import qualified Config.Dyre        as Dyre

--------------------------------------------------------------------------------

data Game = Game {
      _g_playerPos ∷ V2 Int
    , _g_keepRunning ∷ Bool
    , _g_map ∷ String
    }

makeLenses ''Game

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

data Object = OuterWall
            | InnerWall
            | Floor
            | Spawn
            | Table
            | Chair
            | OpenedDoor
            | ClosedDoor
            deriving (Eq, Show)

asciiTable = [ ('═', OuterWall)
             , ('║', OuterWall)
             , ('╔', OuterWall)
             , ('╚', OuterWall)
             , ('╗', OuterWall)
             , ('╝', OuterWall)
             , ('─', InnerWall)
             , ('│', InnerWall)
             , ('┌', InnerWall)
             , ('└', InnerWall)
             , ('┐', InnerWall)
             , ('┘', InnerWall)
             , ('.', Floor)
             , ('╳', Spawn)
             , ('#', Table)
             , ('%', Chair)
             , ('/', OpenedDoor)
             , ('\\', OpenedDoor)
             , ('d', ClosedDoor)
             ]

--------------------------------------------------------------------------------

newtype DreamnetGameF a = DreamnetGameF { runDreamnetGame ∷ StateT Game IO a }
                        deriving (Functor, Applicative, Monad, MonadState Game, MonadIO)



dreamnet ∷ DesignData → IO ()
dreamnet d = do
    map ← readFile "res/map1"

    initScr
    raw    True
    echo   False
    keypad stdScr True
    cursSet CursorInvisible

    drawMap    map
    drawPlayer (V2 1 1)

    evalStateT (runDreamnetGame gameLoop) $ Game (V2 1 1) True map

    endWin


drawMap ∷ (MonadIO m) ⇒ String → m ()
drawMap = liftIO . mvWAddStr stdScr 0 0


drawPlayer ∷ (MonadIO m) ⇒ V2 Int → m ()
drawPlayer (V2 x y) = liftIO $ mvAddCh y x (fromIntegral $ ord '@')


gameLoop ∷ DreamnetGameF ()
gameLoop = do
    k ← liftIO $ getCh
    case k of
        KeyChar 'h' → g_playerPos += V2 -1  0
        KeyChar 'j' → g_playerPos += V2  0  1 
        KeyChar 'k' → g_playerPos += V2  0 -1 
        KeyChar 'l' → g_playerPos += V2  1  0 
        KeyChar 'y' → g_playerPos += V2 -1 -1
        KeyChar 'u' → g_playerPos += V2  1 -1 
        KeyChar 'b' → g_playerPos += V2 -1  1 
        KeyChar 'n' → g_playerPos += V2  1  1 

        KeyChar 'q' → g_keepRunning .= False

        _           → return ()

    map ← use g_map
    drawMap map

    pp  ← use g_playerPos
    drawPlayer pp

    liftIO $ refresh

    r ← use g_keepRunning
    when r gameLoop

