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
             , ('\'', OpenedDoor)
             , ('+', ClosedDoor)
             ]


isPassable ∷ Object → Bool
isPassable OuterWall  = False
isPassable InnerWall  = False
isPassable Floor      = True
isPassable Spawn      = True
isPassable Table      = False
isPassable Chair      = True
isPassable OpenedDoor = True
isPassable ClosedDoor = False

--------------------------------------------------------------------------------

newtype DreamnetGameF a = DreamnetGameF { runDreamnetGame ∷ StateT Game IO a }
                        deriving (Functor, Applicative, Monad, MonadState Game, MonadIO)



dreamnet ∷ DesignData → IO ()
dreamnet d = do
    map ← readFile "res/map1"
    initCurses
    flip evalStateT (Game (V2 1 1) True map) $ runDreamnetGame $ do
        drawInitial map
        gameLoop
    endWin
    where
        initCurses = do
            initScr
            raw    True
            echo   False
            keypad stdScr True
            cursSet CursorInvisible
        drawInitial m = do
            drawMap m
            drawPlayer (V2 1 1)
            liftIO $ refresh


drawMap ∷ String → DreamnetGameF ()
drawMap = liftIO . mvWAddStr stdScr 0 0


drawPlayer ∷ V2 Int → DreamnetGameF ()
drawPlayer (V2 x y) = liftIO $ mvAddCh y x (fromIntegral $ ord '@')


linearMapCoord ∷ V2 Int → Int
linearMapCoord (V2 x y) = y * 81 + x


-- TODO so fucking shaky :-D
charAt ∷ V2 Int → DreamnetGameF Char
charAt v = uses g_map (!! linearMapCoord v)


changeObject ∷ V2 Int → Object → DreamnetGameF ()
changeObject v o = let c =  maybe '.' id $ lookup o $ flipTuple <$> asciiTable
                   in  changeMap v c
    where
        flipTuple (x, y) = (y, x)
        changeMap ∷ V2 Int → Char → DreamnetGameF ()
        changeMap v c = g_map %= (element (linearMapCoord v) .~ c)


movePlayer ∷ V2 Int → DreamnetGameF ()
movePlayer v = do
    npp ← uses g_playerPos (+v)
    c   ← charAt npp
    when (maybe True isPassable $ lookup c asciiTable) $
        g_playerPos += v


debugPrint ∷ String → DreamnetGameF ()
debugPrint = liftIO . mvWAddStr stdScr 41 2


messagePrint ∷ String → DreamnetGameF ()
messagePrint = liftIO . mvWAddStr stdScr 40 2


openDoor ∷ DreamnetGameF ()
openDoor = do
    messagePrint "Which direction?"
    k ← liftIO $ getCh
    let v = case k of
                KeyChar 'h' → V2 -1  0
                KeyChar 'j' → V2  0  1
                KeyChar 'k' → V2  0 -1
                KeyChar 'l' → V2  1  0
                KeyChar 'y' → V2 -1 -1
                KeyChar 'u' → V2  1 -1
                KeyChar 'b' → V2 -1  1
                KeyChar 'n' → V2  1  1

    dp ← uses g_playerPos (+v)
    o  ← (`lookup` asciiTable) <$> charAt dp

    case o of
        (Just ClosedDoor) → changeObject dp OpenedDoor
        _ → return ()


closeDoor ∷ DreamnetGameF ()
closeDoor = do
    messagePrint "Which direction?"
    k ← liftIO $ getCh
    let v = case k of
                KeyChar 'h' → V2 -1  0
                KeyChar 'j' → V2  0  1
                KeyChar 'k' → V2  0 -1
                KeyChar 'l' → V2  1  0
                KeyChar 'y' → V2 -1 -1
                KeyChar 'u' → V2  1 -1
                KeyChar 'b' → V2 -1  1
                KeyChar 'n' → V2  1  1

    dp ← uses g_playerPos (+v)
    o  ← (`lookup` asciiTable) <$> charAt dp

    case o of
        (Just OpenedDoor) → changeObject dp ClosedDoor
        _ → return ()


gameLoop ∷ DreamnetGameF ()
gameLoop = do
    k ← liftIO $ getCh
    case k of
        KeyChar 'h' → movePlayer (V2 -1  0)
        KeyChar 'j' → movePlayer (V2  0  1) 
        KeyChar 'k' → movePlayer (V2  0 -1) 
        KeyChar 'l' → movePlayer (V2  1  0) 
        KeyChar 'y' → movePlayer (V2 -1 -1)
        KeyChar 'u' → movePlayer (V2  1 -1) 
        KeyChar 'b' → movePlayer (V2 -1  1) 
        KeyChar 'n' → movePlayer (V2  1  1) 

        KeyChar 'o' → openDoor
        KeyChar 'c' → closeDoor

        KeyChar 'q' → g_keepRunning .= False

        _           → return ()


    use g_map >>= drawMap
    use g_playerPos >>= drawPlayer
    liftIO $ refresh
    use g_keepRunning >>= (`when` gameLoop)

