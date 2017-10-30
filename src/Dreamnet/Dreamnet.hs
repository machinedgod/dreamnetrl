{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Dreamnet
where

import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.State
import Data.Char  (ord)
import Data.List  (elemIndex, nub)
import Data.Maybe (fromMaybe)
import Linear

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import qualified Data.Set    as Set
import qualified Config.Dyre as Dyre

--------------------------------------------------------------------------------

data Map = Map {
      _m_width ∷ Word
    , _m_height ∷ Word
    , _m_data ∷ [Char]
    }

makeLenses ''Map


data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show)


data Game = Game {
      _g_playerPos ∷ V2 Int
    , _g_keepRunning ∷ Bool
    , _g_map ∷ Map
    , _g_visible ∷ [Visibility]
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


loadMap ∷ FilePath → IO Map
loadMap fp = do
    str ← readFile fp
    let w = fromMaybe 0 $ elemIndex '\n' str
        h = length str - w
    return $ Map (fromIntegral w) (fromIntegral h) (filter (/='\n') str)


dreamnet ∷ DesignData → IO ()
dreamnet d = do
    initCurses

    let pp = V2 1 1
    map ← loadMap "res/map1"
    let initGame = Game pp True map (initVisibility map pp)
    flip evalStateT initGame $ runDreamnetGame $ do
        drawInitial map
        gameLoop
    endWin
    where
        initCurses = do
            initScr
            raw        True
            echo       False
            keypad     stdScr True
            cursSet    CursorInvisible
            --startColor
        drawInitial m = do
            drawMap
            drawPlayer
            liftIO $ refresh
        initVisibility m v = replicate (squareSize m) Unknown 
        squareSize m = fromIntegral $ m^.m_width * m^.m_height


drawMap ∷ DreamnetGameF ()
drawMap = do
    m ← use g_map
    mapM_ (drawTile (m^.m_width)) $ zip [0..] (m^.m_data)
    where
        drawTile ∷ Word → (Int, Char) → DreamnetGameF ()
        drawTile w (i, c) = do
            vis ← use g_visible
            let (V2 x y) = coordLin i w
            liftIO $ case vis !! i of
                        Unknown → mvAddCh y x (fromIntegral $ ord ' ')
                        Known   → attrDimOn >> mvAddCh y x (fromIntegral $ ord c) >> attrDimOff
                        Visible → mvAddCh y x (fromIntegral $ ord c)


drawPlayer ∷ DreamnetGameF ()
drawPlayer = do
    (V2 x y) ← use g_playerPos
    liftIO $ mvAddCh y x (fromIntegral $ ord '@')


linCoord ∷ V2 Int → Word → Int
linCoord (V2 x y) w = y * (fromIntegral w) + x


coordLin ∷ Int → Word → V2 Int
coordLin i w = V2 (i `mod` (fromIntegral w)) (i `div` (fromIntegral w))


-- TODO so fucking shaky :-D
charAt ∷ Map → V2 Int → Char
charAt m v = (m^.m_data) !! linCoord v (m^.m_width)


changeObject ∷ V2 Int → Object → DreamnetGameF ()
changeObject v o = let c =  maybe '.' id $ lookup o $ flipTuple <$> asciiTable
                   in  changeMap v c
    where
        flipTuple (x, y) = (y, x)
        changeMap ∷ V2 Int → Char → DreamnetGameF ()
        changeMap v c = do
            w ← use (g_map.m_width)
            g_map.m_data %= (element (linCoord v w) .~ c)


movePlayer ∷ V2 Int → DreamnetGameF ()
movePlayer v = do
    npp ← uses g_playerPos (+v)
    c   ← uses g_map (`charAt` npp)
    when (maybe True isPassable $ lookup c asciiTable) $
        g_playerPos += v


debugPrint ∷ String → DreamnetGameF ()
debugPrint = liftIO . mvWAddStr stdScr 41 2


messagePrint ∷ String → DreamnetGameF ()
messagePrint = liftIO . mvWAddStr stdScr 40 2


openDoor ∷ DreamnetGameF ()
openDoor = do
    messagePrint "Which direction?"
    liftIO $ refresh

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
    o  ← (`lookup` asciiTable) <$> uses g_map (`charAt` dp)

    case o of
        (Just ClosedDoor) → changeObject dp OpenedDoor
        _ → return ()


closeDoor ∷ DreamnetGameF ()
closeDoor = do
    messagePrint "Which direction?"
    liftIO $ refresh

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
    o  ← (`lookup` asciiTable) <$> uses g_map (`charAt` dp)

    case o of
        (Just OpenedDoor) → changeObject dp ClosedDoor
        _ → return ()


updateVisible ∷ DreamnetGameF ()
updateVisible = do
    pp ← use g_playerPos
    m  ← use g_map

    let l         = replicate (fromIntegral $ (m^.m_width) * (m^.m_height)) Unknown
        points    = floodFillRange 10 pp -- This is pretty slow
        los       = filter (tileVisible m pp) points
        --los       = points
        linPoints = (`linCoord` (m^.m_width)) <$> los
        nl        = elements (`elem` linPoints) .~ Visible $ l

    g_visible %= mergeWith nl . fmap (\case
                                        Visible → Known
                                        Known   → Known
                                        Unknown → Unknown) 
    where
        mergeWith ∷ [Visibility] → [Visibility] → [Visibility]
        mergeWith l ov = fmap (vsum <$> fst <*> snd) $ zip l ov

        vsum ∷ Visibility → Visibility → Visibility
        vsum Unknown o = o
        vsum Visible _ = Visible
        vsum n       o = n


tileVisible ∷ Map → V2 Int → V2 Int → Bool
tileVisible m o d = let vis = fromMaybe False . fmap isPassable . (`lookup` asciiTable) . charAt m <$> bla o d
                        invis = dropWhile (==True) vis
                    in  null invis || length invis == 1


-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
-- Modified to stop at the second point
bla ∷ V2 Int -> V2 Int -> [V2 Int]
bla (V2 x0 y0) d@(V2 x1 y1) =
    let (V2 dx dy) = V2 (x1 - x0) (y1 - y0)
        xyStep b (V2 x y) = V2 (x + signum dx)     (y + signum dy * b)
        yxStep b (V2 x y) = V2 (x + signum dx * b) (y + signum dy)
        (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                     | otherwise       = (abs dx, abs dy, yxStep)
        walk w xy | xy == d   = xy : []
                  | otherwise = xy : walk (tail w) (step (head w) xy)
    in  walk (balancedWord p q 0) (V2 x0 y0)
    where
        balancedWord ∷ Int -> Int -> Int -> [Int]
        balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
        balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)


floodFillRange ∷ Word → V2 Int → [V2 Int]
floodFillRange d o = Set.toList $ snd $ execState nearestNeighbor (Set.singleton o, Set.empty)
    where
        nearestNeighbor ∷ State (Set.Set (V2 Int), Set.Set (V2 Int)) ()
        nearestNeighbor = do
            openSet   ← use _1
            closedSet ← use _2

            mapM_ (\x → do
                        _2 %= Set.insert x
                        _1 %= Set.filter ((&&) . inRange d o <*> not . (`Set.member` closedSet)) . Set.union (neighbors x)
                ) openSet
            when (not $ Set.null openSet)
                nearestNeighbor

        inRange ∷ Word → V2 Int → V2 Int → Bool
        inRange d o x = let tfv = fmap fromIntegral
                        in  abs (distance (tfv o) (tfv x)) < fromIntegral d

        neighbors ∷ V2 Int → Set.Set (V2 Int)
        neighbors p = Set.fromList
            [ p + V2 -1  0
            , p + V2  0  1
            , p + V2  0 -1
            , p + V2  1  0
            , p + V2 -1 -1
            , p + V2  1 -1
            , p + V2 -1  1
            , p + V2  1  1
            ]



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

    updateVisible
    drawMap
    drawPlayer
    liftIO $ refresh
    use g_keepRunning >>= (`when` gameLoop)

