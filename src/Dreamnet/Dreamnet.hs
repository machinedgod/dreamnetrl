{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Dreamnet.Dreamnet
where

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

import UI.NCurses
import qualified Config.Dyre as Dyre

--------------------------------------------------------------------------------

data Map = Map {
      _m_width ∷ Word
    , _m_height ∷ Word
    , _m_data ∷ Vec.Vector Char
    }

makeLenses ''Map


data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show, Ord, Enum)


data Game = Game {
      _g_playerPos ∷ V2 Int
    , _g_keepRunning ∷ Bool
    , _g_map ∷ Map
    , _g_visible ∷ Vec.Vector Visibility
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
{-# INLINE isPassable #-}

--------------------------------------------------------------------------------

type DreamnetGameF = StateT Game Curses


loadMap ∷ (MonadIO m) ⇒ FilePath → m Map
loadMap fp = do
    str ← liftIO $ readFile fp
    let w = fromMaybe 0 $ elemIndex '\n' str
        h = length str - w
    return $ Map (fromIntegral w) (fromIntegral h) (Vec.fromList $ filter (/='\n') str)


dreamnet ∷ DesignData → IO ()
dreamnet d = do
    m ← loadMap "res/map1"
    runCurses (dreamnetCurses m)
    where
        dreamnetCurses map = do
            initCurses
            let pp = V2 1 1
            let iniv = Vec.replicate (squareSize map) Unknown 
            let initGame = Game pp True map iniv
            void $ flip execStateT initGame $ do
                updateVisible
                drawInitial map
                gameLoop
        initCurses = do
            setRaw     True
            setEcho    False
            defaultWindow >>= (`setKeypad` True)
            setCursorMode CursorInvisible
        drawInitial m = do
            drawMap
            drawPlayer
            lift render
        squareSize m = fromIntegral $ m^.m_width * m^.m_height


drawMap ∷ DreamnetGameF ()
drawMap = do
    m   ← use g_map
    vis ← use g_visible
    Vec.imapM_ (drawTile (m^.m_width)) $ Vec.zip (m^.m_data) vis
    where
        drawTile ∷ Word → Int → (Char, Visibility) → DreamnetGameF ()
        drawTile w i (c, v) = do
            let (V2 x y) = coordLin i w
            uncurry (drawCharAt x y) $ case v of
                 Unknown → (' ', [])
                 Known   → (c,   [AttributeDim])
                 Visible → (c,   [])


drawCharAt ∷ (Integral a) ⇒ a → a → Char → [Attribute] → DreamnetGameF ()
drawCharAt x y c s = lift $ defaultWindow >>= \w → updateWindow w $ do
    moveCursor (fromIntegral y) (fromIntegral x)
    drawGlyph (Glyph c s)


drawStringAt ∷ (Integral a) ⇒ a → a → String → DreamnetGameF ()
drawStringAt x y s = lift $ defaultWindow >>= \w → updateWindow w $ do
    moveCursor (fromIntegral y) (fromIntegral x)
    drawString s


drawPlayer ∷ DreamnetGameF ()
drawPlayer = do
    (V2 x y) ← use g_playerPos
    drawCharAt x y '@' []


linCoord ∷ V2 Int → Word → Int
linCoord (V2 x y) w = y * (fromIntegral w) + x
{-# INLINE linCoord #-}


coordLin ∷ Int → Word → V2 Int
coordLin i w = V2 (i `mod` (fromIntegral w)) (i `div` (fromIntegral w))
{-# INLINE coordLin #-}


-- TODO so fucking shaky :-D
charAt ∷ Map → V2 Int → Char
charAt m v = (m^.m_data) Vec.! linCoord v (m^.m_width)


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
debugPrint = drawStringAt 41 2


messagePrint ∷ String → DreamnetGameF ()
messagePrint = drawStringAt 40 2


eventToDir ∷ Event → Maybe (V2 Int)
eventToDir (EventCharacter 'h') = Just (V2 -1  0)
eventToDir (EventCharacter 'j') = Just (V2  0  1)
eventToDir (EventCharacter 'k') = Just (V2  0 -1)
eventToDir (EventCharacter 'l') = Just (V2  1  0)
eventToDir (EventCharacter 'y') = Just (V2 -1 -1)
eventToDir (EventCharacter 'u') = Just (V2  1 -1)
eventToDir (EventCharacter 'b') = Just (V2 -1  1)
eventToDir (EventCharacter 'n') = Just (V2  1  1)
eventToDir _                    = Nothing


directionalInteract ∷ (V2 Int → Maybe Object → DreamnetGameF ()) → DreamnetGameF ()
directionalInteract f = do
    messagePrint "Direction?"
    lift render

    mk ← lift (defaultWindow >>= \w → getEvent w Nothing)
    case mk of
        Nothing → return ()
        Just k  → case eventToDir k of
                      Nothing → return ()
                      Just v  → do
                          dp ← uses g_playerPos (+v)
                          o  ← (`lookup` asciiTable) <$> uses g_map (`charAt` dp)
                          f dp o


updateVisible ∷ DreamnetGameF ()
updateVisible = do
    pp ← use g_playerPos
    m  ← use g_map

    let points    = circle 20 pp
        los       = concat $ (fmap fst . visibleAndOneExtra . tileVisible m pp) <$> points
        linPoints = Set.fromList $ (`linCoord` (m^.m_width)) <$> los

    -- TODO resolving 'x' causes problems
    g_visible %= Vec.imap (\i x → if i `Set.member` linPoints
                                       then Visible
                                       --else Known)
                                       else case x of
                                                Visible → Known
                                                _       → x)
                                                --Known   → Known
                                                --Unknown → Unknown)
    where
        visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
        visibleAndOneExtra l = let front = takeWhile ((==True) . snd) l
                                   rem   = dropWhile ((==True) . snd) l
                               in  bool (head rem : front) front (null rem)


tileVisible ∷ Map → V2 Int → V2 Int → [(V2 Int, Bool)]
tileVisible m o d = let pass  = fromMaybe False . fmap isPassable . (`lookup` asciiTable) . charAt m
                        rmOOB = filter (\(V2 x y) → x >= 0 && y >= 0)
                    in  fmap ((,) <$> id <*> pass) $ rmOOB $ bla o d


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


-- Takes the center of the circle and radius, and returns the circle points
circle ∷ Int → V2 Int → [V2 Int]
circle radius (V2 x0 y0) = uncurry V2 <$> iniPoints
    where
        -- Four initial points, plus the generated points
        iniPoints = (x0, y0 + radius) : (x0, y0 - radius) : (x0 + radius, y0) : (x0 - radius, y0) : points
        -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
        points = concatMap generatePoints $ unfoldr step initialValues
        generatePoints (x, y)
          = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
 
        -- The initial values for the loop
        initialValues = (1 - radius, 1, (-2) * radius, 0, radius)
 
        -- One step of the loop. The loop itself stops at Nothing.
        step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                     | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                       where
                                           (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                            | otherwise = (f + ddf_x, ddf_y, y)
                                           ddf_x' = ddf_x + 2
                                           x' = x + 1


floodFillRange ∷ Word → V2 Int → [V2 Int]
floodFillRange r o = Set.toList $ snd $ execState nearestNeighbor (Set.singleton o, Set.empty)
    where
        nearestNeighbor ∷ State (Set.Set (V2 Int), Set.Set (V2 Int)) ()
        nearestNeighbor = do
            openSet   ← use _1
            closedSet ← use _2

            mapM_ (\x → do
                        _2 %= Set.insert x
                        _1 %= Set.filter ((&&) . inRange r o <*> not . (`Set.member` closedSet)) . Set.union (neighbors x)
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
    k ← lift (defaultWindow >>= \w → getEvent w Nothing)
    case k of
        (Just (EventCharacter 'o')) → directionalInteract $ \v o → case o of
            Just ClosedDoor → changeObject v OpenedDoor
            _               → return ()
        (Just (EventCharacter 'c')) → directionalInteract $ \v o → case o of
            Just OpenedDoor → changeObject v ClosedDoor
            _               → return ()
        (Just (EventCharacter 'q')) → g_keepRunning .= False

        (Just e) → case eventToDir e of
                       Just v → movePlayer v
                       Nothing → return ()
        _        → return ()

    updateVisible
    drawMap
    drawPlayer
    lift render
    use g_keepRunning >>= (`when` gameLoop)

