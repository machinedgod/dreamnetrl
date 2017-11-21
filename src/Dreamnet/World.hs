{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.World
( module Dreamnet.TileMap
, module Control.Monad.Reader

, MonadWorld(..)
, WorldF
, World
, w_playerPos
, w_aim
, w_map
, w_visible
, w_status
, newWorld
, runWorld

, ObjectBox(..)
, objectAt

, Visibility(..)

, changeTile
, movePlayer
, updateAim
, moveAim
, interact
, updateVisible
) where

import Prelude hiding (interact)
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Linear
import Data.Bool
import Data.Maybe
import Data.List

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vec

import Dreamnet.TileMap
import Dreamnet.Input

--------------------------------------------------------------------------------

isPassable ∷ Tile → Bool
isPassable OuterWall  = False
isPassable InnerWall  = False
isPassable Floor      = True
isPassable MapSpawn   = True
isPassable Table      = False
isPassable Chair      = True
isPassable OpenedDoor = True
isPassable ClosedDoor = False
isPassable Computer   = True
isPassable Person     = False
{-# INLINE isPassable #-}


data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show, Ord, Enum)


data ObjectBox = BoxComputer
               | BoxPerson
               | BoxDoor      Bool
               deriving (Eq, Show)

data World = World {
      _w_playerPos ∷ V2 Int
    , _w_aim ∷ Maybe (V2 Int)
    , _w_map ∷ TileMap
    , _w_visible ∷ Vec.Vector Visibility

    , _w_status ∷ String

    , _w_objects ∷ Map.Map (V2 Int) ObjectBox
    }

makeLenses ''World


newWorld ∷ TileMap → World
newWorld m = let iniv    = Vec.replicate (squareSize m) Unknown 
                 pp      = V2 1 1
                 objects = let maybeObject c        = c `lookup` asciiTable >>= createObject
                               insertIntoMap i os o = Map.insert (coordLin m i) o os
                               findObjects i c os   = maybe os (insertIntoMap i os) (maybeObject c)
                           in  Vec.ifoldr findObjects Map.empty (m ^. m_data)
             in  World pp Nothing m iniv "" objects
    where
        squareSize m   = fromIntegral $ m^.m_width * m^.m_height

        createObject Person     = Just BoxPerson
        createObject Computer   = Just BoxComputer
        createObject OpenedDoor = Just (BoxDoor True)
        createObject ClosedDoor = Just (BoxDoor False)
        createObject _          = Nothing

--------------------------------------------------------------------------------

class (MonadState World u) ⇒ MonadWorld u where
    changeTileMap    ∷ V2 Int → Char → u ()
    playerInfo   ∷ String → u ()


newtype WorldF a = WorldF { runWorldF ∷ ReaderT Event (State World) a }
                 deriving (Functor, Applicative, Monad, MonadReader Event, MonadState World)


instance MonadWorld WorldF where
    changeTileMap v c = do
        m ← use w_map
        w_map.m_data %= (element (linCoord m v) .~ c)
    playerInfo s = w_status .= s
    

runWorld ∷ WorldF () → Event → World → World
runWorld wf e w = flip execState w $ flip runReaderT e $ runWorldF wf

--------------------------------------------------------------------------------

changeTile ∷ (MonadWorld u) ⇒ V2 Int → Tile → u ()
changeTile v o = let c =  maybe '.' id $ lookup o $ flipTuple <$> asciiTable
                 in  changeTileMap v c
    where
        flipTuple (x, y) = (y, x)


objectAt ∷ (MonadWorld u) ⇒ V2 Int → u (Maybe ObjectBox)
objectAt v = uses w_objects (Map.lookup v)


movePlayer ∷ (MonadWorld u) ⇒ V2 Int → u ()
movePlayer v = do
    npp ← uses w_playerPos (+v)
    t   ← uses w_map (`tileAt` npp)
    when (isPassable t) $
        w_playerPos += v


moveAim ∷ (MonadWorld u) ⇒ V2 Int → u ()
moveAim v = w_aim %= fmap (+v)


interact ∷ (MonadWorld u) ⇒ (V2 Int → Tile → u ()) → u ()
interact f = do
    mv ← use w_aim
    case mv of
        Just v → do
                 o ← uses w_map (`tileAt` v)
                 f v o
        _      → return ()



updateAim ∷ (MonadWorld u) ⇒ u ()
updateAim = do
    pp ← use w_playerPos
    m  ← use w_map
    let points = clipOutOfBounds $ floodFillRange 2 pp
    na ← foldM (\a x → maybe a (const $ Just x) <$> objectAt x) Nothing points
    w_aim .= na
    


updateVisible ∷ (MonadWorld u) ⇒ u ()
updateVisible = do
    pp ← use w_playerPos
    m  ← use w_map

    let points    = circle 20 pp
        los       = concat $ (fmap fst . visibleAndOneExtra . tileVisible m pp) <$> points
        linPoints = Set.fromList $ linCoord m <$> los

    -- TODO resolving 'x' causes problems
    w_visible %= Vec.imap (\i x → if i `Set.member` linPoints
                                       then Visible
                                       else case x of
                                                Visible → Known
                                                _       → x)
    where
        visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
        visibleAndOneExtra l = let front = takeWhile ((==True) . snd) l
                                   rem   = dropWhile ((==True) . snd) l
                               in  bool (head rem : front) front (null rem)



tileVisible ∷ TileMap → V2 Int → V2 Int → [(V2 Int, Bool)]
tileVisible m o d = let pass  = isPassable . tileAt m
                    in  fmap ((,) <$> id <*> pass) $ clipOutOfBounds $ bla o d


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

