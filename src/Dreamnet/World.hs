{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.World
( module Control.Monad.Reader
, module Dreamnet.Character

, MonadWorld(..)
, WorldF
, World
, w_playerPos
, w_playerState
, w_aim
, w_map
, w_visible
, w_status
, newWorld
, runWorld

, Visibility(..)
, updateVisible

, Object(..)
, objectAt
, changeObject
, changeObject_

, movePlayer
, switchAim
, interact
, examine
, talkTo
) where

import Prelude hiding (interact, head)
import Safe

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Linear
import Data.Bool
import Data.Maybe
import Data.List

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vec

import qualified Dreamnet.TileMap as TMap
import Dreamnet.Input
import Dreamnet.Character

--------------------------------------------------------------------------------

isPassable ∷ TMap.Tile → Bool
isPassable TMap.OuterWall  = False
isPassable TMap.InnerWall  = False
isPassable TMap.Floor      = True
isPassable TMap.MapSpawn   = True
isPassable TMap.Table      = False
isPassable TMap.Chair      = False
isPassable TMap.OpenedDoor = True
isPassable TMap.ClosedDoor = False
isPassable TMap.Computer   = False
isPassable TMap.Person     = False
isPassable TMap.Cupboard   = False
isPassable TMap.Sink       = False
isPassable TMap.Toilet     = False
isPassable TMap.StairsDown = True
isPassable TMap.StairsUp   = True
{-# INLINE isPassable #-}


isSeeThrough ∷ TMap.Tile → Bool
isSeeThrough TMap.OuterWall  = False
isSeeThrough TMap.InnerWall  = False
isSeeThrough TMap.Floor      = True
isSeeThrough TMap.MapSpawn   = True
isSeeThrough TMap.Table      = True
isSeeThrough TMap.Chair      = True
isSeeThrough TMap.OpenedDoor = True
isSeeThrough TMap.ClosedDoor = False
isSeeThrough TMap.Computer   = True
isSeeThrough TMap.Person     = True
isSeeThrough TMap.Cupboard   = False
isSeeThrough TMap.Sink       = True
isSeeThrough TMap.Toilet     = True
isSeeThrough TMap.StairsDown = True
isSeeThrough TMap.StairsUp   = True
{-# INLINE isSeeThrough #-}


data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show, Ord, Enum)


data Object = Computer
            | Person     String     -- <-- Name
            | Door       Bool       -- <-- Is opened?
            | Container  TMap.Tile
            | Dispenser  TMap.Tile
            | Stairs     TMap.Tile
            | Prop       TMap.Tile
            deriving (Eq, Show)


data World = World {
      _w_playerPos ∷ V2 Int
    , _w_playerChar ∷ Character
    , _w_playerState ∷ CharacterState
    , _w_aim ∷ Maybe (V2 Int)

    , _w_map ∷ TMap.TileMap
    , _w_visible ∷ Vec.Vector Visibility

    , _w_objects ∷ Map.Map (V2 Int) Object
    , _w_people ∷ Map.Map String Character
    , _w_status ∷ String
    }

makeLenses ''World


newWorld ∷ TMap.TileMap → World
newWorld m = let iniv    = Vec.replicate (squareSize m) Unknown 
                 objects = TMap.findObjects m tileToObject
                 people  = Map.fromList [ ("Moe", newCharacter "Moe")
                                        , ("Gary", newCharacter "Gary")
                                        ]
                 pp      = headNote "Map is missing spawn points!" $ TMap.findSpawnPoints m
                 pc      = newCharacter "Carla"
             in  World pp pc Normal Nothing m iniv objects people ""
    where
        squareSize m   = fromIntegral $ m ^. TMap.m_width * m ^. TMap.m_height

--------------------------------------------------------------------------------

class (MonadState World u) ⇒ MonadWorld u


newtype WorldF a = WorldF { runWorldF ∷ ReaderT Event (State World) a }
                 deriving (Functor, Applicative, Monad, MonadReader Event, MonadState World)

instance MonadWorld WorldF


runWorld ∷ WorldF () → Event → World → (World)
runWorld wf e w = flip execState w $ flip runReaderT e $ runWorldF wf

--------------------------------------------------------------------------------

tileToObject ∷ Vec.Vector String → V2 Int → TMap.Tile → Maybe Object
tileToObject extra v TMap.Person     = Just (Person $ fromMaybe "!Redshirt!" $ extra Vec.!? 1)
tileToObject extra v TMap.Computer   = Just Computer
tileToObject extra v TMap.OpenedDoor = Just (Door True)
tileToObject extra v TMap.ClosedDoor = Just (Door False)
tileToObject extra v TMap.Cupboard   = Just (Container TMap.Cupboard)
tileToObject extra v TMap.Sink       = Just (Dispenser TMap.Sink)
tileToObject extra v TMap.Toilet     = Just (Container TMap.Toilet)
tileToObject extra v TMap.StairsDown = Just (Stairs TMap.StairsDown)
tileToObject extra v TMap.StairsUp   = Just (Stairs TMap.StairsDown)
tileToObject extra v TMap.Table      = Just (Prop TMap.Table)
tileToObject extra v TMap.Chair      = Just (Prop TMap.Chair)
tileToObject extra v _               = Nothing


objectToTile ∷ Object → TMap.Tile
objectToTile (Person _)    = TMap.Person
objectToTile Computer      = TMap.Computer
objectToTile (Door o)      = bool TMap.ClosedDoor TMap.OpenedDoor o
objectToTile (Container t) = t
objectToTile (Dispenser t) = t
objectToTile (Stairs t)    = t
objectToTile (Prop t)      = t


objectAt ∷ (MonadWorld u) ⇒ V2 Int → u (Maybe Object)
objectAt v = uses w_objects (Map.lookup v)


changeObject ∷ (MonadWorld u) ⇒ V2 Int → Object → u (Maybe Object)
changeObject v o = do
    oldo ← uses w_objects (Map.lookup v)
    w_objects %= Map.update (const $ Just o) v
    w_map %= TMap.changeTile v (objectToTile o)
    return oldo


changeObject_ ∷ (MonadWorld u) ⇒ V2 Int → Object → u ()
changeObject_ v = void . changeObject v


movePlayer ∷ (MonadWorld u) ⇒ V2 Int → u ()
movePlayer v = do
    npp ← uses w_playerPos (+v)
    t   ← uses w_map (`TMap.tileAt` npp)
    when (isPassable t) $
        w_playerPos += v


interact ∷ (MonadWorld u) ⇒ (V2 Int → Object → u ()) → u ()
interact f = void $ runMaybeT $ do
    v ← MaybeT (use w_aim)
    o ← MaybeT (objectAt v)
    MaybeT (Just <$> f v o)


examine ∷ (MonadWorld u) ⇒ String → u ()
examine s = w_playerState .= Examination s


talkTo ∷ (MonadWorld u) ⇒ String → u ()
talkTo n = do
    mc ← uses w_people (Map.lookup n)
    case mc of
        Just c  → w_playerState .= Conversation c
        Nothing → w_status .= "You call out to " ++ n ++ ", but no one responds..."


interestingObjects ∷ (MonadWorld u) ⇒ u [V2 Int]
interestingObjects = do
    pp ← use w_playerPos
    m  ← use w_map
    let points = filter (not . TMap.outOfBounds m) $ floodFillRange 2 pp
    foldM (\l x → maybe l (const $ x:l) <$> objectAt x) [] points


switchAim ∷ (MonadWorld u) ⇒ u ()
switchAim = do
    os ← interestingObjects
    ca ← use w_aim
    case ca of
        Just a → w_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
        _      → w_aim .= headMay os


updateVisible ∷ (MonadWorld u) ⇒ u ()
updateVisible = do
    pp ← use w_playerPos
    m  ← use w_map

    let points    = circle 20 pp
        los       = concat $ (fmap fst . visibleAndOneExtra . tileVisible m pp) <$> points
        linPoints = Set.fromList $ TMap.linCoord m <$> los

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



tileVisible ∷ TMap.TileMap → V2 Int → V2 Int → [(V2 Int, Bool)]
tileVisible m o d = let see = isSeeThrough . TMap.tileAt m
                    in  fmap ((,) <$> id <*> see) $ filter (not . TMap.outOfBounds m) $ bla o d


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
        neighbors p = Set.map (+p) $ Set.fromList [ V2 -1 -1
                                                  , V2  0 -1
                                                  , V2  1 -1
                                                  , V2 -1  0
                                                  , V2  1  0
                                                  , V2 -1  1
                                                  , V2  0  1
                                                  , V2  1  1
                                                  ]

