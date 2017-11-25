{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.World
( module Dreamnet.Character

, MonadWorld(..)
, World
, WorldM
, w_playerPos
, w_aim
, w_map
, w_visible
, w_objects
, w_people
, w_status
, newWorld
, runWorld

, Visibility(..)
, updateVisible

, Object(..)
, objectAt
, changeObject
, changeObject_
, objectDescription
, objectInteraction
, objectToTile

, movePlayer
, switchAim
, interact
, interactOrElse
, examine
) where

import Prelude hiding (interact, head)
import Safe

import Control.Lens
import Control.Monad.State
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
import Dreamnet.GameState
import Dreamnet.Conversation

--------------------------------------------------------------------------------

class (MonadState World u) ⇒ MonadWorld u

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show, Ord, Enum)


-- TODO could this be a functor?
data Object = Computer
            | Person     String     -- <-- Name
            | Door       Bool       -- <-- Is opened?
            | Stairs     Bool       -- <-- Going up?
            | Prop       TMap.Tile
            deriving (Eq, Show, Ord)


data World = World {
      _w_playerPos ∷ V2 Int
    , _w_playerChar ∷ Character
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
                 objects = let extraData v    = fromMaybe Vec.empty $ Map.lookup v $ m ^. TMap.m_extra
                               gatherObject v = tileToObject (extraData v) v
                           in  Map.mapWithKey gatherObject (m^.TMap.m_objects)
                 people  = Map.fromList [ ("Moe", newCharacter "Moe")
                                        , ("Gary", newCharacter "Gary")
                                        ]
                 pp      = headNote "Map is missing spawn points!" $ m^.TMap.m_spawnPoints
                 pc      = newCharacter "Carla"
             in  World pp pc Nothing m iniv objects people ""
    where
        squareSize m   = fromIntegral $ m ^. TMap.m_width * m ^. TMap.m_height

--------------------------------------------------------------------------------

newtype WorldM a = WorldM { runWorldM ∷ State World a }
                 deriving (Functor, Applicative, Monad, MonadState World)

instance MonadWorld WorldM


runWorld ∷ WorldM GameState → World → (GameState, World)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

objectAt ∷ (MonadWorld u) ⇒ V2 Int → u (Maybe Object)
objectAt v = uses w_objects (Map.lookup v)


changeObject ∷ (MonadWorld u) ⇒ V2 Int → Object → u (Maybe Object)
changeObject v o = do
    oldo ← uses w_objects (Map.lookup v)
    w_objects %= Map.update (const $ Just o) v
    return oldo


changeObject_ ∷ (MonadWorld u) ⇒ V2 Int → Object → u ()
changeObject_ v = void . changeObject v


movePlayer ∷ (MonadWorld u) ⇒ V2 Int → u ()
movePlayer v = do
    npp ← uses w_playerPos (+v)
    objectPassable ← uses w_objects (maybe True isPassable . Map.lookup npp)
    tilePassable   ← uses w_map (isPassable . flip TMap.tileAt npp)
    when (objectPassable && tilePassable) $
        w_playerPos += v


interact ∷ (MonadWorld u) ⇒ (V2 Int → Object → u ()) → u ()
interact f = interactOrElse f (return ())


interactOrElse ∷ (MonadWorld u) ⇒ (V2 Int → Object → u a) → u a → u a
interactOrElse f e = fromMaybe e <=< runMaybeT $ do
    v ← MaybeT (use w_aim)
    o ← MaybeT (objectAt v)
    return (f v o)


examine ∷ (MonadWorld w) ⇒ w String
examine = interactOrElse (\_ → return . objectDescription) (use (w_map.TMap.m_desc))


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
    os ← use w_objects

    let points    = circle 20 pp
        los       = concat $ (fmap fst . visibleAndOneExtra . tileVisible m os pp) <$> points
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


tileVisible ∷ TMap.TileMap → Map.Map (V2 Int) Object → V2 Int → V2 Int → [(V2 Int, Bool)]
tileVisible m os o d = let seeThroughTile   = isSeeThrough . TMap.tileAt m
                           seeThroughObject = maybe True isSeeThrough . (`Map.lookup` os)
                       in  fmap ((,) <$> id <*> ((&&) <$> seeThroughTile <*> seeThroughObject)) $ filter (not . TMap.outOfBounds m) $ bla o d


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
        walk w xy | xy == d   = [xy]
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
            unless (Set.null openSet)
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

--------------------------------------------------------------------------------

class Collision a where
    isPassable ∷ a → Bool


instance Collision TMap.Tile where
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


instance Collision Object where
    isPassable Computer   = False
    isPassable (Person _) = False -- TODO maybe passable if its ally?
    isPassable (Door o)   = o
    isPassable (Stairs _) = True
    isPassable (Prop t)   = isPassable t
    {-# INLINE isPassable #-}

--------------------------------------------------------------------------------

class Vision a where
    isSeeThrough ∷ a → Bool

instance Vision TMap.Tile where
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


instance Vision Object where
    isSeeThrough Computer   = True
    isSeeThrough (Person _) = True
    isSeeThrough (Door o)   = o
    isSeeThrough (Stairs _) = True
    isSeeThrough (Prop t)   = isSeeThrough t

--------------------------------------------------------------------------------

tileToObject ∷ Vec.Vector String → V2 Int → TMap.Tile → Object
tileToObject extra v TMap.Person     = Person $ fromMaybe "!Redshirt!" $ extra Vec.!? 1
tileToObject extra v TMap.Computer   = Computer
tileToObject extra v TMap.OpenedDoor = Door True
tileToObject extra v TMap.ClosedDoor = Door False
tileToObject extra v TMap.StairsDown = Stairs False
tileToObject extra v TMap.StairsUp   = Stairs True
tileToObject extra v t               = Prop t
{-# INLINE tileToObject #-}


--------------------------------------------------------------------------------

objectToTile ∷ Object → TMap.Tile
objectToTile (Person _)    = TMap.Person
objectToTile Computer      = TMap.Computer
objectToTile (Door o)      = bool TMap.ClosedDoor TMap.OpenedDoor o
objectToTile (Stairs u)    = bool TMap.StairsUp   TMap.StairsDown u
objectToTile (Prop t)      = t
{-# INLINE objectToTile #-}


objectDescription ∷ Object → String
objectDescription Computer      = "This is a common machine found everywhere today. You wonder if its for better or worse."
objectDescription (Person c)    = c ++ " looks grumpy."
objectDescription (Door o)      = "Just a common door. They're " ++ bool "closed." "opened." o
objectDescription (Stairs t)    = "If map changing would've been coded in, you would use these to switch between maps and layers."
objectDescription (Prop t)      = "A common " ++ show t ++ ". You wonder if it fulfilled its existence."
{-# INLINE objectDescription #-}


objectInteraction ∷ (MonadWorld u) ⇒ V2 Int → Object → u GameState
objectInteraction v Computer      = w_status .= "Using a computer" >> return Normal
objectInteraction v (Person n)    = do
    mc ← uses w_people (Map.lookup n)
    case mc of
        Just c  → return $ Conversation testRecursiveConvo
        Nothing → w_status .= "You call out to " ++ n ++ ", but no one responds..." >> return Normal
objectInteraction v (Door o)      = changeObject_ v (Door (not o)) >> return Normal
objectInteraction v (Stairs t)    = w_status .= "These lead to " ++ show t >> return Normal
objectInteraction v (Prop t)      = propInteraction t
    where
        propInteraction ∷ (MonadWorld u) ⇒ TMap.Tile → u GameState
        propInteraction TMap.Table = w_status .= "Nothing to do with this table." >> return Normal
        propInteraction TMap.Chair = do
            w_status    .= "You sit down and chill out..."
            w_playerPos .= v 
            return Normal
        propInteraction _ = return Normal
{-# INLINE objectInteraction #-}

