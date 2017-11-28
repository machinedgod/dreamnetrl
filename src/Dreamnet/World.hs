{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.World
( module Dreamnet.Character

, MonadWorld(..)
, World
, WorldM
, w_playerPos
, w_playerCharacter
, w_aim
, w_map
, w_visible
, w_objects
, w_items
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

, movePlayer
, switchAim
, interact
, interactOrElse
, examine
, get
) where

import Prelude hiding (interact)
import Safe

import Control.Lens
import Control.Monad.State hiding (get)
import Control.Monad.Trans.Maybe
import Linear
import Data.Bool
import Data.Maybe
import Data.List (intercalate, unfoldr)
import Data.Char (toLower)

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vec

import qualified Dreamnet.TileMap as TMap
import Dreamnet.Input
import Dreamnet.Item
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
            | Prop       String  Bool  Bool  Char  String  -- <-- Name, passable, seeThrough, Char, material
            deriving (Eq, Show, Ord)


data World = World {
      _w_playerPos ∷ V2 Int
    , _w_playerCharacter ∷ Character
    , _w_aim ∷ Maybe (V2 Int)

    , _w_map ∷ TMap.TileMap
    , _w_visible ∷ Vec.Vector Visibility

    , _w_objects ∷ Map.Map (V2 Int) Object -- TODO unless this is a list and I have some stacking support, I won't be able to have eg. a computer sitting atop a table, or a PERSON walking through a DOOR (person would overwrite the door object!)
    , _w_items ∷ Map.Map (V2 Int) [Item]
    , _w_people ∷ Map.Map String Character
    , _w_status ∷ String
    }

makeLenses ''World


newWorld ∷ TMap.TileMap → World
newWorld m = let iniv    = Vec.replicate (squareSize m) Unknown 
                 objects = Map.map tupleToObject (m^.TMap.m_extra)
                 items   = Map.fromList [ (V2 14 7, [ Item "Beer bottle", Item "Whiskey glass", Item "Shot glass" ])
                                        , (V2 10 5, [ Item "Credit scanner" ])
                                        ]
                 people  = Map.fromList [ ("Moe",    newCharacter "Moe"    moeConvo)
                                        , ("Gary",   newCharacter "Gary"   garryConvo)
                                        , ("Johnny", newCharacter "Johnny" johnnyConvo)
                                        , ("Sally",  newCharacter "Sally"  sallyConvo)
                                        ]
                 pp      = headNote "Map is missing spawn points!" $ m^.TMap.m_spawnPoints
                 pc      = newCharacter "Carla" End
             in  World pp pc Nothing m iniv objects items people ""
    where
        squareSize m   = fromIntegral $ m ^. TMap.m_width * m ^. TMap.m_height
        tupleToObject (_, _, "Computer", _, _, _) = Computer
        tupleToObject (_, _, "Person", n, _, _)   = Person n
        tupleToObject (_, _, "Door", o, _, _)     = Door (readDef False o)
        tupleToObject (_, _, "Stairs", u, _, _)   = Stairs (readDef False u)
        tupleToObject (c, m, "Prop", n, p, s)     = Prop n (readDef False p) (readDef False s) c m
        tupleToObject t                           = error $ "Unknown object definition: " ++ show t
        {-# INLINE tupleToObject #-}

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


interact ∷ (MonadWorld w) ⇒ (V2 Int → Object → w ()) → w ()
interact f = interactOrElse f (return ())


interactOrElse ∷ (MonadWorld u) ⇒ (V2 Int → Object → u a) → u a → u a
interactOrElse f e = fromMaybe e <=< runMaybeT $ do
    v ← MaybeT (use w_aim)
    o ← MaybeT (objectAt v)
    return (f v o)


examine ∷ (MonadWorld w) ⇒ w String
examine = interactOrElse examineText (use (w_map.TMap.m_desc))
    where
        examineText v o    = (objectDescription o ++) <$> itemsText v
        itemsText v        = maybe "" itemsDescription <$> uses w_items (Map.lookup v)
        itemsDescription []  = ""
        itemsDescription [i] = "\nThere's a " ++ (toLower <$> i^.i_name) ++ " here."
        itemsDescription l   = "\nThere are " ++ itemListToText l ++ " here."
        itemListToText l     = let sl = fmap toLower . view i_name <$> l
                               in  intercalate ", " (take (length sl - 1) sl) ++ " and " ++ last sl


get ∷ (MonadWorld w) ⇒ w (Maybe Item)
get = interactOrElse getItem (return Nothing)
    where
        getItem v o = runMaybeT $ do
            i ← MaybeT (uses w_items (Map.lookup v >=> headMay))
            addToCharacterInventory i
            lift (removeFromWorldPile v i)
            return i
        addToCharacterInventory i = w_playerCharacter.ch_inventory %= (i:)


removeFromWorldPile ∷ (MonadWorld w) ⇒ V2 Int → Item → w ()
removeFromWorldPile v i = w_items %= Map.update (wrapMaybe . removeFromList i) v
    where
        removeFromList i l = filter (/=i) l
        wrapMaybe [] = Nothing
        wrapMaybe l  = Just l


interestingObjects ∷ (MonadWorld u) ⇒ u [V2 Int]
interestingObjects = do
    pp ← use w_playerPos
    m  ← use w_map
    let points = filter (not . TMap.outOfBounds m) $ floodFillRange 2 pp
    foldM collectObjects [] points
    where
        collectObjects l x = maybe l (const $ x:l) <$> objectAt x


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
    {-# INLINE isPassable #-}


instance Collision Object where
    isPassable Computer         = False
    isPassable (Person _)       = False -- TODO maybe passable if its ally?
    isPassable (Door o)         = o
    isPassable (Stairs _)       = True
    isPassable (Prop _ p _ _ _) = p
    {-# INLINE isPassable #-}

--------------------------------------------------------------------------------

class Vision a where
    isSeeThrough ∷ a → Bool

instance Vision TMap.Tile where
    isSeeThrough TMap.OuterWall  = False
    isSeeThrough TMap.InnerWall  = False
    isSeeThrough TMap.Floor      = True
    {-# INLINE isSeeThrough #-}


instance Vision Object where
    isSeeThrough Computer         = True
    isSeeThrough (Person _)       = True
    isSeeThrough (Door o)         = o
    isSeeThrough (Stairs _)       = True
    isSeeThrough (Prop _ _ s _ _) = s

--------------------------------------------------------------------------------

objectDescription ∷ Object → String
objectDescription Computer         = "This is a common machine found everywhere today. You wonder if its for better or worse."
objectDescription (Person c)       = c ++ " looks grumpy."
objectDescription (Door o)         = "Just a common door. They're " ++ bool "closed." "opened." o
objectDescription (Stairs t)       = "If map changing would've been coded in, you would use these to switch between maps and layers."
objectDescription (Prop t _ _ _ _) = "A common " ++ t ++ "."
{-# INLINE objectDescription #-}


objectInteraction ∷ (MonadWorld u) ⇒ V2 Int → Object → u GameState
objectInteraction v Computer      = w_status .= "Using a computer" >> return Normal
objectInteraction v (Person n)    = do
    mc ← uses w_people (Map.lookup n)
    case mc of
        Just c  → return $ Conversation (c^.ch_name) (c^.ch_conversation)
        Nothing → w_status .= "You call out to " ++ n ++ ", but no one responds..." >> return Normal
objectInteraction v (Door o)         = changeObject_ v (Door (not o)) >> return Normal
objectInteraction v (Stairs t)       = w_status .= "These lead to " ++ show t >> return Normal
objectInteraction v (Prop n _ _ _ _) = w_status .= "You have no idea what to do with this " ++ n >> return Normal
{-# INLINE objectInteraction #-}

