{-# LANGUAGE UnicodeSyntax, NegativeLiterals, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.Engine.WorldMap
( RangeError(..), OobError(..), Safe(unpack), unpacked

, WorldMap, wm_size, wm_base, wm_data, wm_desc, wm_spawns

, newWorldMap, fromTileMap, coordToIndex, indexToCoord, checkBounds
, checkBounds', clipToBounds, clipToBounds', baseAt, cellAt, column
, interestingObjects, castRay

, WorldMapM, runWorldMap, evalWorldMap, execWorldMap

, modifyCell
)
where


import Prelude hiding         (head, tail)
import Control.Lens           (Getter, makeLenses, view, views, use, uses, _1,
                               _2, _3)
import Control.Lens.Operators
import Control.Monad.ST       (ST)
import Control.Monad.State    (State, MonadState, runState, evalState, execState,
                               get, gets)
import Data.Bifunctor         (Bifunctor(bimap))
import Data.Monoid            ((<>))
import Data.Maybe             (catMaybes)
import Data.Either            (rights)
import Data.Bool              (bool)
import Data.List.NonEmpty     (head, tail)
import Numeric.Natural        (Natural)
import Linear                 (V2(V2), V3(V3), _x, _y, _z, _xy)

import qualified Data.Vector         as V  (Vector, (!), replicate, fromList,
                                            modify, concat, length)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Visibility hiding (height)
import qualified Dreamnet.Engine.Visibility as Visibility
import Dreamnet.Engine.Utils
import Dreamnet.Engine.TileMap

--------------------------------------------------------------------------------

data RangeError =
      Underflow
    | Overflow
    deriving (Show)


data OobError =
      WidthBound  RangeError
    | HeightBound RangeError
    | DepthBound  RangeError
    deriving (Show)


newtype Safe a = Safe { unpack ∷ a }
               deriving (Eq, Show)


unpacked ∷ Getter (Safe a) a
unpacked f s = Safe <$> f (unpack s)


-- | Type variables
--   b: base layer data
--   o: gameplay data
data WorldMap b o = WorldMap {
      _wm_size   ∷ (Width, Height, Depth)
    , _wm_base   ∷ V.Vector b
    , _wm_data   ∷ V.Vector o
    , _wm_desc   ∷ String
    , _wm_spawns ∷ V.Vector (Safe (V3 Int))
    }
    deriving (Show)
makeLenses ''WorldMap


instance Bifunctor WorldMap where
    bimap f g = (wm_data %~ fmap g) . (wm_base %~ fmap f)
        

instance CoordVector (WorldMap b o) where
    width  = view (wm_size._1)
    height = view (wm_size._2)

--------------------------------------------------------------------------------

newWorldMap ∷ Width → Height → Depth → b → o →  WorldMap b o
newWorldMap w h d b x =
    WorldMap {
      _wm_size   = (w, h, d)
    , _wm_base   = V.replicate (fromIntegral (squared w h)) b
    , _wm_data   = V.replicate (fromIntegral (cubed w h d)) x
    , _wm_desc   = "Debug generated map!"
    , _wm_spawns = V.fromList [Safe (V3 0 0 0)]
    }


fromTileMap ∷ (Eq o) ⇒ (Tile → Either String b) → (Tile → Either String o) → TileMap → Either String (WorldMap b o)
fromTileMap t2b t2o tm = 
    let baseLayer  = views m_layers head tm
        dataLayers = views m_layers tail tm
        depth      = views m_layers (subtract 1 . fromIntegral . length)
    in  do
        base           ← layerToVector (tm ^. m_tileset) t2b baseLayer
        layeredObjects ← traverse (layerToVector (tm ^. m_tileset) t2o) dataLayers
        pure $ WorldMap
            { _wm_size   = (width tm, height tm, depth tm)
            , _wm_base   = base
            , _wm_data   = V.concat layeredObjects
            , _wm_desc   = tm ^. m_desc
            , _wm_spawns = positionsOfX baseLayer -- TODO this has to be proofed for the future a bit
            }
    where
        positionsOfX bl = let toSpawnPos = Safe . (V3 <$> view _x <*> view _y <*> const 0)
                          in  toSpawnPos <$> V.fromList (findAll '╳' bl)

        --maybeObjects  i = coordLin tm i `M.lookup` (tm^.m_positioned)

        --addPositioned i o = maybe (Right o) (fmap ((o<>) . Cell) . traverse t2o) (maybeObjects i)

        layerToVector ∷ Tileset → (Tile → Either String a) → TileLayer → Either String (V.Vector a)
        layerToVector ts fo = traverse (charToObject ts fo) . view l_data

        charToObject ∷ Tileset → (Tile → Either String a) → Char → Either String a
        charToObject ts fo c =
            let maybeObj = c `M.lookup` ts
            in  fo =<< maybeToEither ("Char " <> [c] <> " doesn't exist in the tileset!") maybeObj


coordToIndex ∷ WorldMap b o → Safe (V3 Int) → Safe Int
coordToIndex m (unpack → V3 x y z) = Safe (x + fromIntegral (width m) * (y + fromIntegral (height m) * z))


indexToCoord ∷ WorldMap b o → Safe Int → Safe (V3 Int)
indexToCoord (view wm_size → (w, h, d)) (Safe i) =
    let area = fromIntegral (squared w h)
        z = i `div` area
        y = (i `mod` area) `div` fromIntegral w
        x = (i `mod` area) `mod` fromIntegral w
    in  Safe (V3 x y z)


checkBounds ∷ WorldMap b o → V3 Int → Either OobError (Safe (V3 Int))
checkBounds (view wm_size → (w, h, d)) v@(V3 x y z)
    | x <  0              = Left (WidthBound Underflow)
    | x >= fromIntegral w = Left (WidthBound Overflow)
    | y <  0              = Left (HeightBound Underflow)
    | y >= fromIntegral h = Left (HeightBound Overflow)
    | z <  0              = Left (DepthBound Underflow)
    | z >= fromIntegral d = Left (DepthBound Overflow)
    | otherwise  = Right (Safe v)


clipToBounds ∷ WorldMap b o → V3 Int → Safe (V3 Int)
clipToBounds (view wm_size → (w, h, d)) v@(V3 x y z) = Safe (V3 (safe w x) (safe h y) (safe d z))
    where
        safe r = max 0 . min (fromIntegral r - 1)


checkBounds' ∷ WorldMap b o → Int → Either RangeError (Safe Int)
checkBounds' (view wm_size → (w, h, d)) i
    | i <  0                          = Left Underflow
    | i >= fromIntegral (cubed w h d) = Left Overflow
    | otherwise                       = Right (Safe i)


clipToBounds' ∷ WorldMap b o → Int → Safe Int
clipToBounds' (views wm_data length → r) = Safe . max 0 . min (fromIntegral r - 1)


baseAt ∷ WorldMap b o → Safe (V3 Int) → b
baseAt wm (view (unpacked._xy) → v) =
    let ix = linCoord wm v
    in  views wm_base (V.! ix) wm


cellAt ∷ WorldMap b o → Safe (V3 Int) → o
cellAt wm v =
    let ix = unpack (coordToIndex wm v)
    in  views wm_data (V.! ix) wm


column ∷ WorldMap b o → Safe (V3 Int) → [Safe (V3 Int)]
column m (unpack → v) = clipToBounds m . V3 (v ^. _x) (v ^. _y) . fromIntegral <$> depthRange
    where
        depthRange = [0..views (wm_size._3) (subtract 1) m]


interestingObjects ∷ WorldMap b o → Safe (V3 Int) → Int → (o → Bool) → [Safe (V3 Int)]
interestingObjects wm (unpack → v) (max 0 → r) ff =
    let inBoundPoints = rights (checkBounds wm <$> points)
    in  foldr collectPoints [] inBoundPoints
    where
        collectPoints ∷ Safe (V3 Int) → [Safe (V3 Int)] → [Safe (V3 Int)]
        collectPoints x l =
            let o = cellAt wm x
            in  bool l (x : l) (ff o)

        points ∷ [V3 Int]
        points = do
            (V2 x y) ← floodFillRange (fromIntegral r) (v ^. _xy)
            V3 x y <$> [0..views (wm_size._3) fromIntegral wm]


-- TODO upgrade to be a proper 3D raycast!
castRay ∷ (VisibleAPI o) ⇒ WorldMap b o → Safe (V3 Int) → Safe (V3 Int) → [Safe (V2 Int, Bool)]
castRay wm (unpack → s) (unpack → t) =
    let line        = drop 1 (bla (s ^. _xy) (t ^. _xy)) -- Drop 's'
        boundedLine = rights (checkBounds wm . v3f <$> line)
    in  findHits <$> boundedLine
    where
        findHits p =
            let c           = cellAt wm p
                goesThrough = isSeeThrough c || Visibility.height c < (s ^. _z)  -- && Visibility.height c < th
            in  Safe (view _xy (unpack p), goesThrough)

        v3f = V3 <$> view _x <*> view _y <*> const 0

--------------------------------------------------------------------------------

newtype WorldMapM b o a = WorldMapM { runWorldMapM ∷ State (WorldMap b o) a }
                      deriving (Functor, Applicative, Monad, MonadState (WorldMap b o))


runWorldMap ∷ WorldMapM b o a → WorldMap b o → (a, WorldMap b o)
runWorldMap wmm = runState (runWorldMapM wmm)


evalWorldMap ∷ WorldMapM b o a → WorldMap b o → a
evalWorldMap wmm = evalState (runWorldMapM wmm)


execWorldMap ∷ WorldMapM b o a → WorldMap b o → WorldMap b o
execWorldMap wmm = execState (runWorldMapM wmm)

--------------------------------------------------------------------------------

modifyCell ∷ Safe (V3 Int) → (o → o) → WorldMapM b o ()
modifyCell v f = do
    m ← get
    wm_data %= V.modify (\vec →
            let i = unpack (coordToIndex m v)
            in  MV.read vec i >>= MV.write vec i . f
        )

