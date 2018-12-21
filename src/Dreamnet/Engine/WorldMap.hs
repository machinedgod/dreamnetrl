{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}

module Dreamnet.Engine.WorldMap
( RangeError(..), OobError(..), Safe(unpack), unpacked

, Base(..), wb_symbol, wb_isSpawn
, Cell
, WorldMap, wm_size, wm_base, wm_data, wm_desc, wm_spawns

, newWorldMap, fromTileMap, {- fromTileMapColumns, -} coordToIndex, indexToCoord
, checkBounds, checkBounds', clipToBounds, clipToBounds', baseAt, baseAtL
, cellAt, cellAtL , isEmptyAt, dataPosition, followLinks, followLinksL
, objectAt, column, columnL, interestingObjects, castRay, predIfJust
, collectNonEmpty

, WorldMapM, runWorldMap, evalWorldMap, execWorldMap

, modifyCell, moveCell, modifyObject, spawnObject, spawnLink, deleteObject
, moveObject
)
where


import Prelude hiding         (head, tail, last)
import Control.Lens           (Getter, to, makeLenses, view, views, preview,
                               _1, _2, _3, lens, _Just)
import Control.Lens.Operators
import Control.Applicative    (Alternative(..))
import Control.Monad.State    (State, MonadState, runState, evalState,
                               execState, get, gets)
import Control.Monad.Except   (MonadError)
import Data.Bifunctor         (Bifunctor(bimap))
import Data.Either            (rights)
import Data.Bool              (bool)
import Data.Foldable          (traverse_)
import Data.List.NonEmpty     (head, tail)
import Linear                 (V2(V2), V3(V3), _x, _y, _xy, zero)

import qualified Data.Vector         as V  (Vector, (!), replicate, fromList,
                                            modify, concat)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Visibility
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


newtype Safe a
    = Safe { unpack ∷ a }
    deriving (Eq, Show, Semigroup, Monoid, Num)


newtype Safe' a t
    = Safe' { unpack' ∷ a }
    deriving (Eq, Show)


unpacked ∷ Getter (Safe a) a
unpacked = lens unpack (const Safe)

-------------------------------------------------------------------------------- 

type IsSpawnLocation = Bool


data Base a =
    Base { 
      _wb_symbol  ∷ a
    , _wb_isSpawn ∷ IsSpawnLocation
    }
    deriving (Functor, Show)
makeLenses ''Base

instance Applicative Base where
    pure x = Base x False
    Base f _ <*> x = fmap f x
instance Monad Base where
    Base x _ >>= f = f x

--------------------------------------------------------------------------------

data Cell a =
      Link LinkCell
    | Data (DataCell a)
    | Empty
    deriving(Eq, Functor, Show)

instance Applicative Cell where
    pure                      = Data . pure
    Data (DataCell _ f) <*> x = fmap f x
    Empty <*> _               = Empty
    Link v <*> _              = Link v
instance Monad Cell where
   Data (DataCell _ x) >>= f = f x
   Link l >>= _              = Link l
   Empty  >>= _              = Empty
instance Alternative Cell where
    empty = Empty
    Data d <|> _      = Data d

    Link _ <|> Data d = Data d
    Link v <|> _      = Link v

    Empty  <|> x      = x



data DataCell a
    = DataCell [Safe (V3 Int)] a
    deriving(Eq, Functor, Show)

instance Applicative DataCell where
    pure = DataCell []
    DataCell _ f <*> x = fmap f x
instance Monad DataCell where
    DataCell _ x >>= f = f x


data LinkCell
    = LinkCell (Safe (V3 Int))
    deriving(Eq, Show)

instance Semigroup LinkCell where
    LinkCell v <> LinkCell v2 = LinkCell (v + v2)
instance Monoid LinkCell where
    mempty = LinkCell (Safe zero)

--------------------------------------------------------------------------------

-- | Type variables
--   b: base layer data
--   o: gameplay data
data WorldMap b o = WorldMap {
      _wm_size   ∷ (Width, Height, Depth)
    , _wm_base   ∷ V.Vector (Base b)
    , _wm_data   ∷ V.Vector (Cell o)
    , _wm_desc   ∷ String
    , _wm_spawns ∷ V.Vector (Safe (V3 Int))
    }
    deriving (Show)
makeLenses ''WorldMap


instance Bifunctor WorldMap where
    bimap f g = (wm_data %~ fmap (fmap g)) . (wm_base %~ fmap (fmap f))
instance CoordVector (WorldMap b o) where
    width  = view (wm_size._1)
    height = view (wm_size._2)

--------------------------------------------------------------------------------

newWorldMap ∷ Width → Height → Depth → b → Cell o →  WorldMap b o
newWorldMap w h d b x =
    WorldMap {
      _wm_size   = (w, h, d)
    , _wm_base   = V.replicate (fromIntegral (squared w h)) (Base b False)
    , _wm_data   = V.replicate (fromIntegral (cubed w h d)) x
    , _wm_desc   = "Debug generated map!"
    , _wm_spawns = V.fromList [Safe (V3 0 0 0)]
    }


fromTileMap ∷ (Eq o, MonadError String me)
            ⇒ (Tile → me (Base b))
            → (Tile → me (Cell o))
            → TileMap
            → me (WorldMap b o)
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
            , _wm_spawns = spawnPositions baseLayer -- TODO this has to be proofed for the future a bit
            }


{-
fromTileMapColumns ∷ (Eq o, MonadError String me) 
                   ⇒ (Tile → me (Base b)) 
                   → (Tile → me ([Cell o])) 
                   → TileMap 
                   → me (WorldMap b o)
fromTileMapColumns t2b t2c tm =
    let baseLayer = views m_layers head tm
        dataLayer = views m_layers last tm
        depth     = views m_layers (subtract 1 . fromIntegral . length)
    in  do
        base           ← layerToVector (tm ^. m_tileset) t2b baseLayer
        layeredObjects ← layerToVector (tm ^. m_tileset) t2c dataLayer
        pure $ WorldMap
            { _wm_size   = (width tm, height tm, depth tm)
            , _wm_base   = base
            , _wm_data   = layeredObjects
            , _wm_desc   = tm ^. m_desc
            , _wm_spawns = spawnPositions baseLayer -- TODO this has to be proofed for the future a bit
            }
-}

--maybeObjects  i = coordLin tm i `M.lookup` (tm^.m_positioned)

--addPositioned i o = maybe (Right o) (fmap ((o<>) . Cell) . traverse t2o) (maybeObjects i)


spawnPositions ∷ TileLayer → V.Vector (Safe (V3 Int))
spawnPositions bl = let toSpawnPos = Safe . (V3 <$> view _x <*> view _y <*> const 0)
                    in  toSpawnPos <$> V.fromList (findAll '╳' bl)


layerToVector ∷ (MonadError String me) ⇒ Tileset → (Tile → me a) → TileLayer → me (V.Vector a)
layerToVector ts fo = traverse (charToObject ts fo) . view l_data


charToObject ∷ (MonadError String me) ⇒ Tileset → (Tile → me a) → Char → me a
charToObject ts fo c =
    let maybeObj = c `M.lookup` ts
    in  fo =<< maybeToError ("Char " <> [c] <> " doesn't exist in the tileset!") maybeObj

--------------------------------------------------------------------------------

coordToIndex ∷ WorldMap b o → Safe (V3 Int) → Safe Int
coordToIndex m (unpack → V3 x y z) = Safe (x + fromIntegral (width m) * (y + fromIntegral (height m) * z))


indexToCoord ∷ WorldMap b o → Safe Int → Safe (V3 Int)
indexToCoord (view wm_size → (w, h, _)) (Safe i) =
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
clipToBounds (view wm_size → (w, h, d)) (V3 x y z) = Safe (V3 (safe w x) (safe h y) (safe d z))
    where
        safe r = max 0 . min (fromIntegral r - 1)


checkBounds' ∷ WorldMap b o → Int → Either RangeError (Safe Int)
checkBounds' (view wm_size → (w, h, d)) i
    | i <  0                          = Left Underflow
    | i >= fromIntegral (cubed w h d) = Left Overflow
    | otherwise                       = Right (Safe i)


clipToBounds' ∷ WorldMap b o → Int → Safe Int
clipToBounds' (views wm_data length → r) = Safe . max 0 . min (fromIntegral r - 1)


baseAt ∷ WorldMap b o → Safe (V3 Int) → Base b
baseAt wm (view (unpacked._xy) → v) =
    let ix = linCoord wm v
    in  views wm_base (V.! ix) wm


baseAtL ∷ WorldMap b o → Getter (Safe (V3 Int)) (Base b)
baseAtL wm = to (baseAt wm)


cellAt ∷ WorldMap b o → Safe (V3 Int) → Cell o
cellAt wm v =
    let ix = unpack (coordToIndex wm v)
    in  views wm_data (V.! ix) wm


cellAtL ∷ WorldMap b o → Getter (Safe (V3 Int)) (Cell o)
cellAtL wm = to (cellAt wm)


isEmptyAt ∷ WorldMap b o → Safe (V3 Int) → Bool
isEmptyAt wm v = case cellAt wm v of
                    Empty → True
                    _     → False


-- TODO ensure that, somehow, we can only ask for data positions on link cells or Data cells
dataPosition ∷ WorldMap b o → Safe (V3 Int) → Maybe (Safe (V3 Int))
dataPosition m v = case cellAt m v of
                    (Data _)             → pure v
                    (Link (LinkCell lv)) → dataPosition m lv
                    Empty                → empty


followLinks ∷ WorldMap b o → Cell o → Maybe o
followLinks _ (Data (DataCell _ os)) = pure os
followLinks m (Link (LinkCell v))    = dataPosition m v >>= followLinks m . cellAt m
followLinks _ Empty                  = empty


followLinksL ∷ WorldMap b o → Getter (Cell o) (Maybe o)
followLinksL w = to (followLinks w)


objectAt ∷ WorldMap b o → Safe (V3 Int) → Maybe o
objectAt m p = followLinks m (cellAt m p)


column ∷ WorldMap b o → Safe (V3 Int) → [Safe (V3 Int)]
column m (unpack → v) = clipToBounds m . V3 (v ^. _x) (v ^. _y) . fromIntegral <$> depthRange
    where
        depthRange = [0..views (wm_size._3) (subtract 1) m]


columnL ∷ WorldMap b o → Getter (Safe (V3 Int)) [Safe (V3 Int)]
columnL wm = to (column wm)


interestingObjects ∷ WorldMap b o → Safe (V3 Int) → Int → (Cell o → Bool) → [Safe (V3 Int)]
interestingObjects wm (unpack → v) (max 0 → r) ff =
    let inBoundPoints = rights (checkBounds wm <$> points)
    in  foldr collectPoints [] inBoundPoints
    where
        collectPoints ∷ Safe (V3 Int) → [Safe (V3 Int)] → [Safe (V3 Int)]
        collectPoints x l =
            --let o = followLinks wm (cellAt wm x)
            let o = cellAt wm <$> dataPosition wm x
            in  bool l (x : l) (maybe False ff o)

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
            let c           = followLinks wm (cellAt wm p)
                goesThrough = maybe False isSeeThrough c -- && Visibility.height c < th
                --goesThrough = seeF c || Visibility.height c < (s ^. _z)  -- && Visibility.height c < th
            in  Safe (view _xy (unpack p), goesThrough)

        v3f = V3 <$> view _x <*> view _y <*> const 0


predIfJust ∷ WorldMap b o → (o → Bool) → (Cell o → Bool)
predIfJust m f = maybe False f . preview (followLinksL m._Just)


-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
collectNonEmpty ∷ WorldMap b o → Safe (V3 Int) → [(Safe (V3 Int), o)] → [(Safe (V3 Int), o)]
collectNonEmpty wm p l =
    maybe l collect $
        preview
            (followLinksL wm._Just)
            (cellAt wm p)
    where
        collect o = (p, o) : l


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

modifyCell ∷ Safe (V3 Int) → (Cell o → Cell o) → WorldMapM b o ()
modifyCell v f = do
    m ← get
    wm_data %= V.modify (\vec →
            let i = unpack (coordToIndex m v)
            in  MV.read vec i >>= MV.write vec i . f
        )


-- | If source coordinate points at a DataCell,
--   it moves it to target
moveCell ∷ Safe (V3 Int) → Safe (V3 Int) → WorldMapM b o ()
moveCell cp np = do
    o ← gets (`cellAt` cp)
    modifyCell cp (const Empty)
    modifyCell np (const o)


modifyObject ∷ Safe (V3 Int) → (o → o) → WorldMapM b o ()
modifyObject v f = modifyCell v (fmap f)
--modifyObject v f = do
--    o ← gets (`dataPosition` v)
--    modifyCell v (fmap f)


-- TODO should this work only on the empty cell?
-- TODO use Safe' to ensure given vector is empty
spawnObject ∷ Safe (V3 Int) → o → WorldMapM b o ()
spawnObject v s = gets (`cellAt` v) >>= \case 
    Empty  → modifyCell v (const (pure s))
    Data _ → pure ()
    Link _ → pure ()



spawnLink ∷ Safe (V3 Int) → Safe (V3 Int) → WorldMapM b o ()
spawnLink s t = gets (`cellAt` t) >>= \case
    Data d → do
        let (nd, nl) = bimap Data Link (mkLink t d)
        modifyCell s (const nl)
        modifyCell t (const nd)
    Link _ → pure ()
    Empty  → pure ()
    where
        mkLink ∷ Safe (V3 Int) → DataCell a → (DataCell a, LinkCell)
        mkLink v d = (appendLink d, LinkCell v)
            where
                appendLink (DataCell ls x) = DataCell (v : ls) x




-- | Deletes object with all its links
deleteObject ∷ Safe (V3 Int) → WorldMapM b o ()
deleteObject v = gets (`cellAt` v) >>= \case
    (Data (DataCell _ _)) → modifyCell v (const Empty)
    (Link (LinkCell cv))  → deleteObject cv
    Empty                 → pure ()



-- | If source coord points at a DataCell, it moves it and all
--   its linked cells, relative to the DataCell movement
--   TODO can be implemented with delete object and spawnObject/spawnLinks
moveObject ∷ Safe (V3 Int) → Safe (V3 Int) → WorldMapM b o ()
moveObject s t = do
    mCorePos ← gets (`dataPosition` s)
    case mCorePos of
        Nothing → pure ()
        Just corePos → gets (`cellAt` corePos) >>= \case
            (Data (DataCell links _)) → do
                wm ← get
                let dir            = unpack t - unpack s
                    allTargets     = unpack corePos + dir : fmap ((+dir) . unpack) links
                    checkedTargets = traverse (checkBounds wm) allTargets
                case checkedTargets of
                    (Right ts) → traverse_ (uncurry moveCell) $ zip (corePos : links) ts
                    _          → pure ()
            _ → pure ()

