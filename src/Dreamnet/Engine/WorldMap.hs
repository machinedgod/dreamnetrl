{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Dreamnet.Engine.WorldMap
( module Dreamnet.Engine.TileMap

, Range
, Cell(cellValues), valueAt, lastValue, replaceInCell, addToCell,
  deleteFromCell, isEmpty

, WorldMapReadAPI(..)
, WorldMapAPI(..)

-- TODO convert to API
, WorldMap, wm_data, wm_spawns, newWorldMap, fromTileMap

, WorldMapM, runWorldMap, evalWorldMap, execWorldMap
) where


import Safe                (atMay, lastMay)
import Control.Lens        (makeLenses, view, use, uses, (^.), (%=))
import Control.Monad       (filterM, foldM)
import Control.Monad.State (State, MonadState, runState, get, gets)
import Data.Bool           (bool)
import Data.Monoid         ((<>))
import Data.List           (delete)
import Data.Maybe          (fromMaybe)
import Linear              (V2(V2))

import qualified Data.Vector         as V  (Vector, (!), (!?), imapM, replicate, fromList, head,
                                            foldl', modify)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.Engine.Utils
import Dreamnet.Engine.TileMap
import Dreamnet.Engine.Visibility hiding (height)
import qualified Dreamnet.Engine.Visibility as Visibility (height)

--------------------------------------------------------------------------------

type Range = Word

--------------------------------------------------------------------------------

newtype Cell a = Cell { cellValues ∷ [a] }
               deriving (Functor, Applicative, Monad, Semigroup, Monoid, Foldable, Traversable)


instance (VisibleAPI a) ⇒ VisibleAPI (Cell a) where
    isSeeThrough = and . fmap isSeeThrough . cellValues
    height = maximum . fmap Visibility.height . cellValues


valueAt ∷ Int → Cell a → Maybe a
valueAt i (Cell l) = l `atMay` i


lastValue ∷ Cell a → Maybe a
lastValue (Cell l) = lastMay l


replaceInCell ∷ Int → a → Cell a → Cell a
replaceInCell ix x (Cell l) = Cell (take ix l <> [x] <> drop (ix + 1) l)


addToCell ∷ a → Cell a → Cell a
addToCell x (Cell l) = Cell (l <> [x])


deleteFromCell ∷ (Eq a) ⇒ a → Cell a → Cell a
deleteFromCell x (Cell l) = Cell (x `delete` l)


isEmpty ∷ Cell a → Bool
isEmpty (Cell l) = null l

--------------------------------------------------------------------------------

class WorldMapReadAPI a wm | wm → a where
    desc               ∷ wm String
    cellAt             ∷ V2 Int → wm (Cell a)
    interestingObjects ∷ V2 Int → Range → (a → Bool) → wm [V2 Int] -- TODO make tuple of (V2 INt, Int)
    oob                ∷ V2 Int → wm Bool
    castRay            ∷ V2 Int → Int → V2 Int → Int → wm [(V2 Int, Bool)]


class (WorldMapReadAPI a wm) ⇒ WorldMapAPI a wm | wm → a where
    modifyCell  ∷ V2 Int → (Cell a → Cell a) → wm ()
    replaceCell ∷ V2 Int → Cell a → wm ()

--------------------------------------------------------------------------------

-- | Type variables
--   a: gameplay data
data WorldMap a = WorldMap {
      _wm_width   ∷ Width
    , _wm_height  ∷ Height
    , _wm_data    ∷ V.Vector (Cell a)
    , _wm_desc    ∷ String
    , _wm_spawns  ∷ V.Vector (V2 Int)
    }
makeLenses ''WorldMap


instance CoordVector (WorldMap a) where
    width  = view wm_width
    height = view wm_height


newWorldMap ∷ Width → Height → a →  WorldMap a
newWorldMap w h x =
    WorldMap {
      _wm_width   = w
    , _wm_height  = h
    , _wm_data    = V.replicate (fromIntegral (w * h)) (pure x)
    , _wm_desc    = "Debug generated map!"
    , _wm_spawns  = V.fromList [V2 0 0]
    }


fromTileMap ∷ ∀ a. (Eq a) ⇒ TileMap → (Tile → Either String a) → Either String (WorldMap a)
fromTileMap tm t2o = do
    let maybeObjects  i   = coordLin tm i `M.lookup` (tm^.m_positioned)
        addPositioned i o = maybe (Right o) (\os → (o<>) . Cell <$> traverse t2o os) (maybeObjects i)
    mapData ←  V.imapM addPositioned =<< transposeAndMerge <$> traverse (layerToObject (tm^.m_tileset)) (tm^.m_layers)
    pure $ WorldMap { _wm_width   = width tm
                    , _wm_height  = height tm
                    , _wm_data    = mapData
                    , _wm_desc    = tm^.m_desc
                    , _wm_spawns  = V.fromList $ findAll '╳' (V.head $ tm^.m_layers) -- TODO this has to be proofed for the future a bit
                    }
    where
        layerToObject ∷ Tileset → TileLayer → Either String (V.Vector a)
        layerToObject ts tl = traverse (charToObject ts) (tl^.l_data)

        charToObject ∷ Tileset → Char → Either String a
        charToObject ts c = let maybeTile = c `M.lookup` ts
                                err       = error ("Char " <> [c] <> " doesn't exist in the tileset!") -- No no no, use alternative
                            in  maybe err t2o maybeTile

        transposeAndMerge ∷ V.Vector (V.Vector a) → V.Vector (Cell a)
        transposeAndMerge ls = V.fromList $ mergeIntoList ls <$> [0..squareSize - 1]
            where
                squareSize = fromIntegral (width tm * height tm)

        mergeIntoList ∷ V.Vector (V.Vector a) → Int → Cell a
        mergeIntoList ls i = V.foldl' appendIfDifferent mempty ls
            where
                appendIfDifferent (Cell []) v = Cell [v V.! i]
                appendIfDifferent (Cell l)  v =
                    if last l == v V.! i
                        then Cell l
                        else Cell (l ++ [v V.! i])

--------------------------------------------------------------------------------

newtype WorldMapM o a = WorldMapM { runWorldMapM ∷ State (WorldMap o) a }
                      deriving (Functor, Applicative, Monad, MonadState (WorldMap o))


runWorldMap ∷ WorldMapM o a → WorldMap o → (a, WorldMap o)
runWorldMap wmm = runState (runWorldMapM wmm)


evalWorldMap ∷ WorldMapM o a → WorldMap o → a
evalWorldMap wmm = fst . runWorldMap wmm


execWorldMap ∷ WorldMapM o a → WorldMap o → WorldMap o
execWorldMap wmm = snd . runWorldMap wmm

--------------------------------------------------------------------------------

instance (VisibleAPI a) ⇒ WorldMapReadAPI a (WorldMapM a) where
    desc = use wm_desc

    -- TODO partial function! :-O
    cellAt v = get >>= \m → uses wm_data (fromMaybe mempty . (V.!? linCoord m v))

    interestingObjects v r ff = do
        points ← filterM (fmap not . oob) (floodFillRange r v)
        foldM collectObjects [] points
        where
            collectObjects l x = cellAt x >>= \o → pure $ bool l (x : l) (or $ ff <$> o)

    oob = gets . flip outOfBounds

    castRay s sh t _ = traverse findHits =<< filterM (fmap not . oob) (drop 1 $ bla s t) -- Dropping originating V2 
        where
            findHits p = do
                c ← cellAt p
                let goesThrough = isSeeThrough c || Visibility.height c < sh  -- && Visibility.height c < th
                pure (p, goesThrough)



instance (VisibleAPI a) ⇒ WorldMapAPI a (WorldMapM a) where
    modifyCell v f = do
        m ← get
        wm_data %= V.modify (modifyInPlace m)
        where
            modifyInPlace m vec = do
                let i = linCoord m v
                os ← MV.read vec i
                MV.write vec i (f os)

    replaceCell v c = modifyCell v (const c)

