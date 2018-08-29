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
, Cell
, valueAt
, lastValue
, replaceInCell
, addToCell
, deleteFromCell
, isEmpty

, WorldMapReadAPI(..)
, WorldMapAPI(..)

, WorldMap
-- TODO convert to API
, wm_data
, wm_spawns
, newWorldMap
, fromTileMap

, WorldMapM
, runWorldMap
, evalWorldMap
, execWorldMap
) where


import Safe                (atMay, lastMay)
import Control.Lens        (makeLenses, view, use, uses, (^.), (%=))
import Control.Monad       (filterM, foldM)
import Control.Monad.State (State, MonadState, runState, get)
import Data.Bool           (bool)
import Data.Monoid         ((<>))
import Data.List           (delete)
import Data.Maybe          (fromMaybe)
import Linear              (V2(V2))

import qualified Data.Vector         as V  (Vector, (!), (!?), imap, replicate, fromList, head,
                                            foldl', modify)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.Engine.Utils
import Dreamnet.Engine.TileMap

--------------------------------------------------------------------------------

type Range = Word

--------------------------------------------------------------------------------

newtype Cell a = Cell [a]
               deriving (Functor, Applicative, Monad, Semigroup, Monoid, Foldable, Traversable)


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
    interestingObjects ∷ V2 Int → Range → (a → Bool) → wm [V2 Int]
    oob                ∷ V2 Int → wm Bool


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
    , _wm_spawns  = V.fromList $ [V2 0 0]
    }


fromTileMap ∷ ∀ a. (Eq a) ⇒ TileMap → (Tile → a) → WorldMap a
fromTileMap tm t2o =
    WorldMap {
      _wm_width   = width tm
    , _wm_height  = height tm
    , _wm_data    = let mapData           = transposeAndMerge $ layerToObject (tm^.m_tileset) <$> (tm^.m_layers)
                        maybeObjects  i   = coordLin tm i `M.lookup` (tm^.m_positioned)
                        addPositioned i o = maybe o ((o<>) . Cell . fmap t2o) (maybeObjects i)
                    in  V.imap addPositioned mapData
    , _wm_desc    = tm^.m_desc
    , _wm_spawns  = V.fromList $ findAll '╳' (V.head $ tm^.m_layers) -- TODO this has to be proofed for the future a bit
    }
    where
        layerToObject ∷ Tileset → TileLayer → V.Vector a
        layerToObject ts tl = charToObject ts <$> (tl^.l_data)

        charToObject ∷ Tileset → Char → a
        charToObject ts c = let maybeTile = c `M.lookup` ts
                                err       = error ("Char " <> [c] <> " doesn't exist in the tileset!")
                            in  maybe err t2o maybeTile

        transposeAndMerge ∷ V.Vector (V.Vector a) → V.Vector (Cell a)
        transposeAndMerge ls = V.fromList $ mergeIntoList ls <$> [0..squareSize - 1]
            where
                squareSize = fromIntegral (width tm * height tm)

        mergeIntoList ∷ V.Vector (V.Vector a) → Int → (Cell a)
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

instance WorldMapReadAPI a (WorldMapM a) where
    desc = use wm_desc

    -- TODO partial function! :-O
    cellAt v = get >>= \m → uses wm_data (fromMaybe mempty . (V.!? linCoord m v))

    interestingObjects v r ff = do
        points ← filterM (fmap not . oob) (floodFillRange r v)
        foldM collectObjects [] points
        where
            collectObjects l x = cellAt x >>= \o → pure $ bool l (x : l) (or $ ff <$> o)

    oob v = flip outOfBounds v <$> get


instance WorldMapAPI a (WorldMapM a) where
    modifyCell v f = do
        m ← get
        wm_data %= V.modify (modifyInPlace m)
        where
            modifyInPlace m vec = do
                let i = linCoord m v
                os ← MV.read vec i
                MV.write vec i (f os)

    replaceCell v c = modifyCell v (const c)

