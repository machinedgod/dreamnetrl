{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Dreamnet.WorldMap
( WorldMap

-- TODO convert to API
, wm_data
, wm_spawns

, newWorldMap
, fromTileMap

, WorldMapReadAPI(..)
, WorldMapAPI(..)
, WorldMapM
, runWorldMap
, evalWorldMap
, execWorldMap
) where

import Safe                (atMay)
import Control.Lens        (makeLenses, view, use, uses, (^.), (%=))
import Control.Monad       (filterM, foldM)
import Control.Monad.State (State, MonadState, runState, get)
import Data.Bool           (bool)
import Data.Semigroup      ((<>))
import Data.List           (delete)
import Data.Maybe          (fromMaybe)
import Linear              (V2(V2))

import qualified Data.Vector         as V  (Vector, (!), (!?), imap, replicate, fromList, head,
                                            foldl', modify)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap

--------------------------------------------------------------------------------

class WorldMapReadAPI a wm | wm → a where
    desc               ∷ wm String
    valuesAt           ∷ V2 Int → wm [a]
    valueAt            ∷ V2 Int → Int → wm (Maybe a)
    interestingObjects ∷ V2 Int → Range → (a → Bool) → wm [V2 Int]
    oob                ∷ V2 Int → wm Bool


class (WorldMapReadAPI a wm) ⇒ WorldMapAPI a wm | wm → a where
    modifyCell     ∷ V2 Int → ([a] → [a]) → wm ()
    replaceCell    ∷ V2 Int → [a] → wm ()
    replaceInCell  ∷ V2 Int → Int → a → wm ()
    addToCell      ∷ V2 Int → a → wm ()
    deleteFromCell ∷ (Eq a) ⇒ V2 Int → a → wm ()
    
--------------------------------------------------------------------------------

type Range = Word

-- | Type variables
--   a: gameplay data
data WorldMap a = WorldMap {
      _wm_width   ∷ Width
    , _wm_height  ∷ Height
    , _wm_data    ∷ V.Vector [a]
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
    , _wm_data    = V.replicate (fromIntegral (w * h)) [x] 
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
                        addPositioned i o = maybe o ((o<>) . fmap t2o) (maybeObjects i)
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

        transposeAndMerge ∷ V.Vector (V.Vector a) → V.Vector [a]
        transposeAndMerge ls = V.fromList $ mergeIntoList ls <$> [0..squareSize - 1]
            where
                squareSize = fromIntegral (width tm * height tm)

        mergeIntoList ∷ V.Vector (V.Vector a) → Int → [a]
        mergeIntoList ls i = V.foldl' appendIfDifferent [] ls
            where
                appendIfDifferent [] v = [v V.! i]
                appendIfDifferent l v  = if last l == v V.! i
                                           then l
                                           else l ++ [v V.! i]

--------------------------------------------------------------------------------

newtype WorldMapM o a = WorldMapM { runWorldMapM ∷ State (WorldMap o) a }
                      deriving (Functor, Applicative, Monad, MonadState (WorldMap o))


runWorldMap ∷ WorldMapM o a → WorldMap o → (a, WorldMap o)
runWorldMap wmm wm = runState (runWorldMapM wmm) wm


evalWorldMap ∷ WorldMapM o a → WorldMap o → a
evalWorldMap wmm = fst . runWorldMap wmm


execWorldMap ∷ WorldMapM o a → WorldMap o → WorldMap o
execWorldMap wmm = snd . runWorldMap wmm

--------------------------------------------------------------------------------

instance WorldMapReadAPI a (WorldMapM a) where
    desc = use wm_desc
    
    -- TODO partial function! :-O
    valuesAt v = get >>= \m → uses wm_data (fromMaybe [] . (V.!? linCoord m v))

    valueAt v i = (`atMay` i) <$> valuesAt v

    interestingObjects v r ff = do
        points ← filterM (fmap not . oob) (floodFillRange r v)
        foldM collectObjects [] points
        where
            collectObjects l x = valuesAt x >>= \o → pure $ bool l (x : l) (or $ ff <$> o)

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

    replaceInCell v ix x = modifyCell v (\l → take ix l <> [x] <> drop (ix + 1) l)

    addToCell v x = modifyCell v (<> [x])

    deleteFromCell v x = modifyCell v (x `delete`)

