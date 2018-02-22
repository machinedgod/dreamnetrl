{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dreamnet.WorldMap
( WorldMap
, desc

-- TODO convert to API
, wm_data
, wm_spawns

, newWorldMap
, fromTileMap

, outOfBounds
, valuesAt
, modifyCell
, replaceCell
, addToCell
, deleteFromCell
, interestingObjects
) where

import Control.Lens   (makeLenses, view, views, (^.), (%~))
import Data.Bool      (bool)
import Data.Semigroup ((<>))
import Data.List      (delete)
import Linear         (V2(V2))

import qualified Data.Vector         as V  (Vector, (!), imap, replicate, fromList, head,
                                            foldl', modify)
import qualified Data.Vector.Mutable as MV (write, read)
import qualified Data.Map            as M  (lookup)


import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap

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


newWorldMap ∷ (Monoid a) ⇒ Width → Height → WorldMap a
newWorldMap w h = 
    WorldMap {
      _wm_width   = w
    , _wm_height  = h
    , _wm_data    = V.replicate (fromIntegral (w * h)) [mempty] 
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
        squareSize ∷ Int
        squareSize = fromIntegral (width tm * height tm)

        layerToObject ∷ Tileset → TileLayer → V.Vector a
        layerToObject ts tl = charToObject ts <$> (tl^.l_data)

        charToObject ∷ Tileset → Char → a
        charToObject ts c = let maybeTile = c `M.lookup` ts
                                err       = error ("Char " <> [c] <> " doesn't exist in the tileset!")
                            in  maybe err t2o maybeTile

        transposeAndMerge ∷ V.Vector (V.Vector a) → V.Vector [a]
        transposeAndMerge ls = V.fromList $ mergeIntoList ls <$> [0..squareSize - 1]

        mergeIntoList ∷ V.Vector (V.Vector a) → Int → [a]
        mergeIntoList ls i = V.foldl' appendIfDifferent [] ls
            where
                appendIfDifferent [] v = [v V.! i]
                appendIfDifferent l v  = if last l == v V.! i
                                           then l
                                           else l ++ [v V.! i]


desc ∷ WorldMap a → String
desc = view wm_desc


-- TODO partial function! :-O
valuesAt ∷ V2 Int → WorldMap a → [a]
valuesAt v m = views wm_data (V.! linCoord m v) m


modifyCell ∷ V2 Int → ([a] → [a]) → WorldMap a → WorldMap a
modifyCell v f m = wm_data %~ V.modify modifyInPlace $ m
    where
        modifyInPlace vec = do
            let i = linCoord m v
            os ← MV.read vec i
            MV.write vec i (f os)


replaceCell ∷ V2 Int → [a] → WorldMap a → WorldMap a
replaceCell v os = modifyCell v (const os)


addToCell ∷ V2 Int → a → WorldMap a → WorldMap a
addToCell v x = modifyCell v (<> [x])


deleteFromCell ∷ (Eq a) ⇒ V2 Int → a → WorldMap a → WorldMap a
deleteFromCell v x = modifyCell v (x `delete`)


interestingObjects ∷ V2 Int → Range → (a → Bool) → WorldMap a → [V2 Int]
interestingObjects v r ff m =
    let points = filter (not . outOfBounds m) (floodFillRange r v)
    in  foldr collectObjects [] points
    where
        collectObjects x l = let o = valuesAt x m
                             in  bool l (x : l) (or $ ff <$> o)
