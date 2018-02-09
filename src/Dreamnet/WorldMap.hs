{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dreamnet.WorldMap
( WorldMap
, wm_width
, wm_height
, wm_data
, wm_visible
, wm_desc
, wm_spawns

, newWorldMap
, fromTileMap
, outOfBounds
, objectsAt
, interestingObjects
) where

import Control.Lens   (makeLenses, views, (^.))
import Data.Bool      (bool)
import Data.Semigroup ((<>))
import Linear         (V2(V2))

import qualified Data.Vector as V (Vector, (!), imap, replicate, fromList, head,
                                   foldl')
import qualified Data.Map    as M (lookup)


import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap

--------------------------------------------------------------------------------

type Range    = Word
type Width    = Word
type Height   = Word

-- | Type variables
--   a: gameplay data
--   b: visibility data
data WorldMap a b = WorldMap {
      _wm_width   ∷ Width
    , _wm_height  ∷ Height
    , _wm_data    ∷ V.Vector [a]
    , _wm_visible ∷ V.Vector b
    , _wm_desc    ∷ String
    , _wm_spawns  ∷ V.Vector (V2 Int) 
    }

makeLenses ''WorldMap


instance CoordVector (WorldMap a b) where
    width = (^.wm_width)


newWorldMap ∷ (Monoid a, Monoid b) ⇒ Width → Height → WorldMap a b
newWorldMap w h = 
    WorldMap {
      _wm_width   = w
    , _wm_height  = h
    , _wm_data    = V.replicate (fromIntegral (w * h)) [mempty] 
    , _wm_visible = V.replicate (fromIntegral (w * h)) mempty
    , _wm_desc    = "Debug generated map!"
    , _wm_spawns  = V.fromList $ [V2 0 0]
    }


fromTileMap ∷ ∀ a b. (Eq a) ⇒ TileMap → (Tile → a) → b → WorldMap a b
fromTileMap tm t2o dv = 
    WorldMap {
      _wm_width   = tm^.m_width
    , _wm_height  = tm^.m_height
    , _wm_data    = let mapData           = transposeAndMerge $ layerToObject (tm^.m_tileset) <$> (tm^.m_layers)
                        maybeObjects i    = coordLin tm i `M.lookup` (tm^.m_positioned)
                        addPositioned i o = maybe o ((o<>) . fmap t2o) (maybeObjects i)
                    in  V.imap addPositioned mapData
    , _wm_visible = V.replicate squareSize dv
    , _wm_desc    = tm^.m_desc
    , _wm_spawns  = V.fromList $ findAll '╳' (V.head $ tm^.m_layers) -- TODO this has to be proofed for the future a bit
    }
    where
        squareSize ∷ Int
        squareSize = fromIntegral (tm^.m_width * tm^.m_height)

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


outOfBounds ∷ WorldMap a b → V2 Int → Bool
outOfBounds m (V2 x y)
    | x < 0                              = True
    | y < 0                              = True
    | x >= fromIntegral (m ^. wm_width)  = True
    | y >= fromIntegral (m ^. wm_height) = True
    | otherwise                          = False


-- TODO partial function! :-O
objectsAt ∷ V2 Int → WorldMap a b → [a]
objectsAt v m = views wm_data (V.! linCoord m v) m


interestingObjects ∷ V2 Int → Range → (a → Bool) → WorldMap a b → [V2 Int]
interestingObjects v r ff m =
    let points = filter (not . outOfBounds m) (floodFillRange r v)
    in  foldr collectObjects [] points
    where
        collectObjects x l = let o = objectsAt x m
                             in  bool l (x : l) (or $ ff <$> o)
