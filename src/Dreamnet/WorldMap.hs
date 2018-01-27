{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.WorldMap
( WorldMap
, wm_width
, wm_height
, wm_data
, wm_visible
, wm_desc
, wm_spawns

, fromTileMap
, outOfBounds
, objectAt
, interestingObjects
) where

import Control.Lens   (makeLenses, views, (^.))
import Data.Bool      (bool)
import Data.Semigroup (Semigroup, (<>))
import Linear         (V2(V2))

import qualified Data.Vector as V (Vector, (!), imap, replicate, fromList, head,
                                   length, zip, zip3, zip4)
import qualified Data.Map    as M (lookup)


import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap

--------------------------------------------------------------------------------

type Range = Word

type Width  = Word
type Height = Word

-- | Type variables
--   a: gameplay data
--   b: visibility data
data WorldMap a b = WorldMap {
      _wm_width   ∷ Width
    , _wm_height  ∷ Height
    , _wm_data    ∷ V.Vector a
    , _wm_visible ∷ V.Vector b
    , _wm_desc    ∷ String
    , _wm_spawns  ∷ V.Vector (V2 Int) 
    }

makeLenses ''WorldMap


instance CoordVector (WorldMap a b) where
    width = (^.wm_width)


fromTileMap ∷ (Eq a, Semigroup a) ⇒ TileMap → (Tile → a) → b → WorldMap a b
fromTileMap tm t2o dv = 
    WorldMap {
      _wm_width   = tm^.m_width
    , _wm_height  = tm^.m_height
    , _wm_data    = let layerData         = mergeLayers $ layerToObject t2o <$> (tm^.m_layers)
                        maybeObjects i    = M.lookup (coordLin tm i) (tm^.m_positioned)
                        mergeTiles []     = error "Positioned tile list empty, instead of not being added at all! :-O"
                        mergeTiles [t]    = t2o t
                        mergeTiles l      = foldr1 (<>) $ fmap t2o l
                        addPositioned i o = maybe o ((o <>) . mergeTiles) (maybeObjects i)
                    in  V.imap addPositioned layerData
    , _wm_visible = V.replicate squareSize dv
    , _wm_desc    = tm^.m_desc
    , _wm_spawns  = V.fromList $ findAll '╳' (V.head $ tm^.m_layers) -- TODO this has to be proofed for the future a bit
    }
    where
        squareSize = fromIntegral (tm^.m_width * tm^.m_height)


layerToObject ∷ (Tile → a) → TileLayer → V.Vector a
layerToObject t2o tl = charToObject (tl^.l_tileset) <$> (tl^.l_data)
    where
        charToObject ts c = let maybeTile = c `M.lookup` ts
                                err       = error ("Char " <> [c] <> " doesn't exist in the tileset!")
                            in  maybe err t2o maybeTile
                            --in  maybe err (objectFromTile dd) maybeTile


mergeLayers ∷ (Eq a, Semigroup a) ⇒ V.Vector (V.Vector a) → V.Vector a
mergeLayers   (V.length → 0) = error "No layers to merge!"
mergeLayers i@(V.length → 1) = V.head i
mergeLayers i@(V.length → 2) = merge2 <$> V.zip  (i V.! 0) (i V.! 1)
mergeLayers i@(V.length → 3) = merge3 <$> V.zip3 (i V.! 0) (i V.! 1) (i V.! 2)
mergeLayers i@(V.length → 4) = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)
mergeLayers i                = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)

merge2 ∷ (Eq a, Semigroup a) ⇒ (a, a) → a
merge2 (o1, o2) = bool o1 (o1 <> o2) (o1 /= o2)

merge3 ∷ (Eq a, Semigroup a) ⇒ (a, a, a) → a
merge3 (o1, o2, o3) = let oTop = merge2 (o2, o3) 
                      in  bool o1 (o1 <> oTop) (o1 /= oTop)

merge4 ∷ (Eq a, Semigroup a) ⇒ (a, a, a, a) → a
merge4 (o1, o2, o3, o4) = let oTop = merge3 (o2, o3, o4)
                          in  bool o1 (o1 <> oTop) (o1 /= oTop)


outOfBounds ∷ WorldMap a b → V2 Int → Bool
outOfBounds m (V2 x y)
    | x < 0                                = True
    | y < 0                                = True
    | x >= (fromIntegral $ m ^. wm_width)  = True
    | y >= (fromIntegral $ m ^. wm_height) = True
    | otherwise                            = False


objectAt ∷ V2 Int → WorldMap a b → a
objectAt v m = views wm_data (V.! linCoord m v) m


interestingObjects ∷ V2 Int → Range → (a → Bool) → WorldMap a b → [V2 Int]
interestingObjects v r ff m =
    let points = filter (not . outOfBounds m) (floodFillRange r v)
    in  foldr collectObjects [] points
    where
        collectObjects x l = let o = objectAt x m
                             in  bool l (x : l) (ff o)


--removeFromWorldPile ∷ (MonadWorld w) ⇒ V2 Int → Item → w ()
--removeFromWorldPile v i = w_items %= M.update (wrapMaybe . removeFromList i) v
--    where
--        removeFromList i l = filter (/=i) l
--        wrapMaybe [] = Nothing
--        wrapMaybe l  = Just l
--
--

