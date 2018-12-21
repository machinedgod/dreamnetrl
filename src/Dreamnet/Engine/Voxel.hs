{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TemplateHaskell            #-}


module Dreamnet.Engine.Voxel
where

import Prelude hiding (Either(..))

import Control.Applicative  (Alternative(empty))
import Control.Lens         (makeLenses, (^.), _1, _2)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad        (when)
import Data.Foldable        (for_)
import Data.List            (group)
import Linear               (V2(V2))

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.TileMap

--------------------------------------------------------------------------------

data Projection
    = Front
    | Left
    | Top


newtype Grid (p ∷ Projection)
    = Grid { ungrid ∷ TileLayer } 
    deriving (Show)


loadProjections ∷ FilePath → IO (Grid 'Front, Grid 'Left, Grid 'Top)
loadProjections fp = (,,) <$> (Grid <$> loadLayer (fp <> "." <> "fl"))
                          <*> (Grid <$> loadLayer (fp <> "." <> "ll"))
                          <*> (Grid <$> loadLayer (fp <> "." <> "tl"))


data VoxelGrid
    = VoxelGrid {
      _vgSize   ∷ (Width, Height, Depth)
    , _vgVoxels ∷ V.Vector (Maybe Char)
    }
    deriving (Show)
makeLenses ''VoxelGrid


fromProjections ∷ (MonadError String me) 
                ⇒ Grid 'Front 
                → Grid 'Left 
                → Grid 'Top 
                → me VoxelGrid
fromProjections (Grid fl) (Grid ll) (Grid tl) = do
    when(height fl /= height ll) $
        throwError "Front and left projections heights aren't equal."
    when(height tl /= fromIntegral (width ll)) $
        throwError "Top projection height /= left projections width."
    when(width fl /= width tl) $
        throwError "Front and top projections widths aren't equal."
    let w    = fl ^. l_size._1
        h    = fl ^. l_size._2
        d    = fromIntegral (tl ^. l_size._2)
        grid = V.replicate (fromIntegral (cubed w h d)) Nothing

    -- Try 1: if voxel exist in all projections, it is solid.
    --        otherwise, its empty
    let allCoords = (,,) <$> [0..w-1] <*> [0..h-1] <*> [0..d-1]

    pure $
        VoxelGrid 
            (w, h, d) 
            (V.modify (\vec → for_ allCoords (projectCoord w h vec)) grid)
    where
        projectCoord w h vec coord@(x, y, z) =
            let fc      = fl `codeAt` V2 (fromIntegral x) (fromIntegral y)
                lc      = ll `codeAt` V2 (fromIntegral z) (fromIntegral y)
                tc      = tl `codeAt` V2 (fromIntegral x) (fromIntegral z)
                codesEq = length (group [fc, lc, tc]) == 1
                codesEmpty = and $ (==' ') <$> [fc, lc, tc]
                val     = if codesEq && not codesEmpty then pure fc else empty
            in MV.write vec (linCoord3D w h coord) val


-- Works
linCoord3D ∷ Width → Height → (Width, Height, Depth) → Int
linCoord3D (fromIntegral → w) (fromIntegral → h) (x, y, z) = 
    (fromIntegral z * w * h) + (fromIntegral y * w) + fromIntegral x


-- Works
coordLin3D ∷ Width → Height → Int → (Width, Height, Depth)
coordLin3D w h ix = let x = ix `mod` fromIntegral w
                        y = (ix `mod` (fromIntegral w * fromIntegral h)) `div` fromIntegral w
                        z = ix `div` (fromIntegral w * fromIntegral h)
                    in  (fromIntegral x, fromIntegral y, fromIntegral z)


{-
slice ∷ VoxelGrid → Depth → [Maybe Char]
slice vg d =
    let w = vg^.vgSize._1
        h = vg^.vgSize._2
        allCoords = (,,d)
                        <$> [0..w - 1]
                        <*> [0..h - 1]
    in  (V.!) (_vgVoxels vg) . linCoord3D w h <$> allCoords
-}
