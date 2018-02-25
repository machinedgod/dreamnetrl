{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.TileData
( ttype
, readBoolProperty
, readWordProperty
, readStringProperty
) where

import Safe

import Control.Lens   (view)
import Data.Maybe     (fromMaybe)
import Data.Semigroup ((<>))

import qualified Data.Vector as V ((!?))

import Dreamnet.TileMap

--------------------------------------------------------------------------------

ttype ∷ Tile → String
ttype = fromMaybe (error "Tile type not set!") . (V.!? 0) . view t_data
{-# INLINE ttype #-}


readBoolProperty ∷ Int → Tile → Bool
readBoolProperty i = maybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") (readNote "Failed to read Bool property ") . (V.!? i) . view t_data


readWordProperty ∷ Int → Tile → Word
readWordProperty i = maybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") (readNote "Failed to read Word property ") . (V.!? i) . view t_data


readStringProperty ∷ Int → Tile → String
readStringProperty i = fromMaybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") . (V.!? i) . view t_data

