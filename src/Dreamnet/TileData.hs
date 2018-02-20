{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.TileData
( ttype
, readBoolProperty
, readWordProperty
, readStringProperty
) where

import Safe

import Control.Lens
import qualified Data.Vector as V
import Dreamnet.TileMap

--------------------------------------------------------------------------------

ttype ∷ Tile → String
ttype = V.head . view t_data
{-# INLINE ttype #-}


readBoolProperty ∷ Int → Tile → Bool
readBoolProperty i = readNote "Failed to read Bool property " . (V.! i) . view t_data


readWordProperty ∷ Int → Tile → Word
readWordProperty i = readNote "Failed to read Word property " . (V.! i) . view t_data


readStringProperty ∷ Int → Tile → String
readStringProperty i = (V.! i) . view t_data

