{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.TileData
( ttype
, readBoolProperty
, readStringProperty
) where

import Prelude hiding (read, head)
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


readStringProperty ∷ Int → Tile → String
readStringProperty i = (V.! i) . view t_data

