{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.Engine.ProcGen.Common
( blit
)
where


import Safe            (fromJustNote)
import Numeric.Natural
import Linear

import qualified Data.Vector as V

import Dreamnet.Engine.CoordVector


--------------------------------------------------------------------------------

blit ∷ Width → Height → V.Vector a → Natural → Natural → Width → Height → V.Vector a → V.Vector a
blit sw sh sl (fromIntegral → dx) (fromIntegral → dy) dw _ = V.imap (replaceTile . coordLin' dw)
    where
        --replaceTile ∷ V2 Int → a → a
        replaceTile v c =
            let nv@(V2 x' y') = v - V2 dx dy
            in  if x' < 0 || x' >= fromIntegral sw || y' < 0 || y' >= fromIntegral sh
                    then c
                    else fromJustNote "Unreasonable coordinate!" (sl V.!? linCoord' sw nv)

