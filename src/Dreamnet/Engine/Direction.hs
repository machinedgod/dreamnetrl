{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Dreamnet.Engine.Direction
where


import Linear              (V2(V2), normalize, Epsilon)
import Data.Singletons     (fromSing)
import Data.Singletons.TH  (genSingletons)


data Direction = North
               | NorthEast
               | East
               | SouthEast
               | South
               | SouthWest
               | West
               | NorthWest
               deriving (Eq, Ord, Show, Read, Enum)
$(genSingletons [ ''Direction ])


dirToVec' ∷ SDirection d → V2 Int
dirToVec' = dirToVec . fromSing


dirToVec ∷ Direction → V2 Int
dirToVec North     = V2  0  -1
dirToVec NorthEast = V2  1  -1
dirToVec East      = V2  1   0
dirToVec SouthEast = V2  1   1
dirToVec South     = V2  0   1
dirToVec SouthWest = V2 -1   1
dirToVec West      = V2 -1   0
dirToVec NorthWest = V2 -1  -1

--------------------------------------------------------------------------------

vecToDir ∷ (RealFrac a, Floating a, Epsilon a) ⇒ V2 a → Maybe Direction
vecToDir = toDir . fmap round . normalize
    where
        toDir ∷ V2 Int → Maybe Direction
        toDir (V2  0  -1) = Just North
        toDir (V2  1  -1) = Just NorthEast
        toDir (V2  1   0) = Just East
        toDir (V2  1   1) = Just SouthEast
        toDir (V2  0   1) = Just South
        toDir (V2 -1   1) = Just SouthWest
        toDir (V2 -1   0) = Just West
        toDir (V2 -1  -1) = Just NorthWest
        toDir _           = Nothing

