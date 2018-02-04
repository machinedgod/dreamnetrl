{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dreamnet.ObjectProperties
( IsPassable(..)
, IsSeeThrough(..)
, Describable(..)
, HasAi(..)
) where


import Data.List (intercalate)
import Linear    (V2)

--------------------------------------------------------------------------------

class IsPassable a where
    isPassable ∷ a → Bool
    allPassable ∷ [a] → Bool -- TODO loosen datatypes
    allPassable = and . fmap isPassable

class IsSeeThrough a where
    isSeeThrough ∷ a → Bool
    areSeeThrough ∷ [a] → Bool
    areSeeThrough = and . fmap isSeeThrough

class Describable a where
    description ∷ a → String
    describeAll ∷ [a] → String
    describeAll = intercalate ", " . fmap description 


class HasAi m a where
    runAi ∷ V2 Int → a → m a

