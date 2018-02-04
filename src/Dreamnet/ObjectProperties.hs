{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dreamnet.ObjectProperties
( IsPassable(..)
, IsSeeThrough(..)
, Describable(..)
, HasAi(..)
) where


import Linear (V2)

--------------------------------------------------------------------------------

class IsPassable a where
    isPassable ∷ a → Bool

class IsSeeThrough a where
    isSeeThrough ∷ a → Bool

class Describable a where
    description ∷ a → String

class HasAi m a where
    runAi ∷ V2 Int → a → m a

