{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dreamnet.ObjectProperties
( IsPassable(..)
, IsSeeThrough(..)
, Describable(..)
, HasAi(..)
) where


class IsPassable a where
    isPassable ∷ a → Bool

class IsSeeThrough a where
    isSeeThrough ∷ a → Bool

class Describable a where
    description ∷ a → String

class HasAi m a where
    runAi ∷ a → m a
