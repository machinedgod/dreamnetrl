{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.Engine.Visibility
( Visibility(..)
, VisibleAPI(..)
) where


import Data.Bool                  (bool)

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Ord, Show)

instance Semigroup Visibility where
    x <> y = bool y x (x > y)

instance Monoid Visibility where
    mempty = Unknown

--------------------------------------------------------------------------------

class VisibleAPI v where
    isSeeThrough ∷ v → Bool

