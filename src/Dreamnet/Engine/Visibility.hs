{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.Engine.Visibility
( Visibility(..)
) where

import Data.Bool (bool)

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Ord, Show)

instance Semigroup Visibility where
    x <> y = bool y x (x > y)

instance Monoid Visibility where
    mempty = Unknown
