{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.Visibility
( Visibility(..)
) where

import Data.Bool (bool)

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Ord, Show)

instance Monoid Visibility where
    mempty = Unknown
    x `mappend` y = bool y x (x > y)
