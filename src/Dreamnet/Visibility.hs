{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.Visibility
( Visibility(..)
) where

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Ord, Show)

