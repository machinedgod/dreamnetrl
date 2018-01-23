{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Dreamnet.Item
( SlotType(..)
, Slot(Slot)
, s_item
) where

import Control.Lens (makeLenses)

--------------------------------------------------------------------------------

data SlotType = Hand
              | Torso

newtype Slot (t ∷ SlotType) a = Slot {
      _s_item ∷ Maybe a
    }
    deriving (Eq, Show)

makeLenses ''Slot

