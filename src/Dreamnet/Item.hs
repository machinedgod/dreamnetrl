{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Dreamnet.Item
( Item(Item)
, i_name
, SlotType(..)
, Slot(Slot)
, s_item
) where

import Control.Lens

--------------------------------------------------------------------------------

newtype Item = Item {
      _i_name ∷ String
    }
    deriving (Eq, Show)

makeLenses ''Item

--------------------------------------------------------------------------------

data SlotType = Hand
              | Torso

newtype Slot (t ∷ SlotType) = Slot {
      _s_item ∷ Maybe Item
    }
    deriving (Eq, Show)

makeLenses ''Slot

