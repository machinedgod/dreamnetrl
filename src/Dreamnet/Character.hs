{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Character
( Character
, ch_name
, ch_leftHand
, ch_rightHand
, newCharacter
) where

import Control.Lens

--------------------------------------------------------------------------------

newtype Item = Item {
      _i_name ∷ String
    }
    deriving (Show)

makeLenses ''Item

data EquipmentSlot = EquipmentSlot {
      _es_name ∷ String
    , _es_item ∷ Maybe Item
    }
    deriving (Show)

makeLenses ''EquipmentSlot

data Character = Character {
      _ch_name      ∷ String
    , _ch_leftHand  ∷ EquipmentSlot
    , _ch_rightHand ∷ EquipmentSlot
    }
    deriving (Show)

makeLenses ''Character


newCharacter ∷ String → Character
newCharacter n = Character n leftHand rightHand
    where
        leftHand  = EquipmentSlot "leftHand" Nothing
        rightHand = EquipmentSlot "rightHand" Nothing

