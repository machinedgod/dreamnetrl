{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Dreamnet.Character
( Character
, ch_name
, ch_leftHand
, ch_rightHand
, newCharacter
) where

import Control.Lens

import Dreamnet.Item

--------------------------------------------------------------------------------

data Character = Character {
      _ch_name      ∷ String
    , _ch_leftHand  ∷ Slot 'Hand
    , _ch_rightHand ∷ Slot 'Hand

    , _ch_torso ∷ Slot 'Torso
    }
    deriving (Show)

makeLenses ''Character


newCharacter ∷ String → Character
newCharacter n = Character n empty empty empty
    where
        empty = Slot Nothing

