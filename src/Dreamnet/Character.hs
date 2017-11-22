{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Character
( Character
, c_name
, newCharacter
) where

import Control.Lens

--------------------------------------------------------------------------------

data Character = Character {
      _c_name ∷ String
    }

makeLenses ''Character

newCharacter ∷ String → Character
newCharacter n = Character n
