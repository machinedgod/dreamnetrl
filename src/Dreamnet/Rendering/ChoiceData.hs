{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Rendering.ChoiceData
( ChoiceData
, cd_options
, cd_currentSelection
, cd_position
, cd_size
, newChoiceData

, selectNext
, selectPrevious
) where


import Control.Lens   (makeLenses, views, (%~))
import Linear         (V2)

import qualified Data.Vector as V (Vector, fromList, length)

--------------------------------------------------------------------------------

data ChoiceData = ChoiceData {
      _cd_options          ∷ V.Vector String
    , _cd_currentSelection ∷ Int

    , _cd_position ∷ V2 Integer
    , _cd_size     ∷ V2 Integer
    }
    deriving (Eq, Show)
makeLenses ''ChoiceData


newChoiceData ∷ V2 Integer → V2 Integer → [String] → ChoiceData
newChoiceData p s o = ChoiceData (V.fromList o) 0 p s

--------------------------------------------------------------------------------

selectNext ∷ ChoiceData → ChoiceData
selectNext cd = let maxI = views cd_options (subtract 1 . V.length) cd
                in  cd_currentSelection %~ min maxI  . (+1) $ cd


selectPrevious ∷ ChoiceData → ChoiceData
selectPrevious = cd_currentSelection %~ max 0 . subtract 1

