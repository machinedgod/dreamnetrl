{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Rendering.ChoiceData
( ChoiceData, cdOptions, cdCurrentSelection, cdPosition, cdSize
, newChoiceData, selectNext, selectPrevious
) where


import Control.Lens   (makeLenses, views, (%~))
import Linear         (V2)

import qualified Data.Vector as V (Vector, fromList, length)

--------------------------------------------------------------------------------

data ChoiceData = ChoiceData {
      _cdOptions          ∷ V.Vector String
    , _cdCurrentSelection ∷ Int

    , _cdPosition ∷ V2 Integer
    , _cdSize     ∷ V2 Integer
    }
    deriving (Eq, Show)
makeLenses ''ChoiceData


newChoiceData ∷ V2 Integer → V2 Integer → [String] → ChoiceData
newChoiceData p s o = ChoiceData (V.fromList o) 0 p s

--------------------------------------------------------------------------------

selectNext ∷ ChoiceData → ChoiceData
selectNext cd = let maxI = views cdOptions (subtract 1 . V.length) cd
                in  cdCurrentSelection %~ min maxI  . (+1) $ cd


selectPrevious ∷ ChoiceData → ChoiceData
selectPrevious = cdCurrentSelection %~ max 0 . subtract 1

