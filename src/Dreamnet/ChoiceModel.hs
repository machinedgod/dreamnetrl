{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}


module Dreamnet.ChoiceModel
( ChoiceModel(ChoiceModel)
, cm_options
, cm_currentSelection

, ChoiceM
, controlChoice
) where


import Control.Lens
import Control.Monad.State
import qualified Data.Vector as Vec

import Dreamnet.Input

--------------------------------------------------------------------------------

data ChoiceModel = ChoiceModel {
      _cm_options ∷ Vec.Vector String
    , _cm_currentSelection ∷ Word
    }

makeLenses ''ChoiceModel

--------------------------------------------------------------------------------

type ChoiceM a = State ChoiceModel a 

--------------------------------------------------------------------------------

controlChoice ∷ UIEvent → ChoiceM Bool
controlChoice MoveUp = do
    cm_currentSelection -= 1 
    return False
controlChoice MoveDown = do
    maxV ← uses cm_options (\v → fromIntegral $ Vec.length v - 1) 
    cm_currentSelection %= min maxV . (+1)
    return False
controlChoice SelectChoice = return True
controlChoice Back = return False

