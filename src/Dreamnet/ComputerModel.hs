{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.ComputerModel
( ComputerAPI(..)
) where


import Control.Lens
import Control.Monad.State

--------------------------------------------------------------------------------

class ComputerAPI c where
    inputChar ∷ Char → c ()
    commitInput ∷ c String

--------------------------------------------------------------------------------

newtype ComputerData = ComputerData {
      _cd_input ∷ String
    }

makeLenses ''ComputerData

--------------------------------------------------------------------------------

newtype ComputerM a = ComputerM { runComputerM ∷ State ComputerData a }
                    deriving (Functor, Applicative, Monad, MonadState ComputerData) 


newComputerModel ∷ ComputerM ()
newComputerModel = return ()
