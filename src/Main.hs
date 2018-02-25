{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Control.Monad.Random (evalRandT, mkStdGen)

import Dreamnet.Dreamnet
import DesignData


main ∷ IO ()
main = 
    let seed = 0
    in  evalRandT defaultDesignData (mkStdGen seed) >>= launchDreamnet
