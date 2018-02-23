{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Control.Monad.Random (evalRand, mkStdGen)

import Dreamnet.DesignData
import Dreamnet.Dreamnet


main ∷ IO ()
main = 
    let seed = 0
        dd   = evalRand defaultDesignData (mkStdGen seed)
    in  launchDreamnet dd
