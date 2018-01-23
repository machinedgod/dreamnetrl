{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Dreamnet.DesignData
import Dreamnet.Dreamnet


main ∷ IO ()
main = launchDreamnet defaultDesignData
