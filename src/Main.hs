{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Dreamnet.DesignData
import Dreamnet.Dreamnet

import Dreamnet.GamePipes

main ∷ IO ()
main = launchDreamnet defaultDesignData
