{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings #-}

module Main
where

import Dreamnet.Dreamnet

main ∷ IO ()
main = launchDreamnet defaultDesignData
