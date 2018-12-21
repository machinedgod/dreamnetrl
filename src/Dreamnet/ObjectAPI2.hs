{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.ObjectAPI2
( ObjectDSL, runObjectDSL
)
where


import Control.Monad.Reader (Reader, runReader)

import Dreamnet.World


{-
Design notes
* DSL requires objects to return new versions of themselves
* Allow reading of environment data
* Allow posting messages to other object update queues (absolute coordinate is the ID)
* Modify gameloop to process messages at the beginning (or at the end?)
-}

newtype ObjectDSL a = ObjectDSL (a → Reader World a)


runObjectDSL ∷ World → ObjectDSL a → a → a
runObjectDSL w (ObjectDSL f) x = runReader (f x) w
