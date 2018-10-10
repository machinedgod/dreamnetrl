{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.Engine.Iteration
where


import Data.Singletons    (fromSing)
import Data.Singletons.TH (genSingletons)


data Iteration = Next | Previous
               deriving (Eq, Ord, Show, Read, Enum)
$(genSingletons [ ''Iteration ])


itToInt' ∷ SIteration i → Int
itToInt' = itToInt . fromSing


itToInt ∷ Iteration → Int
itToInt Next     =  1
itToInt Previous = -1

