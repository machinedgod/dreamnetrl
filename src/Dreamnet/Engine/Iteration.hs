{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.Engine.Iteration
where


import Data.Singletons
import Data.Singletons.TH


data Iteration = Next | Previous
               deriving (Eq, Ord, Show, Read, Enum)
$(genSingletons [ ''Iteration ])


itToInt' ∷ Sing (i ∷ Iteration) → Int
itToInt' = itToInt . fromSing

itToInt ∷ Iteration → Int
itToInt Next     =  1
itToInt Previous = -1

