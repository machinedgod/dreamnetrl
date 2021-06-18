{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnicodeSyntax            #-}

module Dreamnet.Engine.Iteration
where


import Data.Singletons    (fromSing)
import Data.Singletons.TH (genSingletons)


data Iteration 
    = Next 
    | Previous
    deriving (Eq, Ord, Show, Read, Enum)
$(genSingletons [ ''Iteration ])


itToInt' ∷ SIteration i → Int
itToInt' = itToInt . fromSing


itToInt ∷ Iteration → Int
itToInt Next     =  1
itToInt Previous = -1

