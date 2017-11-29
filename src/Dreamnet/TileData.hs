{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.TileData
( ttype
, doorPassable
, stairsUp
, itemName
, personName
, propPassable
, propSeeThrough
) where

import Prelude hiding (read, head)
import Safe

import Control.Lens (view, (^.))
import qualified Data.Vector as V
import Dreamnet.TileMap

--------------------------------------------------------------------------------

ttype ∷ Tile → String
ttype = V.head . view t_data
{-# INLINE ttype #-}


doorPassable ∷ Tile → Bool
doorPassable t = if ttype t == "Door"
                     then readNote "Door data has no passable entry!" $ (t^.t_data) V.! 1
                     else error "Not a door tile!"
{-# INLINE doorPassable #-}

stairsUp ∷ Tile → Bool
stairsUp t = if ttype t == "Stairs"
                 then readNote "Stairs have no direction entry!" $ (t^.t_data) V.! 1
                 else error "Not a stairs tile!"


itemName ∷ Tile → String
itemName t = if ttype t == "Item"
                 then (t^.t_data) V.! 1
                 else error "Not an item!"
{-# INLINE itemName #-}


personName ∷ Tile → String
personName t = if ttype t == "Person"
                   then (t^.t_data) V.! 1
                   else error "Not a person!"
{-# INLINE personName #-}
 

propPassable ∷ Tile → Bool
propPassable t = if ttype t == "Prop"
                     then readNote "Prop has no passable entry!" $ (t^.t_data) V.! 2
                     else error "Not a prop!"
{-# INLINE propPassable #-}



propSeeThrough ∷ Tile → Bool
propSeeThrough t = if ttype t == "Prop"
                       then readNote "Prop has no passable entry!" $ (t^.t_data) V.! 2
                       else error "Not a prop!"
{-# INLINE propSeeThrough #-}

