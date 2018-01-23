{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.MapObject
( Object(..)
, isPassable
, isSeeThrough

, objectFromTile

, objectToChar
, objectToMat
) where


import Control.Lens   ((^.))
import Data.Semigroup (Semigroup, (<>))
import Data.Bool      (bool)
import Data.Maybe     (fromMaybe)

import qualified Data.Map as M  (Map, lookup)

import Dreamnet.ObjectProperties
import Dreamnet.Conversation     (ConversationNode)
import Dreamnet.Character        (Character, ch_name)
import Dreamnet.DesignData       (DesignData, dd_characters, dd_defaultRedshirt)
import Dreamnet.TileMap          (Tile, t_char)
import Dreamnet.TileData         (ttype, readBoolProperty, readStringProperty)

--------------------------------------------------------------------------------

type Passable   = Bool
type SeeThrough = Bool
type Opened     = Bool
type GoingUp    = Bool
type Name       = String
type Material   = String


-- TODO Container should be a flag on prop or something?
-- TODO eventually, all objects will be squashed into the general
--      object that cojoins all their different properties
--      Basically, a Prop with all the interaction features out-coded
data Object = Base      Char Passable SeeThrough
            | Door      Opened
            | Stairs    GoingUp
            | Prop      Char Name Material Passable SeeThrough
            | Person    (Character String ConversationNode)
            | Computer
            | ItemO     String
            | Union Object Object
            deriving (Eq, Show)


instance Semigroup Object where
    o1 <> o2 = Union o1 o2

instance IsPassable Object where
    isPassable (Base _ p _)     = p
    isPassable (Door o)         = o
    isPassable (Stairs _)       = True
    isPassable (Prop _ _ _ p _) = p
    isPassable (Person _)       = False -- TODO Check if allied
    isPassable Computer         = False
    isPassable (ItemO _)        = True
    isPassable (Union o1 o2)    = isPassable o1 && isPassable o2
    {-# INLINE isPassable #-}

instance Describable Object where
    description (Base _ _ _)     = "<you-should-not-be-able-to-examine-base-objects!>"
    description (Door o)         = "Just a common door. They're " <> bool "closed." "opened." o
    description (Stairs t)       = "If map changing would've been coded in, you would use these to go " <> bool "down." "up." t
    description (Prop _ n _ _ _) = "A " <> n <> "."
    description (Person c)       = "Its " <> (c^.ch_name) <> "."
    description Computer         = "Your machine. You wonder if Devin mailed you about the job."
    description (ItemO n)        = "A " <> n <> "."
    description (Union o1 o2)    = let md  = description o1
                                       md2 = description o2
                                   in  md `mappend` ", " `mappend` md2
    {-# INLINE description #-}

instance IsSeeThrough Object where
    isSeeThrough (Base _ _ s)     = s
    isSeeThrough (Door o)         = o
    isSeeThrough (Stairs _)       = True
    isSeeThrough (Prop _ _ _ _ s) = s
    isSeeThrough (Person _)       = True
    isSeeThrough Computer         = True
    isSeeThrough (ItemO _)        = True
    isSeeThrough (Union o1 o2)    = isSeeThrough o1 && isSeeThrough o2
    {-# INLINE isSeeThrough #-}



objectFromTile ∷ DesignData → Tile → Object
objectFromTile _  t@(ttype → "Base")     = Base (t^.t_char) (1 `readBoolProperty` t)  (2 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Door")     = Door (1 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Stairs")   = Stairs (1 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Prop")     = Prop (t^.t_char) (1 `readStringProperty` t) (4 `readStringProperty` t) (2 `readBoolProperty` t)  (3 `readBoolProperty` t)
objectFromTile dd t@(ttype → "Person")   = let name      = 1 `readStringProperty` t
                                               maybeChar = M.lookup name (dd^.dd_characters)
                                           in  Person (fromMaybe (dd^.dd_defaultRedshirt) maybeChar)
objectFromTile _    (ttype → "Spawn")    = Base '.' True True -- TODO shitty hardcoding, spawns should probably be generalized somehow!
objectFromTile _    (ttype → "Computer") = Computer
objectFromTile _  t@(ttype → "Item")     = ItemO (1 `readStringProperty` t)
objectFromTile _  t                      = error $ "Can't convert Tile type into Object: " <> show t
{-# INLINE objectFromTile #-}


objectToChar ∷ Object → Char
objectToChar (Base c _ _)     = c
objectToChar (Door o)         = bool '+' '\'' o
objectToChar (Stairs u)       = bool '<' '>' u
objectToChar (Prop c _ _ _ _) = c
objectToChar (Person _)       = '@' -- TODO if ally, color green
objectToChar Computer         = '$'
objectToChar (ItemO _)        = '['
objectToChar (Union _ o2)     = objectToChar o2
{-# INLINE objectToChar #-}


objectToMat ∷ M.Map String a → a → Object → a
--objectToMat ∷ M.Map String [C.Attribute] → Object → [C.Attribute] → [C.Attribute]
objectToMat _    def (Base _ _ _)     = def
objectToMat mats def (Door _)         = fromMaybe def $ "wood" `M.lookup` mats
objectToMat mats def (Stairs _)       = fromMaybe def $ "wood" `M.lookup` mats
objectToMat mats def (Prop _ _ m _ _) = fromMaybe def $ m `M.lookup` mats
objectToMat _    def (Person _)       = def
objectToMat mats def  Computer        = fromMaybe def $ "metal" `M.lookup` mats
objectToMat mats def (ItemO _)        = fromMaybe def $ "blue plastic" `M.lookup` mats
objectToMat mats def (Union _ o2)     = objectToMat mats def o2
{-# INLINE objectToMat #-}

