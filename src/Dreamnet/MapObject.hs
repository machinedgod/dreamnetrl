{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.MapObject
( Object(..)
, isPassable
, isSeeThrough

, objectFromTile

, objectToChar
, objectToMat
) where


import Control.Lens   ((^.), view)
import Data.Semigroup ((<>))
import Data.Bool      (bool)
import Data.Maybe     (fromMaybe)
import Data.Char      (intToDigit)

import qualified Data.Map as M  (Map, lookup)


import Dreamnet.ObjectProperties
import Dreamnet.Entity           (e_position, e_object)
import Dreamnet.Conversation     (ConversationNode)
import Dreamnet.Character        (Character, ch_name, Item(Item))
import Dreamnet.DesignData       (DesignData, dd_characters, dd_defaultRedshirt)
import Dreamnet.TileMap          (Tile, t_char)
import Dreamnet.TileData         (ttype, readBoolProperty, readStringProperty)
import Dreamnet.World            (WorldReadAPI(..), WorldAPI(..), changeObject_)

--------------------------------------------------------------------------------

type Passable   = Bool
type SeeThrough = Bool
type Opened     = Bool
type GoingUp    = Bool
type Name       = String
type Material   = String
type AlarmLevel = Int


-- TODO Container should be a flag on prop or something?
-- TODO eventually, all objects will be squashed into the general
--      object that cojoins all their different properties
--      Basically, a Prop with all the interaction features out-coded
data Object = Base      Char Passable SeeThrough
            | Door      Opened
            | Stairs    GoingUp
            | Prop      Char Name Material Passable SeeThrough
            | Person    (Character Item ConversationNode)
            | Camera    AlarmLevel
            | Computer
            | ItemO     Item
            deriving (Eq, Show)


instance (Applicative w, WorldReadAPI Object b (Character c d) w) ⇒ IsPassable w Object where
    isPassable (Base _ p _)     = pure p
    isPassable (Door o)         = pure o
    isPassable (Stairs _)       = pure True
    isPassable (Prop _ _ _ p _) = pure p
    isPassable (Person c)       = or . fmap nameMatches <$> team
        where
            nameMatches c' = view ch_name c == view (e_object.ch_name) c'
    isPassable (Camera _)       = pure True
    isPassable Computer         = pure False
    isPassable (ItemO _)        = pure True
    {-# INLINE isPassable #-}


instance Describable Object where
    description (Base _ _ _)     = "<base>"
    description (Door o)         = "Just a common door. They're " <> bool "closed." "opened." o
    description (Stairs t)       = "If map changing would've been coded in, you would use these to go " <> bool "down." "up." t
    description (Prop _ n _ _ _) = "A " <> n <> "."
    description (Person c)       = "Its " <> (c^.ch_name) <> "."
    description (Camera a)       = bool
                                       "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care."
                                       "A camera is frantically following your motion as you move around the room and blinking a little red LED. You pessimistically assume you must've been detected!"
                                       (a > 5)
    description Computer         = "Your machine. You wonder if Devin mailed you about the job."
    description (ItemO (Item n)) = "A " <> n <> "."
    {-# INLINE description #-}


instance IsSeeThrough Object where
    isSeeThrough (Base _ _ s)     = s
    isSeeThrough (Door o)         = o
    isSeeThrough (Stairs _)       = True
    isSeeThrough (Prop _ _ _ _ s) = s
    isSeeThrough (Person _)       = True
    isSeeThrough (Camera _)       = True
    isSeeThrough Computer         = True
    isSeeThrough (ItemO _)        = True
    {-# INLINE isSeeThrough #-}


-- TODO the undecideable instance here is really scary :-(
-- TODO I *really* need to think about how objects perceive and affect their
-- environment. I assume this'll have to start from the player, and extract a
-- whole lot of player code into something that can be controlled either by AI,
-- or by keyboard
--instance (Monad m, WorldReadAPI Object b c m) ⇒ HasAi m Object where
instance (Monad w, WorldAPI Object b c w) ⇒ HasAi w Object where
    runAi v c@(Camera l) = do
        pv         ← view e_position <$> active  -- TODO whole team!
        seesPlayer ← and . fmap snd <$> castVisibilityRay v pv
        if seesPlayer
            then changeObject_ v c (Camera (min 9 (l + 1)))
            else changeObject_ v c (Camera (max 0 (l - 1)))
    runAi _ _ = pure ()


objectFromTile ∷ DesignData → Tile → Object
objectFromTile _  t@(ttype → "Base")     = Base (t^.t_char) (1 `readBoolProperty` t)  (2 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Door")     = Door (1 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Stairs")   = Stairs (1 `readBoolProperty` t)
objectFromTile _  t@(ttype → "Prop")     = Prop (t^.t_char) (1 `readStringProperty` t) (4 `readStringProperty` t) (2 `readBoolProperty` t)  (3 `readBoolProperty` t)
objectFromTile dd t@(ttype → "Person")   = let name      = 1 `readStringProperty` t
                                               maybeChar = M.lookup name (dd^.dd_characters)
                                           in  Person (fromMaybe (dd^.dd_defaultRedshirt) maybeChar)
objectFromTile _    (ttype → "Spawn")    = Base '.' True True -- TODO shitty hardcoding, spawns should probably be generalized somehow!
objectFromTile _    (ttype → "Camera")   = Camera 0
objectFromTile _    (ttype → "Computer") = Computer
objectFromTile _  t@(ttype → "Item")     = ItemO $ Item (1 `readStringProperty` t)
objectFromTile _  t                      = error $ "Can't convert Tile type into Object: " <> show t
{-# INLINE objectFromTile #-}


-- TODO Errrrrr, this should be done through the tileset???
objectToChar ∷ Object → Char
objectToChar (Base c _ _)     = c
objectToChar (Door o)         = bool '+' '\'' o
objectToChar (Stairs u)       = bool '<' '>' u
objectToChar (Prop c _ _ _ _) = c
objectToChar (Person _)       = '@' -- TODO if ally, color green
objectToChar (Camera l)       = intToDigit l
--objectToChar (Camera _)       = '*'
objectToChar Computer         = '$'
objectToChar (ItemO _)        = '['
{-# INLINE objectToChar #-}


-- TODO Errrrrr, this should be done through the tileset???
objectToMat ∷ M.Map String a → a → Object → a
--objectToMat ∷ M.Map String [C.Attribute] → Object → [C.Attribute] → [C.Attribute]
objectToMat _    def (Base _ _ _)     = def
objectToMat mats def (Door _)         = fromMaybe def $ "wood" `M.lookup` mats
objectToMat mats def (Stairs _)       = fromMaybe def $ "wood" `M.lookup` mats
objectToMat mats def (Prop _ _ m _ _) = fromMaybe def $ m `M.lookup` mats
objectToMat _    def (Person _)       = def
objectToMat mats def (Camera l)
    | l > 2 && l <= 7 = fromMaybe def $ ("yellow light" `M.lookup` mats)
    | l > 7           = fromMaybe def $ ("red light" `M.lookup` mats)
    | otherwise       = fromMaybe def $ ("green light" `M.lookup` mats)
objectToMat mats def  Computer        = fromMaybe def $ "metal" `M.lookup` mats
objectToMat mats def (ItemO _)        = fromMaybe def $ "blue plastic" `M.lookup` mats
{-# INLINE objectToMat #-}

