{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.ObjectStates
( module Dreamnet.Engine.Character
, module Dreamnet.Engine.Conversation
, module Dreamnet.ComputerModel

, ItemTraits(..)

, Material(..)

, WearableItem(..), wi_name, wi_equippedAt, wi_volume, wi_weight, wi_material,
  wi_coverage, wi_containerVolume, wi_storedItems

, AmmoType(..)

, WeaponItem(..), wpi_name, wpi_description, wpi_settings, wpi_ammoType

, AmmoItem(..), ami_name, ami_type, ami_currentLoad, ami_maxLoad

, ThrownWeaponItem(..), twi_name

, ConsumableItem(..), ci_name

, Faction(..)
, States(..), _Prop, _Camera, _Person, _Computer, _Clothes, _Weapon, _Ammo,
  _Throwable, _Consumable

, DreamnetCharacter
)
where


import Control.Lens       (makeLenses, makePrisms, view)
import Control.Monad.Free (Free(..))
import Data.Maybe         (isJust)

import qualified Data.Map as M

import Dreamnet.Engine.Character
import Dreamnet.Engine.Conversation
import Dreamnet.ComputerModel

--------------------------------------------------------------------------------

class ItemTraits i where
    isContainer ∷ i → Bool
    -- TODO container of what type?

data Material = Kevlar
              | Polyester
              | Cotton
              deriving (Eq, Show)


-- TODO I'm just like making shit up here, to define what wearable item is
data WearableItem i = WearableItem {
      _wi_name       ∷ String
    , _wi_equippedAt ∷ SlotType
    -- Volume of the item
    , _wi_volume     ∷ Word
    -- Weight of the item
    , _wi_weight     ∷ Float
    -- Material (TODO make a list?)
    , _wi_material   ∷ Material
    -- When worn as clothes, what is the coverage percent of the body part?
    , _wi_coverage   ∷ Float

    -- If Nothing, its not a container. If Just x, then what is the container's volume?
    -- Note, does not necessarily have to be less than _wi_volume, eg. belts and clip carriers
    , _wi_containerVolume ∷ Maybe Word
    , _wi_storedItems ∷ [i] -- TODO probably just use a slot, or slots! :-O?
    }
    deriving (Eq, Functor)
makeLenses ''WearableItem


instance Show (WearableItem i) where
    show = _wi_name

instance ItemTraits (WearableItem i) where
    isContainer = isJust . _wi_containerVolume


data AmmoType = LaserjetBattery
              deriving (Eq)


-- TODO ammo type as phantom type?
data WeaponItem = WeaponItem {
      _wpi_name        ∷ String
    , _wpi_description ∷ String
    , _wpi_settings    ∷ M.Map String String
    , _wpi_ammoType    ∷ AmmoType
    }
    deriving (Eq)
makeLenses ''WeaponItem


data AmmoItem = AmmoItem {
      _ami_name        ∷ String
    , _ami_type        ∷ AmmoType
    , _ami_currentLoad ∷ Word
    , _ami_maxLoad     ∷ Word
    }
    deriving (Eq)
makeLenses ''AmmoItem


newtype ThrownWeaponItem = ThrownWeaponItem {
      _twi_name ∷ String
    }
    deriving (Eq)
makeLenses ''ThrownWeaponItem


newtype ConsumableItem = ConsumableItem {
      _ci_name ∷ String
    }
    deriving(Eq)
makeLenses ''ConsumableItem

--------------------------------------------------------------------------------

newtype Faction = Faction String
                deriving (Eq, Show)


type DreamnetCharacter = Character States (Free (ConversationF States) ()) Faction

-- TODO Not happy with this development!
-- NOTE this *could* be a record of lists instead, eg.
-- cameras :: [(Faction, Word)], props :: [String], etc
-- Also, if I use a table of stats, then I could encode few different types
-- of objects together?
-- It could be that states support few general programs, that are then
-- configurable via properties and switches. Then, when making a map, object Type
-- would determine the program to run, and tile properties would be switches and
-- additional parameters
--
-- So for example, Camera could utilize some generic 'perception' program that
-- somehow signals some other object, that's set up with switches?
data States = Prop        String String
            | Camera      Faction Word
            | Person      DreamnetCharacter
            | Computer    ComputerData
            | Clothes     (WearableItem States)
            | Weapon      WeaponItem
            | Ammo        AmmoItem
            | Throwable   ThrownWeaponItem
            | Consumable  ConsumableItem
            deriving(Eq)
makePrisms ''States


instance Show States where
    show (Prop s _)      = s
    show (Camera _ _)    = "camera"
    show (Person ch)     = view ch_name ch
    show (Computer _)    = "computer"
    show (Clothes wi)    = view wi_name wi
    show (Weapon wpi)    = view wpi_name wpi
    show (Ammo ami)      = view ami_name ami
    show (Throwable twi) = view twi_name twi
    show (Consumable ci) = view ci_name ci


instance ItemTraits States where
    isContainer (Clothes wi) = isContainer wi
    isContainer _            = False

