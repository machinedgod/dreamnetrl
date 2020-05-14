{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.ObjectStates
( module Dreamnet.Engine.Character
, module Dreamnet.Engine.Conversation
, module Dreamnet.ComputerModel

, ItemTraits(..)

, Material(..)

, WearableItem(..), wiName, wiEquippedAt, wiVolume, wiWeight, wiMaterial
, wiCoverage, wiContainerVolume, wiStoredItems

, AmmoType(..)

, WeaponItem(..), wpiName, wpiDescription, wpiSettings, wpiAmmoType

, AmmoItem(..), amiName, amiType, amiCurrentLoad, amiMaxLoad

, ThrownWeaponItem(..), twiName

, ConsumableItem(..), ciName

, Faction(..)
, States(..), _Prop, _Camera, _Person, _Computer, _Clothes, _Weapon, _Ammo
, _Throwable, _Consumable

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
      _wiName       ∷ String
    , _wiEquippedAt ∷ SlotType
    -- Volume of the item
    , _wiVolume     ∷ Word
    -- Weight of the item
    , _wiWeight     ∷ Float
    -- Material (TODO make a list?)
    , _wiMaterial   ∷ Material
    -- When worn as clothes, what is the coverage percent of the body part?
    , _wiCoverage   ∷ Float

    -- If Nothing, its not a container. If Just x, then what is the container's volume?
    -- Note, does not necessarily have to be less than _wi_volume, eg. belts and clip carriers
    , _wiContainerVolume ∷ Maybe Word
    , _wiStoredItems ∷ [i] -- TODO probably just use a slot, or slots! :-O?
    }
    deriving (Eq, Functor)
makeLenses ''WearableItem


instance Show (WearableItem i) where
    show = _wiName

instance ItemTraits (WearableItem i) where
    isContainer = isJust . _wiContainerVolume


data AmmoType = LaserjetBattery
              deriving (Eq)


-- TODO ammo type as phantom type?
data WeaponItem = WeaponItem {
      _wpiName        ∷ String
    , _wpiDescription ∷ String
    , _wpiSettings    ∷ M.Map String String
    , _wpiAmmoType    ∷ AmmoType
    }
    deriving (Eq)
makeLenses ''WeaponItem


data AmmoItem = AmmoItem {
      _amiName        ∷ String
    , _amiType        ∷ AmmoType
    , _amiCurrentLoad ∷ Word
    , _amiMaxLoad     ∷ Word
    }
    deriving (Eq)
makeLenses ''AmmoItem


newtype ThrownWeaponItem = ThrownWeaponItem {
      _twiName ∷ String
    }
    deriving (Eq)
makeLenses ''ThrownWeaponItem


newtype ConsumableItem = ConsumableItem {
      _ciName ∷ String
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
--
-- TODO should use a single object for all data, to be able to use Prisms w/o relying on tuple lenses
data States = Prop        String String
            | Camera      Faction Word
            | Person      DreamnetCharacter
            | Computer    ComputerData
            | Clothes     (WearableItem States)
            | Weapon      WeaponItem
            | Ammo        AmmoItem
            | Throwable   ThrownWeaponItem
            | Consumable  ConsumableItem
            -- | Pile     States
            deriving(Eq)
makePrisms ''States


instance Show States where
    show (Prop s _)      = s
    show (Camera _ _)    = "camera"
    show (Person ch)     = view chName ch
    show (Computer _)    = "computer"
    show (Clothes wi)    = view wiName wi
    show (Weapon wpi)    = view wpiName wpi
    show (Ammo ami)      = view amiName ami
    show (Throwable twi) = view twiName twi
    show (Consumable ci) = view ciName ci


instance ItemTraits States where
    isContainer (Clothes wi) = isContainer wi
    isContainer _            = False

