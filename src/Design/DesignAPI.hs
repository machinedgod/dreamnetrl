{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Design.DesignAPI
where

import Control.Lens       (makeLenses, view)
import Control.Monad.Free (Free)
import Linear             (V2)
import Data.Maybe         (isJust)
import Data.Semigroup     ((<>))

import qualified Data.Map as M (Map)

import Dreamnet.Engine.Character            (SlotType(..), Character, ch_name)
import Dreamnet.Engine.ConversationMonad    (ConversationF, ConversationNode(..))

import Design.ComputerModel

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


instance Show (WearableItem i) where
    show wi = _wi_name wi

instance ItemTraits (WearableItem i) where
    isContainer = isJust . _wi_containerVolume

--------------------------------------------------------------------------------

data AmmoType = LaserjetBattery
              deriving (Eq)


data WeaponItem = WeaponItem {
      _wpi_name     ∷ String
    , _wpi_ammoType ∷ AmmoType
    }
    deriving (Eq)


data AmmoItem = AmmoItem {
      _ami_name        ∷ String
    , _ami_type        ∷ AmmoType
    , _ami_currentLoad ∷ Word
    , _ami_maxLoad     ∷ Word
    }
    deriving (Eq)


data ThrownWeaponItem = ThrownWeaponItem {
      _twi_name ∷ String
    }
    deriving (Eq)

--------------------------------------------------------------------------------

newtype Faction = Faction String
                deriving (Eq, Show)


-- TODO Not happy with this development!
-- NOTE this *could* be a record of lists instead, eg.
-- cameras :: [(Faction, Word)], props :: [String], etc
data States = Prop      String
            | Camera    Faction Word
            | Person    DreamnetCharacter
            | Computer  ComputerData
            | Door
            | Mirror
            | Clothes   (WearableItem States)
            | Weapon    WeaponItem
            | Ammo      AmmoItem
            | Throwable ThrownWeaponItem
            | Empty
            deriving (Eq)


instance Show States where
    show (Prop s)        = "A " <> s
    show (Camera _ _)    = "A camera."
    show (Person ch)     = view ch_name ch
    show (Computer _)    = "A computer"
    show Door            = "An generic, common object that switches collidable state when activated."
    show Mirror          = "You're so pretty!"
    show (Clothes wi)    = _wi_name wi
    show (Weapon wpi)    = _wpi_name wpi
    show (Ammo ami)      = _ami_name ami
    show (Throwable twi) = _twi_name twi
    show Empty           = "This, is, uh... nothing."


instance ItemTraits States where
    isContainer (Clothes wi) = isContainer wi
    isContainer _            = False


type DreamnetCharacter = Character States (Free ConversationF ()) Faction 

--------------------------------------------------------------------------------

data GameState = Quit
               | Normal
               | Examination        String
               | ComputerOperation  (V2 Int) Int ComputerData
               | HudTeam            Int
               | HudMessages
               | HudWatch           Int Int
               | Conversation       ConversationNode
               | InventoryUI
               -- | InventoryUI        [String]
               | SkillsUI           DreamnetCharacter
               | EquipmentUI        DreamnetCharacter

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter

    , _dd_dev_startingMap ∷ String
    }
makeLenses ''DesignData

--------------------------------------------------------------------------------
-- TODO can't find shit if placed next to datas :-(
makeLenses ''WearableItem
makeLenses ''WeaponItem
makeLenses ''AmmoItem
makeLenses ''ThrownWeaponItem
