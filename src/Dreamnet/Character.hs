{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Dreamnet.Character
( ItemTraits(..)
, Item(..)

, SlotType(..)
, Slot(..)
, SlotWrapper(..)

, CombatSkills
, ElectronicsSkills
, CommunicationSkills
, Character
, ch_name
, ch_leftHand
, ch_rightHand
, ch_torso
, ch_conversation
, ch_experience
, ch_combat
, ch_electronics
, ch_social
, newCharacter

, equipmentSlots
, equippedSlots
, equippedContainers
) where


import Control.Lens (makeLenses, (^.))
import Data.Maybe   (isJust)

--------------------------------------------------------------------------------

class ItemTraits i where
    isContainer ∷ i → Bool

newtype Item = Item String
             deriving (Eq, Show)

instance ItemTraits Item where
    isContainer = const False

--------------------------------------------------------------------------------

data SlotType = Hand
              | Torso


newtype Slot (t ∷ SlotType) a = Slot (Maybe a)

deriving instance (Show a) ⇒ Show (Slot t a)
deriving instance (Eq a) ⇒ Eq (Slot t a)


-- Cannot use record syntax due to escaped type variables
data SlotWrapper a = forall t. SlotWrapper (Slot t a)

deriving instance (Show a) ⇒ Show (SlotWrapper a)

instance (Eq a) ⇒ Eq (SlotWrapper a) where
    (SlotWrapper (Slot i)) == (SlotWrapper (Slot i')) = i == i'

--------------------------------------------------------------------------------

-- All combat-related skills
data CombatSkills = CombatSkills {
      _cs_remainingPoints ∷ Word

    , _cs_melee      ∷ Word
    , _cs_barehanded ∷ Word
    , _cs_knives     ∷ Word
    , _cs_swords     ∷ Word
    , _cs_staves     ∷ Word
    , _cs_maces      ∷ Word

    , _cs_ranged    ∷ Word
    , _cs_guns      ∷ Word
    , _cs_smgs      ∷ Word
    , _cs_shotguns  ∷ Word
    , _cs_assault   ∷ Word
    , _cs_sniper    ∷ Word
    , _cs_bows      ∷ Word
    , _cs_crossbows ∷ Word
    , _cs_plasma    ∷ Word
    , _cs_lasers    ∷ Word
    }
    deriving (Eq, Show)

--makeLenses ''CombatSkills

-- Holds all the skills needed to deal with electronics
data ElectronicsSkills = ElectronicsSkills {
      _es_remainingPoints ∷ Word

    , _es_juryrig ∷ Word
    , _es_modify  ∷ Word
    , _es_analyze ∷ Word
    }
    deriving (Eq, Show)

--makeLenses ''ElectronicsSkills


data CommunicationSkills = CommunicationSkills {
      _ss_remainingPoints ∷ Word

    , _ss_smallTalk           ∷ Word
    , _ss_bodyLanguageReading ∷ Word
    , _ss_bodyLanguageControl ∷ Word
    , _ss_trade               ∷ Word
    , _ss_interrogation       ∷ Word
    , _ss_charm               ∷ Word
    }
    deriving (Eq, Show)

--makeLenses ''CommunicationSkills


data Character a b = Character {
      _ch_name      ∷ String

    , _ch_leftHand  ∷ Slot 'Hand a
    , _ch_rightHand ∷ Slot 'Hand a
    , _ch_torso     ∷ Slot 'Torso a

    , _ch_conversation ∷ b

    -- Earned only through missions and combat,
    -- represents general experience
    -- Used to earn skillpoints in each of the skill branches
    , _ch_experience ∷ Word

    -- Skilltrees *only* affect chances of performing a certain task
    -- To train to a certain level, you need to purchase skillpoints
    -- by spending general experience, and then find a person willing to
    -- train you, who has that skill equal or above desired level
    , _ch_combat      ∷ CombatSkills
    , _ch_electronics ∷ ElectronicsSkills
    , _ch_social      ∷ CommunicationSkills
    }
    deriving (Show)

makeLenses ''Character


instance Eq (Character a b) where
    ch1 == ch2 = ch1 ^. ch_name == ch2 ^. ch_name


newCharacter ∷ String → b → Character a b
newCharacter n cn = Character n empty empty empty cn 0 cs es ss
    where
        empty = Slot Nothing
        cs    = CombatSkills 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        es    = ElectronicsSkills 0 0 0 0
        ss    = CommunicationSkills 0 0 0 0 0 0 0


equipmentSlots ∷ Character a b → [SlotWrapper a]
equipmentSlots ch = [ SlotWrapper (ch ^. ch_leftHand)
                    , SlotWrapper (ch ^. ch_rightHand)
                    , SlotWrapper (ch ^. ch_torso)
                    ]


equippedSlots ∷ Character a b → [SlotWrapper a]
equippedSlots = filter hasItem . equipmentSlots
    where
        hasItem (SlotWrapper (Slot mi)) = isJust mi


equippedContainers ∷ (ItemTraits a) ⇒ Character a b → [SlotWrapper a]
equippedContainers = filter containers . equippedSlots
    where
        containers (SlotWrapper (Slot i)) = maybe False isContainer i

