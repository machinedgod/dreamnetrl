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

, Stance(..)

, CombatSkills
, ElectronicsSkills
, CommunicationSkills
, Character
, ch_name
, ch_leftHand
, ch_rightHand
, ch_torso
, ch_stance
, ch_conversation
, ch_experience
, ch_combat
, ch_electronics
, ch_social
, ch_infiltration
, newCharacter

, equipmentSlots
, equippedSlots
, equippedContainers

, isFriend
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


newtype Slot (t ∷ SlotType) i = Slot (Maybe i)

deriving instance (Show i) ⇒ Show (Slot t i)
deriving instance (Eq i) ⇒ Eq (Slot t i)


-- Cannot use record syntax due to escaped type variables
data SlotWrapper i = forall t. SlotWrapper (Slot t i)

deriving instance (Show i) ⇒ Show (SlotWrapper i)

instance (Eq i) ⇒ Eq (SlotWrapper i) where
    (SlotWrapper (Slot i)) == (SlotWrapper (Slot i')) = i == i'

--------------------------------------------------------------------------------

data Stance = Upright
            | Crouch
            | Prone
            deriving(Show, Enum)

--------------------------------------------------------------------------------

-- All combat-related skills
data CombatSkills = CombatSkills {
      _cs_remainingPoints ∷ Word

    -- General melee skill, applied everywhere where hand-to-hand combat is
    -- involved, with or without weapons
    , _cs_melee      ∷ Word
    -- Specific added bonuses applied when fighting barehanded
    , _cs_barehanded ∷ Word
    -- Specific added bonuses applied when utilizing one of the weapons listed
    , _cs_knives     ∷ Word
    , _cs_swords     ∷ Word
    , _cs_staves     ∷ Word
    , _cs_maces      ∷ Word

    -- General ranged combat skill, applied everywhere where aiming and
    -- shooting is involved, from bows to firearms
    , _cs_ranged    ∷ Word
    -- Specific added bonuses applied when fighting with specified weapon group
    , _cs_guns      ∷ Word
    , _cs_smgs      ∷ Word
    , _cs_shotguns  ∷ Word
    , _cs_assault   ∷ Word
    , _cs_sniper    ∷ Word
    , _cs_bows      ∷ Word
    , _cs_crossbows ∷ Word
    , _cs_plasma    ∷ Word
    , _cs_lasers    ∷ Word

    -- General throwing combat skill, applied everywhere where throwing
    -- an explosive or a device is used
    , _cs_throwing ∷ Word
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


data InfiltrationSkills = InfiltrationSkills {
      _is_remainingPoints ∷ Word

    -- Remaining visually undetected when outside light radius
    , _is_blendInShadows      ∷ Word
    -- Remaining visually undetected when taking cover behind an object
    , _is_useOfCover          ∷ Word
    -- Remaining audibly undetected while moving (add groups for running, walking, and prone)
    , _is_silentMovement      ∷ Word
    -- Remaining audibly and visually undetected while switching covers with a roll
    -- Also affects max maneuvering distance
    , _is_coverSwitchManeuver ∷ Word
    }
    deriving(Eq, Show)

--------------------------------------------------------------------------------

data Character i c f = Character {
      _ch_name      ∷ String

    , _ch_leftHand  ∷ Slot 'Hand i
    , _ch_rightHand ∷ Slot 'Hand i
    , _ch_torso     ∷ Slot 'Torso i
    , _ch_stance    ∷ Stance

    , _ch_faction      ∷ f
    , _ch_conversation ∷ c

    -- Earned only through missions and combat,
    -- represents general experience
    -- Used to earn skillpoints in each of the skill branches
    , _ch_experience ∷ Word

    -- Skilltrees *only* affect chances of performing a certain task
    -- To train to a certain level, you need to purchase skillpoints
    -- by spending general experience, and then find a person willing to
    -- train you, who has that skill equal or above desired level
    -- 
    -- Additionally, abilities are *not* acquired through skills! They are
    -- either "innate", meaning *everyone* can try to perform them once learned,
    -- or they're applied through usage of certain items.
    -- For example, there's no innate lockpicking ability: you need to purchase
    -- and use lockpicking tools in order to try and pick a lock, and this is
    -- what engages lockpicking skill to modify success roll.
    -- For a second example, cover swtich maneuver is an innate skill. Unskilled
    -- characters will simply run or walk to next cover. Highly skilled characters
    -- will perform fast and silent maneuver at the right time not to be seen
    -- by NPC's or cameras.
    , _ch_combat       ∷ CombatSkills
    , _ch_electronics  ∷ ElectronicsSkills
    , _ch_social       ∷ CommunicationSkills
    , _ch_infiltration ∷ InfiltrationSkills
    }
    deriving (Show)

makeLenses ''Character


instance Eq (Character i c f) where
    ch1 == ch2 = ch1 ^. ch_name == ch2 ^. ch_name


newCharacter ∷ String → f → c → Character i c f
newCharacter n fac cn = Character n empty empty empty Upright fac cn 0 cs es ss is
    where
        empty = Slot Nothing
        cs    = CombatSkills 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        es    = ElectronicsSkills 0 0 0 0
        ss    = CommunicationSkills 0 0 0 0 0 0 0
        is    = InfiltrationSkills 0 0 0 0 0


equipmentSlots ∷ Character i c f → [SlotWrapper i]
equipmentSlots ch = [ SlotWrapper (ch ^. ch_leftHand)
                    , SlotWrapper (ch ^. ch_rightHand)
                    , SlotWrapper (ch ^. ch_torso)
                    ]


equippedSlots ∷ Character i c f → [SlotWrapper i]
equippedSlots = filter hasItem . equipmentSlots
    where
        hasItem (SlotWrapper (Slot mi)) = isJust mi


equippedContainers ∷ (ItemTraits i) ⇒ Character i c f → [SlotWrapper i]
equippedContainers = filter containers . equippedSlots
    where
        containers (SlotWrapper (Slot i)) = maybe False isContainer i


isFriend ∷ (Eq f) ⇒ Character i c f → Character i c f → Bool
isFriend c1 c2 = c1 ^. ch_faction == c2 ^. ch_faction

