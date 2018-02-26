{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Dreamnet.Character
( ItemTraits(..)
, Item(..)

, SlotType(..)
, Slot(..)
, SlotWrapper(..)

, Stance(..)

, MeleeCombatSkills
, RangedCombatSkills
, ThrowingSkills
, EngineeringSkills
, CommunicationSkills
, InfiltrationSkills

, Character
, ch_name
, ch_handedness
, ch_description
, ch_leftHand
, ch_rightHand
, ch_torso
, ch_stance
, ch_faction
, ch_conversation
, ch_healthPoints
, ch_maxHealthPoints
, ch_experience
, ch_meleeCombat
, ch_rangedCombat
, ch_throwing
, ch_engineering
, ch_communication
, ch_infiltration
, ch_primaryHand

, newCharacter
, pickUp

, equipmentSlots
, equippedSlots
, equippedContainers
) where


import Control.Lens (Lens', makeLenses, (^.), (%~))
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


newtype Slot (t ∷ SlotType) i = Slot { slottedItem ∷ Maybe i }

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
            deriving(Eq, Ord, Bounded, Enum, Show)

data Handedness = LeftHand
                | RightHand
                deriving (Show)
                
--------------------------------------------------------------------------------

-- All combat-related skills
data MeleeCombatSkills = MeleeCombatSkills {
      _mcs_remainingPoints ∷ Word

    -- Specific added bonuses applied when fighting barehanded
    , _mcs_barehanded ∷ Word
    -- Specific added bonuses applied when utilizing one of the weapons listed
    , _mcs_knives     ∷ Word
    , _mcs_swords     ∷ Word
    , _mcs_staves     ∷ Word
    , _mcs_maces      ∷ Word
    }
    deriving (Eq, Show)
--makeLenses ''MeleeCombatSkills


data RangedCombatSkills = RangedCombatSkills {
      _rcs_remainingPoints ∷ Word

    -- Specific added bonuses applied when fighting with specified weapon group
    , _rcs_guns      ∷ Word
    , _rcs_smgs      ∷ Word
    , _rcs_shotguns  ∷ Word
    , _rcs_assault   ∷ Word
    , _rcs_sniper    ∷ Word
    , _rcs_bows      ∷ Word
    , _rcs_crossbows ∷ Word
    , _rcs_plasma    ∷ Word
    , _rcs_lasers    ∷ Word
    }
    deriving (Eq, Show)
--makeLenses ''RangedCombatSkills

data ThrowingSkills = ThrowingSkills {
      _ts_remainingPoints ∷ Word

    , _ts_grenades  ∷ Word
    , _ts_knives    ∷ Word
    , _ts_shurikens ∷ Word
    , _ts_stickies  ∷ Word
    }
    deriving (Eq, Show)


-- Holds all the skills needed to deal with electronics
data EngineeringSkills = EngineeringSkills {
      _es_remainingPoints ∷ Word

    -- Ability to create useful items from garbage
    , _es_juryrig ∷ Word
    -- Ability to modify items to add new abilities or enhance/change existing
    , _es_modify  ∷ Word
    -- Ability to repair damaged and broken items
    , _es_repair  ∷ Word
    -- Ability to figure out what certain item's abilities are
    , _es_analyze ∷ Word
    }
    deriving (Eq, Show)
--makeLenses ''EngineeringSkills


data CommunicationSkills = CommunicationSkills {
      _ss_remainingPoints ∷ Word

    -- Extraction of information from smalltalk
    , _ss_smallTalk           ∷ Word
    -- Ability to read body language to detect hidden context
    , _ss_bodyLanguage        ∷ Word
    -- Ability to implant ideas into other people's heads
    , _ss_bodyLanguageControl ∷ Word
    -- Ability to trade low-value items for high-value items
    , _ss_haggle              ∷ Word
    -- Ability to extract information by coercion
    , _ss_interrogation       ∷ Word
    -- Ability to extract information through sexual attraction
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
--makeLenses ''InfiltrationSkills

--------------------------------------------------------------------------------

data Character i c f = Character {
      _ch_name        ∷ String
    , _ch_handedness  ∷ Handedness
    , _ch_description ∷ String

    , _ch_leftHand  ∷ Slot 'Hand i
    , _ch_rightHand ∷ Slot 'Hand i
    , _ch_torso     ∷ Slot 'Torso i
    , _ch_stance    ∷ Stance

    , _ch_faction      ∷ f
    , _ch_conversation ∷ c

    -- Earned only through missions and combat,
    -- represents general experience
    -- Used to earn skillpoints in each of the skill branches
    , _ch_healthPoints    ∷ Word  -- TODO better injury sysstem!
    , _ch_maxHealthPoints ∷ Word  -- TODO better injury sysstem!
    , _ch_experience      ∷ Word

    -- Skilltrees *only* affect chances of performing a certain task
    -- To train to a certain level, you need to purchase skillpoints
    -- by spending general experience, and then find a person willing to
    -- train you, who has that skill equal or above desired level
    --
    -- Total amount of skill points in a skill branch contributes towards
    -- a bonus. This means that if you'll be a better sword fighter if you're
    -- also proficient in staves as well.
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
    , _ch_meleeCombat   ∷ MeleeCombatSkills
    , _ch_rangedCombat  ∷ RangedCombatSkills
    , _ch_throwing      ∷ ThrowingSkills
    , _ch_engineering   ∷ EngineeringSkills
    , _ch_communication ∷ CommunicationSkills
    , _ch_infiltration  ∷ InfiltrationSkills
    }
    deriving (Show)

makeLenses ''Character

ch_primaryHand ∷ Character i c f → Lens' (Character i c f) (Slot 'Hand i)
ch_primaryHand ch = slot $ ch ^. ch_handedness
    where slot LeftHand  = ch_leftHand
          slot RightHand = ch_rightHand


instance Eq (Character i c f) where
    ch1 == ch2 = ch1 ^. ch_name == ch2 ^. ch_name

--------------------------------------------------------------------------------

newCharacter ∷ String → String → f → c → Character i c f
newCharacter n d fac cn =
    Character n RightHand d empty empty empty Upright fac cn 10 10 0 mcs rcs ts es ss is
    where
        empty = Slot Nothing
        mcs   = MeleeCombatSkills 0 0 0 0 0 0
        rcs   = RangedCombatSkills 0 0 0 0 0 0 0 0 0 0
        ts    = ThrowingSkills 0 0 0 0 0
        es    = EngineeringSkills 0 0 0 0 0
        ss    = CommunicationSkills 0 0 0 0 0 0 0
        is    = InfiltrationSkills 0 0 0 0 0


-- TODO Only if hands not full!
pickUp ∷ i → Character i c f → Character i c f
pickUp i ch = (ch_primaryHand ch) %~ equipSlot i $ ch

--------------------------------------------------------------------------------

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


equipSlot ∷ i → Slot t i → Slot t i
equipSlot i s = s { slottedItem = Just i }
