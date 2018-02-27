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

, MeleeCombatSkills(..)
, mcs_remainingPoints
, mcs_barehanded
, mcs_knives
, mcs_swords
, mcs_staves
, mcs_maces
, sumMelee
, RangedCombatSkills(..)
, rcs_remainingPoints
, rcs_guns
, rcs_smgs
, rcs_shotguns
, rcs_assault
, rcs_sniper
, rcs_bows
, rcs_crossbows
, rcs_plasma
, rcs_lasers
, sumRanged
, ThrowingSkills(..)
, ts_remainingPoints
, ts_grenades
, ts_knives
, ts_shurikens
, ts_stickies
, sumThrowing
, EngineeringSkills(..)
, es_remainingPoints
, es_assembly
, es_modding
, es_repair
, es_analysis
, es_juryrigging
, sumEngineering
, CommunicationSkills(..)
, ss_remainingPoints
, ss_smallTalk
, ss_bodyLanguage
, ss_neurolinguisticProgramming
, ss_haggle
, ss_interrogation
, ss_seduction
, sumCommunication
, InfiltrationSkills(..)
, is_remainingPoints
, is_blendInShadows
, is_useOfCover
, is_silentMovement
, is_coverSwitchManeuver
, sumInfiltration

, Equipment
, eq_leftHand
, eq_rightHand
, eq_head
, eq_torso
, eq_back
, eq_belt
, eq_leftArm
, eq_rightArm
, eq_leftThigh
, eq_rightThigh
, eq_leftShin
, eq_rightShin
, eq_leftFoot
, eq_rightFoot
 
, Character
, ch_name
, ch_lastName
, ch_nickName
, ch_handedness
, ch_description
, ch_equipment
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


import Control.Lens (Lens', makeLenses, view, (^.), (%~))
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
              | Head
              | Torso
              | Back
              | Belt
              | Arm
              | Thigh
              | Shin
              | Foot


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
makeLenses ''MeleeCombatSkills


sumMelee ∷ MeleeCombatSkills → Word
sumMelee mcs = sum $ fmap (\l → view l mcs)
    [ mcs_remainingPoints
    , mcs_barehanded
    , mcs_knives
    , mcs_swords
    , mcs_staves
    , mcs_maces
    ]


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
makeLenses ''RangedCombatSkills


sumRanged ∷ RangedCombatSkills → Word
sumRanged rcs = sum $ fmap (\l → view l rcs)
    [ rcs_remainingPoints
    , rcs_guns
    , rcs_smgs
    , rcs_shotguns
    , rcs_assault
    , rcs_sniper
    , rcs_bows
    , rcs_crossbows
    , rcs_plasma
    , rcs_lasers
    ]


data ThrowingSkills = ThrowingSkills {
      _ts_remainingPoints ∷ Word

    , _ts_grenades  ∷ Word
    , _ts_knives    ∷ Word
    , _ts_shurikens ∷ Word
    , _ts_stickies  ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''ThrowingSkills


sumThrowing ∷ ThrowingSkills → Word
sumThrowing ts = sum $ fmap (\l → view l ts)
    [ ts_remainingPoints
    , ts_grenades
    , ts_knives
    , ts_shurikens
    , ts_stickies
    ]


-- Holds all the skills needed to deal with electronics
data EngineeringSkills = EngineeringSkills {
      _es_remainingPoints ∷ Word

    -- Ability to create new items from parts
    , _es_assembly    ∷ Word
    -- Ability to modify items to add new abilities or enhance/change existing
    , _es_modding     ∷ Word
    -- Ability to repair damaged and broken items
    , _es_repair      ∷ Word
    -- Ability to figure out what certain item's abilities are
    , _es_analysis    ∷ Word
    -- Ability to repurpose "garbage" into parts
    , _es_juryrigging ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''EngineeringSkills


sumEngineering ∷ EngineeringSkills → Word
sumEngineering es = sum $ fmap (\l → view l es)
    [ es_remainingPoints
    , es_assembly
    , es_modding
    , es_repair
    , es_analysis
    , es_juryrigging
    ]


data CommunicationSkills = CommunicationSkills {
      _ss_remainingPoints ∷ Word

    -- Extraction of information from smalltalk
    , _ss_smallTalk                  ∷ Word
    -- Ability to read body language to detect hidden context
    , _ss_bodyLanguage               ∷ Word
    -- Ability to implant ideas into other people's heads
    , _ss_neurolinguisticProgramming ∷ Word
    -- Ability to trade low-value items for high-value items
    , _ss_haggle                     ∷ Word
    -- Ability to extract information by coercion
    , _ss_interrogation              ∷ Word
    -- Ability to extract information through sexual attraction
    , _ss_seduction                  ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''CommunicationSkills


sumCommunication ∷ CommunicationSkills → Word
sumCommunication ss = sum $ fmap (\l → view l ss)
    [ ss_remainingPoints
    , ss_smallTalk
    , ss_bodyLanguage
    , ss_neurolinguisticProgramming
    , ss_haggle
    , ss_interrogation
    , ss_seduction
    ]


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
makeLenses ''InfiltrationSkills


sumInfiltration ∷ InfiltrationSkills → Word
sumInfiltration is = sum $ fmap (\l → view l is)
    [ is_remainingPoints
    , is_blendInShadows
    , is_useOfCover
    , is_silentMovement
    , is_coverSwitchManeuver
    ]

--------------------------------------------------------------------------------

data Equipment i = Equipment {
      _eq_leftHand  ∷ Slot 'Hand i
    , _eq_rightHand ∷ Slot 'Hand i

    , _eq_head       ∷ Slot 'Head i
    , _eq_torso      ∷ Slot 'Torso i
    , _eq_back       ∷ Slot 'Back i
    , _eq_belt       ∷ Slot 'Belt i
    , _eq_leftArm    ∷ Slot 'Arm i
    , _eq_rightArm   ∷ Slot 'Arm i
    , _eq_leftThigh  ∷ Slot 'Thigh i
    , _eq_rightThigh ∷ Slot 'Thigh i
    , _eq_leftShin   ∷ Slot 'Shin i
    , _eq_rightShin  ∷ Slot 'Shin i
    , _eq_leftFoot   ∷ Slot 'Foot i
    , _eq_rightFoot  ∷ Slot 'Foot i
    }
    deriving(Eq, Show)
makeLenses ''Equipment

--------------------------------------------------------------------------------

data Character i c f = Character {
      _ch_name        ∷ String
    , _ch_lastName    ∷ String
    , _ch_nickName    ∷ String
    , _ch_handedness  ∷ Handedness
    , _ch_description ∷ String
    , _ch_equipment   ∷ Equipment i
    , _ch_stance      ∷ Stance

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
    where slot LeftHand  = ch_equipment.eq_leftHand
          slot RightHand = ch_equipment.eq_rightHand


instance Eq (Character i c f) where
    ch1 == ch2 = ch1 ^. ch_name == ch2 ^. ch_name

--------------------------------------------------------------------------------

newCharacter ∷ String → String → String → String → f → c → Character i c f
newCharacter n ln nn d fac cn =
    Character {
      _ch_name        = n
    , _ch_lastName    = ln
    , _ch_nickName    = nn
    , _ch_handedness  = RightHand
    , _ch_description = d

    , _ch_equipment  = Equipment em em em em em em em em em em em em em em
    , _ch_stance     = Upright

    , _ch_faction      = fac
    , _ch_conversation = cn

    , _ch_healthPoints    = 10
    , _ch_maxHealthPoints = 10
    , _ch_experience      = 0

    , _ch_meleeCombat   = MeleeCombatSkills 0 0 0 0 0 0
    , _ch_rangedCombat  = RangedCombatSkills 0 0 0 0 0 0 0 0 0 0
    , _ch_throwing      = ThrowingSkills 0 0 0 0 0
    , _ch_engineering   = EngineeringSkills 0 0 0 0 0 0
    , _ch_communication = CommunicationSkills 0 0 0 0 0 0 0
    , _ch_infiltration  = InfiltrationSkills 0 0 0 0 0
    }
    where
        em = Slot Nothing


-- TODO Only if hands not full!
pickUp ∷ i → Character i c f → Character i c f
pickUp i ch = (ch_primaryHand ch) %~ equipSlot i $ ch

--------------------------------------------------------------------------------

equipmentSlots ∷ Character i c f → [SlotWrapper i]
equipmentSlots ch = 
    [ SlotWrapper $ ch ^. ch_equipment.eq_leftHand
    , SlotWrapper $ ch ^. ch_equipment.eq_rightHand
    , SlotWrapper $ ch ^. ch_equipment.eq_head
    , SlotWrapper $ ch ^. ch_equipment.eq_torso
    , SlotWrapper $ ch ^. ch_equipment.eq_back
    , SlotWrapper $ ch ^. ch_equipment.eq_belt
    , SlotWrapper $ ch ^. ch_equipment.eq_leftArm
    , SlotWrapper $ ch ^. ch_equipment.eq_rightArm
    , SlotWrapper $ ch ^. ch_equipment.eq_leftThigh
    , SlotWrapper $ ch ^. ch_equipment.eq_rightThigh
    , SlotWrapper $ ch ^. ch_equipment.eq_leftShin
    , SlotWrapper $ ch ^. ch_equipment.eq_rightShin
    , SlotWrapper $ ch ^. ch_equipment.eq_leftFoot
    , SlotWrapper $ ch ^. ch_equipment.eq_rightFoot
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
