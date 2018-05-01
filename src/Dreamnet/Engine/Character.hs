{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Dreamnet.Engine.Character
( Orientation(..)
, SlotType(..)
, Slot(Slot)
, s_item
, slotType
, slotOrientation

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

, Equipment(Equipment)
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

, newCharacter
, modifySlotContent
, pickUp

, SlotWrapper(..)
, slotWrapperOrientation
, slotWrapperType
, slotWrapperItem
, slotsAsList
, equippedSlots
, primaryHandSlot
, secondaryHandSlot
) where


import Control.Lens            (makeLenses, view, views, (^.), (%~))
import Data.Maybe              (isJust, isNothing)
import Data.Singletons         (Demote, Sing, SingI, fromSing, sing)
import Data.Singletons.TH      (genSingletons)

--------------------------------------------------------------------------------

data Orientation = LeftSide
                 | RightSide
                 deriving (Eq, Ord, Bounded, Enum, Show)
$(genSingletons [ ''Orientation ])


data SlotType = Hand
              | Head
              | Torso
              | Back
              | Belt
              | Arm
              | Thigh
              | Shin
              | Foot
              deriving (Eq, Show)
$(genSingletons [ ''SlotType ])


-- TODO we probably should keep type as a single variable, and then join
--      whatever we want in (Slot type, orientation, allowed items, etc)
newtype Slot (o ∷ Maybe Orientation) (t ∷ SlotType) i = Slot { _s_item ∷ Maybe i }
                                                      deriving (Functor, Applicative, Monad)
makeLenses ''Slot

deriving instance (Show i) ⇒ Show (Slot o t i)
deriving instance (Eq i) ⇒ Eq (Slot o t i)


slotType ∷ ∀ o t i. (SingI t) ⇒ Slot o t i → Demote SlotType
slotType _ = fromSing (sing ∷ Sing t)


slotOrientation ∷ ∀ o t i. (SingI o) ⇒ Slot o t i → Demote (Maybe Orientation)
slotOrientation _ = fromSing (sing ∷ Sing o)

--------------------------------------------------------------------------------

data Stance = Upright
            | Crouch
            | Prone
            deriving(Eq, Ord, Bounded, Enum, Show)

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

-- TODO some items should be able to take over multiple slots, like two-handed
--      items or whole-body armours
-- NOTE support for different types of items in different slots? Eg "Any" type of items or "Clothes" items?
data Equipment i = Equipment {
      _eq_leftHand  ∷ Slot ('Just 'LeftSide) 'Hand i
    , _eq_rightHand ∷ Slot ('Just 'RightSide) 'Hand i

    , _eq_head       ∷ Slot 'Nothing 'Head i
    , _eq_torso      ∷ Slot 'Nothing 'Torso i
    , _eq_back       ∷ Slot 'Nothing 'Back i
    , _eq_belt       ∷ Slot 'Nothing 'Belt i
    , _eq_leftArm    ∷ Slot ('Just 'LeftSide) 'Arm i
    , _eq_rightArm   ∷ Slot ('Just 'RightSide) 'Arm i
    , _eq_leftThigh  ∷ Slot ('Just 'LeftSide) 'Thigh i
    , _eq_rightThigh ∷ Slot ('Just 'RightSide) 'Thigh i
    , _eq_leftShin   ∷ Slot ('Just 'LeftSide) 'Shin i
    , _eq_rightShin  ∷ Slot ('Just 'RightSide) 'Shin i
    , _eq_leftFoot   ∷ Slot ('Just 'LeftSide) 'Foot i
    , _eq_rightFoot  ∷ Slot ('Just 'RightSide) 'Foot i
    }
    deriving(Eq, Show)
makeLenses ''Equipment

--------------------------------------------------------------------------------

data Character i c f = Character {
      _ch_name        ∷ String
    , _ch_lastName    ∷ String
    , _ch_nickName    ∷ String
    , _ch_handedness  ∷ Orientation
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

instance Eq (Character i c f) where
    ch1 == ch2 = ch1 ^. ch_name == ch2 ^. ch_name

--------------------------------------------------------------------------------

newCharacter ∷ String
             → String
             → String
             → Orientation
             → String
             → Equipment i
             → f
             → c
             → Word
             → MeleeCombatSkills
             → RangedCombatSkills
             → ThrowingSkills
             → EngineeringSkills
             → CommunicationSkills
             → InfiltrationSkills
             → Character i c f
newCharacter n ln nn hnd d eq fac cn mhp msk rsk tsk esk csk isk =
    Character {
      _ch_name        = n
    , _ch_lastName    = ln
    , _ch_nickName    = nn
    , _ch_handedness  = hnd
    , _ch_description = d

    , _ch_equipment  = eq
    , _ch_stance     = Upright

    , _ch_faction      = fac
    , _ch_conversation = cn

    , _ch_healthPoints    = mhp
    , _ch_maxHealthPoints = mhp
    , _ch_experience      = 0

    , _ch_meleeCombat   = msk
    , _ch_rangedCombat  = rsk
    , _ch_throwing      = tsk
    , _ch_engineering   = esk
    , _ch_communication = csk
    , _ch_infiltration  = isk
    }

--------------------------------------------------------------------------------

modifySlotContent ∷ Maybe Orientation → SlotType → (Maybe i → Maybe i) → Character i c f → Character i c f
modifySlotContent (Just LeftSide)  Hand  f = ch_equipment.eq_leftHand.s_item %~ f
modifySlotContent (Just RightSide) Hand  f = ch_equipment.eq_rightHand.s_item %~ f
modifySlotContent _                Head  f = ch_equipment.eq_head.s_item %~ f
modifySlotContent _                Torso f = ch_equipment.eq_torso.s_item %~ f
modifySlotContent _                Back  f = ch_equipment.eq_back.s_item %~ f
modifySlotContent _                Belt  f = ch_equipment.eq_belt.s_item %~ f
modifySlotContent (Just LeftSide)  Arm   f = ch_equipment.eq_rightArm.s_item %~ f
modifySlotContent (Just RightSide) Arm   f = ch_equipment.eq_rightArm.s_item %~ f
modifySlotContent (Just LeftSide)  Thigh f = ch_equipment.eq_leftThigh.s_item %~ f
modifySlotContent (Just RightSide) Thigh f = ch_equipment.eq_rightThigh.s_item %~ f
modifySlotContent (Just LeftSide)  Shin  f = ch_equipment.eq_leftShin.s_item %~ f
modifySlotContent (Just RightSide) Shin  f = ch_equipment.eq_rightShin.s_item %~ f
modifySlotContent (Just LeftSide)  Foot  f = ch_equipment.eq_leftFoot.s_item %~ f
modifySlotContent (Just RightSide) Foot  f = ch_equipment.eq_rightFoot.s_item %~ f
modifySlotContent _                _     _ = id


pickUp ∷ i → Character i c f → Character i c f
pickUp i ch =
    let phsw = primaryHandSlot ch
    in  if isNothing (slotWrapperItem phsw)
            then modifySlotContent (slotWrapperOrientation phsw) (slotWrapperType phsw) (const (Just i)) $ ch
            else ch

--------------------------------------------------------------------------------

-- Cannot use record syntax due to escaped type variables
data SlotWrapper i = ∀ o t. (SingI o, SingI t) ⇒ SlotWrapper (Slot o t i)

deriving instance (Show i) ⇒ Show (SlotWrapper i)
deriving instance Functor SlotWrapper

instance (Eq i) ⇒ Eq (SlotWrapper i) where
    (SlotWrapper (Slot i)) == (SlotWrapper (Slot i')) = i == i'


slotWrapperOrientation ∷ SlotWrapper i → Maybe Orientation
slotWrapperOrientation (SlotWrapper s) = slotOrientation s


slotWrapperType ∷ SlotWrapper i → SlotType
slotWrapperType (SlotWrapper s) = slotType s


slotWrapperItem ∷ SlotWrapper i → Maybe i
slotWrapperItem (SlotWrapper s) = view s_item s


slotsAsList ∷ Character i c f → [SlotWrapper i]
slotsAsList ch = 
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
equippedSlots = filter hasItem . slotsAsList
    where
        hasItem (SlotWrapper s) = views s_item isJust s


primaryHandSlot ∷ Character i c f → SlotWrapper i
primaryHandSlot ch = views ch_handedness fetch ch
    where
        fetch LeftSide  = SlotWrapper (ch ^. ch_equipment.eq_leftHand)
        fetch RightSide = SlotWrapper (ch ^. ch_equipment.eq_rightHand)


secondaryHandSlot ∷ Character i c f → SlotWrapper i
secondaryHandSlot ch = views ch_handedness fetch ch
    where
        fetch LeftSide  = SlotWrapper (ch ^. ch_equipment.eq_rightHand)
        fetch RightSide = SlotWrapper (ch ^. ch_equipment.eq_leftHand)

