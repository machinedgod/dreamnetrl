{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dreamnet.Engine.Character
( Orientation(..)
, SlotType(..), Slot(Slot), sItem, slotType, slotOrientation

, Stance(..)

, MeleeCombatSkills(..), mcsRemainingPoints, mcsBarehanded, mcsKnives
, mcsSwords, mcsStaves, mcsMaces, sumMelee

, RangedCombatSkills(..), rcsRemainingPoints, rcsGuns, rcsSmgs, rcsShotguns
, rcsAssault, rcsSniper, rcsBows, rcsCrossbows, rcsPlasma, rcsLasers
, sumRanged

, ThrowingSkills(..), tsRemainingPoints, tsGrenades, tsKnives, tsShurikens
, tsStickies, sumThrowing

, EngineeringSkills(..), esRemainingPoints, esAssembly, esModding, esRepair
, esAnalysis, esJuryrigging, sumEngineering

, CommunicationSkills(..), ssRemainingPoints, ssSmallTalk, ssBodyLanguage
, ssNeurolinguisticProgramming, ssHaggle, ssInterrogation, ssSeduction
, sumCommunication

, InfiltrationSkills(..), isRemainingPoints, isBlendInShadows, isUseOfCover
, isSilentMovement, isCoverSwitchManeuver, sumInfiltration

, Equipment(Equipment), eqLeftHand, eqRightHand, eqHead, eqTorso, eqBack
, eqBelt, eqLeftArm, eqRightArm, eqLeftThigh, eqRightThigh, eqLeftShin
, eqRightShin, eqLeftFoot, eqRightFoot

, Character, chName, chLastName, chNickName, chHandedness, chDescription
, chEquipment, chStance, chFaction, chConversation, chHealthPoints
, chMaxHealthPoints, chExperience, chMeleeCombat, chRangedCombat
, chThrowing, chEngineering, chCommunication, chInfiltration

, newCharacter, modifySlotContent, pickUp

, SlotWrapper(..), slotWrapperOrientation, slotWrapperType, slotWrapperItem
, slotsAsList, equippedSlots, primaryHandSlot, secondaryHandSlot
) where


import Control.Lens         (makeLenses, makePrisms, view, views, (^.), (%~))
import Data.Maybe           (isJust, isNothing)
import Data.Singletons      (Demote, Sing, SingI, fromSing, sing)
import Data.Singletons.TH   (genSingletons)

--------------------------------------------------------------------------------

data Orientation = LeftSide
                 | RightSide
                 deriving (Eq, Ord, Bounded, Enum, Show)
$(genSingletons [ ''Orientation ])
makePrisms ''Orientation


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
makePrisms ''SlotType


-- TODO we probably should keep type as a single variable, and then join
--      whatever we want in (Slot type, orientation, allowed items, etc)
newtype Slot (o ∷ Maybe Orientation) (t ∷ SlotType) i = Slot { _sItem ∷ Maybe i }
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
makePrisms ''Stance

--------------------------------------------------------------------------------

-- All combat-related skills
data MeleeCombatSkills = MeleeCombatSkills {
      _mcsRemainingPoints ∷ Word

    -- Specific added bonuses applied when fighting barehanded
    , _mcsBarehanded ∷ Word
    -- Specific added bonuses applied when utilizing one of the weapons listed
    , _mcsKnives     ∷ Word
    , _mcsSwords     ∷ Word
    , _mcsStaves     ∷ Word
    , _mcsMaces      ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''MeleeCombatSkills


sumMelee ∷ MeleeCombatSkills → Word
sumMelee mcs = sum $ fmap (`view` mcs)
    [ mcsRemainingPoints
    , mcsBarehanded
    , mcsKnives
    , mcsSwords
    , mcsStaves
    , mcsMaces
    ]


data RangedCombatSkills = RangedCombatSkills {
      _rcsRemainingPoints ∷ Word

    -- Specific added bonuses applied when fighting with specified weapon group
    , _rcsGuns      ∷ Word
    , _rcsSmgs      ∷ Word
    , _rcsShotguns  ∷ Word
    , _rcsAssault   ∷ Word
    , _rcsSniper    ∷ Word
    , _rcsBows      ∷ Word
    , _rcsCrossbows ∷ Word
    , _rcsPlasma    ∷ Word
    , _rcsLasers    ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''RangedCombatSkills


sumRanged ∷ RangedCombatSkills → Word
sumRanged rcs = sum $ fmap (`view` rcs)
    [ rcsRemainingPoints
    , rcsGuns
    , rcsSmgs
    , rcsShotguns
    , rcsAssault
    , rcsSniper
    , rcsBows
    , rcsCrossbows
    , rcsPlasma
    , rcsLasers
    ]


data ThrowingSkills = ThrowingSkills {
      _tsRemainingPoints ∷ Word

    , _tsGrenades  ∷ Word
    , _tsKnives    ∷ Word
    , _tsShurikens ∷ Word
    , _tsStickies  ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''ThrowingSkills


sumThrowing ∷ ThrowingSkills → Word
sumThrowing ts = sum $ fmap (`view` ts)
    [ tsRemainingPoints
    , tsGrenades
    , tsKnives
    , tsShurikens
    , tsStickies
    ]


-- Holds all the skills needed to deal with electronics
data EngineeringSkills = EngineeringSkills {
      _esRemainingPoints ∷ Word

    -- Ability to create new items from parts
    , _esAssembly    ∷ Word
    -- Ability to modify items to add new abilities or enhance/change existing
    , _esModding     ∷ Word
    -- Ability to repair damaged and broken items
    , _esRepair      ∷ Word
    -- Ability to figure out what certain item's abilities are
    , _esAnalysis    ∷ Word
    -- Ability to repurpose "garbage" into parts
    , _esJuryrigging ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''EngineeringSkills


sumEngineering ∷ EngineeringSkills → Word
sumEngineering es = sum $ fmap (`view` es)
    [ esRemainingPoints
    , esAssembly
    , esModding
    , esRepair
    , esAnalysis
    , esJuryrigging
    ]


data CommunicationSkills = CommunicationSkills {
      _ssRemainingPoints ∷ Word

    -- Extraction of information from smalltalk
    , _ssSmallTalk                  ∷ Word
    -- Ability to read body language to detect hidden context
    , _ssBodyLanguage               ∷ Word
    -- Ability to implant ideas into other people's heads
    , _ssNeurolinguisticProgramming ∷ Word
    -- Ability to trade low-value items for high-value items
    , _ssHaggle                     ∷ Word
    -- Ability to extract information by coercion
    , _ssInterrogation              ∷ Word
    -- Ability to extract information through sexual attraction
    , _ssSeduction                  ∷ Word
    }
    deriving (Eq, Show)
makeLenses ''CommunicationSkills


sumCommunication ∷ CommunicationSkills → Word
sumCommunication ss = sum $ fmap (`view` ss)
    [ ssRemainingPoints
    , ssSmallTalk
    , ssBodyLanguage
    , ssNeurolinguisticProgramming
    , ssHaggle
    , ssInterrogation
    , ssSeduction
    ]


data InfiltrationSkills = InfiltrationSkills {
      _isRemainingPoints ∷ Word

    -- Remaining visually undetected when outside light radius
    , _isBlendInShadows      ∷ Word
    -- Remaining visually undetected when taking cover behind an object
    , _isUseOfCover          ∷ Word
    -- Remaining audibly undetected while moving (add groups for running, walking, and prone)
    , _isSilentMovement      ∷ Word
    -- Remaining audibly and visually undetected while switching covers with a roll
    -- Also affects max maneuvering distance
    , _isCoverSwitchManeuver ∷ Word
    }
    deriving(Eq, Show)
makeLenses ''InfiltrationSkills


sumInfiltration ∷ InfiltrationSkills → Word
sumInfiltration is = sum $ fmap (`view` is)
    [ isRemainingPoints
    , isBlendInShadows
    , isUseOfCover
    , isSilentMovement
    , isCoverSwitchManeuver
    ]

--------------------------------------------------------------------------------

-- TODO some items should be able to take over multiple slots, like two-handed
--      items or whole-body armours
-- NOTE support for different types of items in different slots? Eg "Any" type of items or "Clothes" items?
data Equipment i = Equipment {
      _eqLeftHand  ∷ Slot ('Just 'LeftSide) 'Hand i
    , _eqRightHand ∷ Slot ('Just 'RightSide) 'Hand i

    , _eqHead       ∷ Slot 'Nothing 'Head i
    , _eqTorso      ∷ Slot 'Nothing 'Torso i
    , _eqBack       ∷ Slot 'Nothing 'Back i
    , _eqBelt       ∷ Slot 'Nothing 'Belt i
    , _eqLeftArm    ∷ Slot ('Just 'LeftSide) 'Arm i
    , _eqRightArm   ∷ Slot ('Just 'RightSide) 'Arm i
    , _eqLeftThigh  ∷ Slot ('Just 'LeftSide) 'Thigh i
    , _eqRightThigh ∷ Slot ('Just 'RightSide) 'Thigh i
    , _eqLeftShin   ∷ Slot ('Just 'LeftSide) 'Shin i
    , _eqRightShin  ∷ Slot ('Just 'RightSide) 'Shin i
    , _eqLeftFoot   ∷ Slot ('Just 'LeftSide) 'Foot i
    , _eqRightFoot  ∷ Slot ('Just 'RightSide) 'Foot i
    }
    deriving(Eq, Show)
makeLenses ''Equipment

--------------------------------------------------------------------------------

data Character i c f = Character {
      _chName        ∷ String
    , _chLastName    ∷ String
    , _chNickName    ∷ String
    , _chHandedness  ∷ Orientation
    , _chDescription ∷ String
    , _chEquipment   ∷ Equipment i
    , _chStance      ∷ Stance

    , _chFaction      ∷ f
    , _chConversation ∷ c

    -- Earned only through missions and combat,
    -- represents general experience
    -- Used to earn skillpoints in each of the skill branches
    , _chHealthPoints    ∷ Word  -- TODO better injury sysstem!
    , _chMaxHealthPoints ∷ Word  -- TODO better injury sysstem!
    , _chExperience      ∷ Word

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
    , _chMeleeCombat   ∷ MeleeCombatSkills
    , _chRangedCombat  ∷ RangedCombatSkills
    , _chThrowing      ∷ ThrowingSkills
    , _chEngineering   ∷ EngineeringSkills
    , _chCommunication ∷ CommunicationSkills
    , _chInfiltration  ∷ InfiltrationSkills
    }
    deriving (Show)
makeLenses ''Character

instance Eq (Character i c f) where
    ch1 == ch2 = ch1 ^. chName == ch2 ^. chName

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
      _chName        = n
    , _chLastName    = ln
    , _chNickName    = nn
    , _chHandedness  = hnd
    , _chDescription = d

    , _chEquipment  = eq
    , _chStance     = Upright

    , _chFaction      = fac
    , _chConversation = cn

    , _chHealthPoints    = mhp
    , _chMaxHealthPoints = mhp
    , _chExperience      = 0

    , _chMeleeCombat   = msk
    , _chRangedCombat  = rsk
    , _chThrowing      = tsk
    , _chEngineering   = esk
    , _chCommunication = csk
    , _chInfiltration  = isk
    }

--------------------------------------------------------------------------------

modifySlotContent ∷ Maybe Orientation → SlotType → (Maybe i → Maybe i) → Character i c f → Character i c f
modifySlotContent (Just LeftSide)  Hand  f = chEquipment.eqLeftHand   .sItem %~ f
modifySlotContent (Just RightSide) Hand  f = chEquipment.eqRightHand  .sItem %~ f
modifySlotContent _                Head  f = chEquipment.eqHead       .sItem %~ f
modifySlotContent _                Torso f = chEquipment.eqTorso      .sItem %~ f
modifySlotContent _                Back  f = chEquipment.eqBack       .sItem %~ f
modifySlotContent _                Belt  f = chEquipment.eqBelt       .sItem %~ f
modifySlotContent (Just LeftSide)  Arm   f = chEquipment.eqRightArm   .sItem %~ f
modifySlotContent (Just RightSide) Arm   f = chEquipment.eqRightArm   .sItem %~ f
modifySlotContent (Just LeftSide)  Thigh f = chEquipment.eqLeftThigh  .sItem %~ f
modifySlotContent (Just RightSide) Thigh f = chEquipment.eqRightThigh .sItem %~ f
modifySlotContent (Just LeftSide)  Shin  f = chEquipment.eqLeftShin   .sItem %~ f
modifySlotContent (Just RightSide) Shin  f = chEquipment.eqRightShin  .sItem %~ f
modifySlotContent (Just LeftSide)  Foot  f = chEquipment.eqLeftFoot   .sItem %~ f
modifySlotContent (Just RightSide) Foot  f = chEquipment.eqRightFoot  .sItem %~ f
modifySlotContent _                _     _ = id


--------------------------------------------------------------------------------

-- Cannot use record syntax due to escaped type variables
data SlotWrapper i = ∀ o t. (SingI o, SingI t) ⇒ SlotWrapper (Slot o t i)
makePrisms 'SlotWrapper

deriving instance (Show i) ⇒ Show (SlotWrapper i)
deriving instance Functor SlotWrapper

instance (Eq i) ⇒ Eq (SlotWrapper i) where
    (SlotWrapper (Slot i)) == (SlotWrapper (Slot i')) = i == i'


slotWrapperOrientation ∷ SlotWrapper i → Maybe Orientation
slotWrapperOrientation (SlotWrapper s) = slotOrientation s


slotWrapperType ∷ SlotWrapper i → SlotType
slotWrapperType (SlotWrapper s) = slotType s


slotWrapperItem ∷ SlotWrapper i → Maybe i
slotWrapperItem (SlotWrapper s) = view sItem s


slotsAsList ∷ Character i c f → [SlotWrapper i]
slotsAsList ch = 
    [ SlotWrapper $ ch ^. chEquipment.eqLeftHand
    , SlotWrapper $ ch ^. chEquipment.eqRightHand
    , SlotWrapper $ ch ^. chEquipment.eqHead
    , SlotWrapper $ ch ^. chEquipment.eqTorso
    , SlotWrapper $ ch ^. chEquipment.eqBack
    , SlotWrapper $ ch ^. chEquipment.eqBelt
    , SlotWrapper $ ch ^. chEquipment.eqLeftArm
    , SlotWrapper $ ch ^. chEquipment.eqRightArm
    , SlotWrapper $ ch ^. chEquipment.eqLeftThigh
    , SlotWrapper $ ch ^. chEquipment.eqRightThigh
    , SlotWrapper $ ch ^. chEquipment.eqLeftShin
    , SlotWrapper $ ch ^. chEquipment.eqRightShin
    , SlotWrapper $ ch ^. chEquipment.eqLeftFoot
    , SlotWrapper $ ch ^. chEquipment.eqRightFoot
    ]


equippedSlots ∷ Character i c f → [SlotWrapper i]
equippedSlots = filter hasItem . slotsAsList
    where
        hasItem (SlotWrapper s) = views sItem isJust s


primaryHandSlot ∷ Character i c f → SlotWrapper i
primaryHandSlot ch = views chHandedness fetch ch
    where
        fetch LeftSide  = SlotWrapper (ch ^. chEquipment.eqLeftHand)
        fetch RightSide = SlotWrapper (ch ^. chEquipment.eqRightHand)


secondaryHandSlot ∷ Character i c f → SlotWrapper i
secondaryHandSlot ch = views chHandedness fetch ch
    where
        fetch LeftSide  = SlotWrapper (ch ^. chEquipment.eqRightHand)
        fetch RightSide = SlotWrapper (ch ^. chEquipment.eqLeftHand)

--------------------------------------------------------------------------------

pickUp ∷ i → Character i c f → Character i c f
pickUp i ch =
    let phsw = primaryHandSlot ch
    in  if isNothing (slotWrapperItem phsw)
            then modifySlotContent (slotWrapperOrientation phsw) (slotWrapperType phsw) (const (Just i)) ch
            else ch

