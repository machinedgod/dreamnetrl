{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Dreamnet.Character
( module Dreamnet.Item

, CombatSkills
, cs_remainingPoints
, cs_melee
, cs_barehanded
, cs_knives
, cs_swords
, cs_staves
, cs_maces
, cs_ranged
, cs_guns
, cs_smgs
, cs_shotguns
, cs_assault
, cs_sniper
, cs_bows
, cs_crossbows
, cs_plasma
, cs_lasers

, ElectronicsSkills
, es_remainingPoints
, es_juryrig
, es_modify
, es_analyze

, CommunicationSkills
, ss_remainingPoints
, ss_smallTalk
, ss_bodyLanguageReading
, ss_bodyLanguageControl
, ss_trade
, ss_interrogation
, ss_charm

, Character
, ch_name
, ch_leftHand
, ch_rightHand
, ch_torso
, ch_conversation
, ch_inventory
, ch_experience
, ch_combat
, ch_electronics
, ch_social
, newCharacter
) where

import Control.Lens (makeLenses)

import Dreamnet.Item

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

makeLenses ''CombatSkills

-- Holds all the skills needed to deal with electronics
data ElectronicsSkills = ElectronicsSkills {
      _es_remainingPoints ∷ Word

    , _es_juryrig ∷ Word
    , _es_modify  ∷ Word
    , _es_analyze ∷ Word
    }
    deriving (Eq, Show)

makeLenses ''ElectronicsSkills


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

makeLenses ''CommunicationSkills


data Character a b = Character {
      _ch_name      ∷ String

    , _ch_leftHand  ∷ Slot 'Hand a
    , _ch_rightHand ∷ Slot 'Hand a
    , _ch_torso     ∷ Slot 'Torso a

    , _ch_conversation ∷ b

    , _ch_inventory ∷ [a] -- TODO remove intrinsic inventory!

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
    deriving (Eq, Show)

makeLenses ''Character


newCharacter ∷ String → b → Character a b
newCharacter n cn = Character n empty empty empty cn inv 0 cs es ss
    where
        empty = Slot Nothing
        inv   = []
        cs    = CombatSkills 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        es    = ElectronicsSkills 0 0 0 0
        ss    = CommunicationSkills 0 0 0 0 0 0 0

