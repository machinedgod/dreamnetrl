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

, moeConvo
, garryConvo
, johnnyConvo
, sallyConvo
) where

import Control.Lens

import Dreamnet.Item
import Dreamnet.Conversation

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

makeLenses ''CombatSkills

-- Holds all the skills needed to deal with electronics
data ElectronicsSkills = ElectronicsSkills {
      _es_remainingPoints ∷ Word

    , _es_juryrig ∷ Word
    , _es_modify  ∷ Word
    , _es_analyze ∷ Word
    }

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

makeLenses ''CommunicationSkills


data Character = Character {
      _ch_name      ∷ String

    , _ch_leftHand  ∷ Slot 'Hand
    , _ch_rightHand ∷ Slot 'Hand
    , _ch_torso ∷ Slot 'Torso

    , _ch_conversation ∷ ConversationNode

    , _ch_inventory ∷ [Item]

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

makeLenses ''Character


newCharacter ∷ String → ConversationNode → Character
newCharacter n cn = Character n empty empty empty cn inv 0 cs es ss
    where
        empty = Slot Nothing
        inv   = []
        cs    = CombatSkills 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        es    = ElectronicsSkills 0 0 0 0
        ss    = CommunicationSkills 0 0 0 0 0 0 0

--------------------------------------------------------------------------------

garryConvo ∷ ConversationNode
garryConvo = ListenNode "Beat it, lizzie!" End

johnnyConvo ∷ ConversationNode
johnnyConvo =
    ListenNode "Yea?" $
      TalkNode "Hey dude, I'm Carla" $
        ListenNode "Who gives a shit? Now would you mind, we're in the middle of something." $
          End

sallyConvo ∷ ConversationNode
sallyConvo =
    ListenNode "Mmmm, hello _there_ hot stuff." $
      TalkNode "Hi, I am..." $
        ListenNode "Yeah, listen, not tonight sugar puffs, I'm in the middle of something, OK?" $
          End

moeConvo ∷ ConversationNode
moeConvo =
    TalkNode "Hey, Moe."
        $ ListenNode "Hey Cal! What's up? Usual?" mainBranch
    where
        mainBranch = ChoiceNode [ "I was wondering if you could tell me some cool stuff."
                                , "What's on the tap tonight?"
                                , "Anyone new in tonight"
                                , "I gotta split, Moe. Tell Delgado I'm looking for him, will ya?"
                                ]
                         [ ListenNode "Sorry babe, I know nothin'." mainBranch
                         , brewBranch
                         , ListenNode "Yeah, that dude and that gal over there" newPeopleBranch
                         , ListenNode "Yeah yeah yeah... later girl." End
                         ]
              
        brewBranch = ListenNode "Indian light laced with my special mix!"
                         $ ChoiceNode [ "Hit me up!"
                                      , "Uuuh, I'll pass!"
                                      ]
                             [ ListenNode "Here it comes!" mainBranch
                             , ListenNode "Suit yourself, babe." mainBranch
                             ]
        newPeopleBranch = ChoiceNode [ "Who's the dude?"
                                     , "Who's the gal?" 
                                     , "Actually, I wanted to ask you:"
                                     ]
                            [ ListenNode "Word on the street he's some jacked up street surgeon from up the sprawl. Not a type of dude you wanna mess with." newPeopleBranch
                            , ListenNode "His girlfriend. Also an assasin wired with Omega5. Storm tried to run a full scan on her, said her wetware fried his probe." $
                                TalkNode "Shit, no kidding!" $
                                  ListenNode "No kidding." $
                                    TalkNode "Knowing Devin, it just ticked him off. He has a thing for puzzles." $
                                      ListenNode "He backed off this one, though." $
                                        TalkNode "Oh?" $
                                          ListenNode "Yeah, she sent him a drink-" $
                                            ListenNode "<laughs and snorts>" $
                                              ListenNode "-a bloody mary! Subtle! <snort>" newPeopleBranch
                            , mainBranch
                            --, ListenNode "Yeah?" mainBranch
                            ]
