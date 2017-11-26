{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Dreamnet.Character
( Character
, ch_name
, ch_leftHand
, ch_rightHand
, ch_torso
, ch_conversation
, newCharacter

, moeConvo
, beatItConversation
) where

import Control.Lens

import Dreamnet.Item
import Dreamnet.Conversation

--------------------------------------------------------------------------------

data Character = Character {
      _ch_name      ∷ String

    , _ch_leftHand  ∷ Slot 'Hand
    , _ch_rightHand ∷ Slot 'Hand
    , _ch_torso ∷ Slot 'Torso

    , _ch_conversation ∷ ConversationNode
    }

makeLenses ''Character


newCharacter ∷ String → ConversationNode → Character
newCharacter n cn = Character n empty empty empty cn
    where
        empty = Slot Nothing

--------------------------------------------------------------------------------

beatItConversation ∷ ConversationNode
beatItConversation = ListenNode "Beat it, lizzie!" End


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
              
        brewBranch = ListenNode "Light brew specex!"
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
                            , ListenNode "His girlfriend. Also an assasin wired with Omega5. Storm tried to run a full scan on her, said her wetware fried his probe. She sent him over a bloody mary that evening. Kid knows when to back off." newPeopleBranch
                            , mainBranch
                            --, ListenNode "Yeah?" mainBranch
                            ]
