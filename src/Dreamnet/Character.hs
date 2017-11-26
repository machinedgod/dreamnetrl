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
, garryConvo
, johnnyConvo
, sallyConvo
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
