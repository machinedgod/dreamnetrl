{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Conversation
( Conversation(..)
, ConversationNode(..)
, ConversationM
, runConversation

, testLinearConvo
, testBranchingConvo
, testRecursiveConvo
) where


import Control.Lens
import Control.Monad.State

--------------------------------------------------------------------------------

class Conversation c where
    pick    ∷ Word → c ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
    advance ∷ ConversationNode → c ConversationNode

--------------------------------------------------------------------------------

data ConversationNode = TalkNode   String   ConversationNode
                      | ListenNode String   ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | End


newtype ConversationM a = ConversationM { runConversationM ∷ State ConversationNode a }
                        deriving (Functor, Applicative, Monad, MonadState ConversationNode)


instance Conversation ConversationM where
    pick i = get >>= \case
        TalkNode   _ n  → put n >> get
        ListenNode _ n  → put n >> get
        ChoiceNode _ ns → put (ns !! fromIntegral i) >> get
    advance (ChoiceNode _ _) = pick 0
    advance (TalkNode _ n)   = put n >> get
    advance (ListenNode _ n) = put n >> get
    advance End              = return End


runConversation ∷ ConversationNode → ConversationM ConversationNode → ConversationNode
runConversation ol c = evalState (runConversationM c) ol

--------------------------------------------------------------------------------

testLinearConvo ∷ ConversationNode
testLinearConvo = 
    TalkNode "Hey, Moe."
        $ ListenNode "Hey Cal! What's up? Usual?"
            $ TalkNode "Yea, thanks. Is Delgado here?"
                $ ListenNode "Just missed him."
                    $ TalkNode "Damn. I was running late. He was supposed to help move some hardware."
                        $ ListenNode "Mmmmm..."
                            $ TalkNode "Actually, hold that drink, will ya? If he's not here I'll use the time and go do some stuff."
                                $ ListenNode "Later, Cal."
                                    $ End


testBranchingConvo ∷ ConversationNode
testBranchingConvo =
    ChoiceNode [ "Hey, Moe."
               , "Hey you stupid fuck!"
               ]
        [ ListenNode "Hey Cal! Sorry I'm really busy, can't talk now!" End
        , ListenNode "<pulls the gun>" End
        ]


testRecursiveConvo ∷ ConversationNode
testRecursiveConvo =
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
