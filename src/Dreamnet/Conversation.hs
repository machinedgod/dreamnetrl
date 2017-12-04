{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Conversation
( Conversation(..)
, ConversationNode(..)
, ConversationM
, runConversation
) where


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
                      deriving (Eq, Show)


newtype ConversationM a = ConversationM { runConversationM ∷ State ConversationNode a }
                        deriving (Functor, Applicative, Monad, MonadState ConversationNode)


instance Conversation ConversationM where
    pick i = get >>= \case
        TalkNode   _ n  → put n *> get
        ListenNode _ n  → put n *> get
        ChoiceNode _ ns → put (ns !! fromIntegral i) *> get
        End             → get
    advance (ChoiceNode _ _) = pick 0
    advance (TalkNode _ n)   = put n *> get
    advance (ListenNode _ n) = put n *> get
    advance End              = return End


runConversation ∷ ConversationNode → ConversationM ConversationNode → ConversationNode
runConversation ol c = evalState (runConversationM c) ol
