{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Conversation
( ConversationNode(..)
, pick
, advance
) where


--------------------------------------------------------------------------------

data ConversationNode = TalkNode   String   ConversationNode
                      | ListenNode String   ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | End
                      deriving (Eq, Show)


pick ∷ Word → ConversationNode → ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
pick _ (TalkNode   _ n)  = n
pick _ (ListenNode _ n)  = n
pick i (ChoiceNode _ ns) = ns !! fromIntegral i
pick _ End               = End


advance ∷ ConversationNode → ConversationNode
advance n@(ChoiceNode _ _) = pick 0 n
advance (TalkNode _ n)     = n
advance (ListenNode _ n)   = n
advance End                = End

