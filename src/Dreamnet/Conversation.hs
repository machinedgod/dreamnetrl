{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.Conversation
( ConversationAPI(..)

, ConversationNode(..)
, pick
, advance
) where


import Safe            (at)

--------------------------------------------------------------------------------

class ConversationAPI c where
    name     ∷ Word → c String
    lastname ∷ Word → c String
    nick     ∷ Word → c String
    talk     ∷ Word → String → c ()
    continue ∷ String → c ()
    reply    ∷ String → c ()
    choice   ∷ [String] → c Int
    choice_  ∷ [(String, c ())] → c ()
    describe ∷ String → c ()
    (|=>)    ∷ String → c () → (String, c ())

--------------------------------------------------------------------------------
-- TODO VINTAGE

-- Note: NEVER slap equality on recursive data structures
data ConversationNode = TalkNode   String Word [String] ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | DescriptionNode String ConversationNode
                      | End
                      deriving (Show)


-- TODO I wonder if this equality will bite me in the ass later on?
instance Eq ConversationNode where
    (TalkNode _ _ _ _)    == (TalkNode _ _ _ _)    = True
    (ChoiceNode _ _)      == (ChoiceNode _ _)      = True
    (DescriptionNode _ _) == (DescriptionNode _ _) = True
    End                   == End                   = True
    _                     == _                     = False


--instance Show ConversationNode where
--    show (TalkNode t _ _ _)    = "TalkNode " <> t
--    show (ChoiceNode cs _)     = "ChoiceNode " <> show cs
--    show (DescriptionNode s _) = "DescriptionNode " <> show s
--    show End                   = "End"


instance Monoid ConversationNode where
    mempty = End
    mappend (TalkNode s i ns nxt)   c = TalkNode s i ns (nxt `mappend` c)
    mappend (ChoiceNode opts nxts)  c = ChoiceNode opts ((`mappend` c) <$> nxts)
    mappend (DescriptionNode s nxt) c = DescriptionNode s (nxt `mappend` c)
    mappend End                     c = c

 
pick ∷ ConversationNode → Word → ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
pick (TalkNode _ _ _ n)    _ = n
pick (ChoiceNode _ ns)     i = ns `at` fromIntegral i
pick (DescriptionNode _ n) _ = n
pick End                   _ = End


advance ∷ ConversationNode → ConversationNode
advance n@(ChoiceNode _ _)    = pick n 0
advance (TalkNode _ _ _ n)    = n
advance (DescriptionNode _ n) = n
advance End                   = End

