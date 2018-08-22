{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dreamnet.Engine.ConversationMonad
( ConversationF
, runConversationF_temp

, ConversationNode(..)
, pick
, advance
) where


import Safe                (at)
import Control.Monad.Free  (Free(Pure, Free))
import Data.Bool           (bool)
import Data.Monoid         ((<>))

import Dreamnet.Engine.Conversation


--------------------------------------------------------------------------------
-- TODO VINTAGE

-- Note: NEVER slap equality on recursive data structures
data ConversationNode = TalkNode        String Word [String] ConversationNode
                      | ChoiceNode      [String] [ConversationNode]
                      | DescriptionNode String ConversationNode
                      | End
                      deriving (Show)


instance Semigroup  ConversationNode where
    (TalkNode s i ns nxt)   <> c = TalkNode s i ns (nxt `mappend` c)
    (ChoiceNode opts nxts)  <> c = ChoiceNode opts ((`mappend` c) <$> nxts)
    (DescriptionNode s nxt) <> c = DescriptionNode s (nxt `mappend` c)
    End                     <> c = c

instance Monoid ConversationNode where
    mempty = End
 
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

--------------------------------------------------------------------------------

data ConversationF o a = CName         Word (String → a)
                       | CLastname     Word (String → a)
                       | CNick         Word (String → a)
                       | CTalk         Word String a
                       | CContinue     String a
                       | CReply        String a
                       | CChoice       [String] (Int → a)
                       | CChoice_      [(String, Free (ConversationF o) ())] a -- TODO highly likely, this one can be implemented in terms of Choice
                       | CDescribe     String a
                       | CReceiveItem  Word o a
                       deriving (Functor)


-- So that we can debug-print characters
instance Show (ConversationF o a) where
    show _ = "[CONV]"


instance ConversationAPI o (Free (ConversationF o)) where
    name i = Free $ CName i Pure

    lastname i = Free $ CLastname i Pure

    nick i = Free $ CNick i Pure

    talk i s = Free $ CTalk i s (Pure ())

    continue s = Free $ CContinue s (Pure ())

    reply s = Free $ CReply s (Pure ())

    choice opts = Free $ CChoice opts Pure

    choice_ opts = Free $ CChoice_ opts (Pure ())

    describe s = Free $ CDescribe s (Pure ())
     
    receiveItem i o = Free $ CReceiveItem i o (Pure ())

    (|=>) = (,)


runConversationF_temp ∷ [String] → Free (ConversationF o) a → ConversationNode
runConversationF_temp names = runConversationF names 0 1 End


-- TODO I should get away with ConversationNode alltogether and use free monads to render conversations
--      in realtime, adjusting parameters as necessary
runConversationF ∷ [String] → Word → Word → ConversationNode → Free (ConversationF o) a → ConversationNode
runConversationF ps curr prev cn (Free (CName i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i))

runConversationF ps curr prev cn (Free (CLastname i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i)) -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CNick i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i)) -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CTalk i s n)) =
    runConversationF ps i (bool curr prev $ i == curr) (cn <> TalkNode s i ps End) n

runConversationF ps curr prev cn (Free (CContinue s n)) =
    runConversationF ps curr prev (cn <> TalkNode s curr ps End) n

runConversationF ps curr prev cn (Free (CReply s n)) =
    runConversationF ps prev curr (cn <> TalkNode s prev ps End) n

runConversationF ps curr prev cn (Free (CChoice _ fn)) =
    runConversationF ps curr prev cn (fn 0)  -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CChoice_ opts n)) =
    let (ss, prgs) = unzip opts
        nodes      = fmap (runConversationF ps curr prev End) prgs
    in  runConversationF ps curr prev (cn <> ChoiceNode ss nodes) n

runConversationF ps curr prev cn (Free (CDescribe s n)) =
    runConversationF ps curr prev (cn <> DescriptionNode s End) n

runConversationF ps curr prev cn (Free (CReceiveItem _ _ n)) =
    runConversationF ps curr prev cn n
    
runConversationF _ _ _ cn (Pure _) =
    cn

