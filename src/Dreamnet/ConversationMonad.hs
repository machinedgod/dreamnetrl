{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ConversationMonad
( runConversationF_temp
) where


import Safe                (at)
import Control.Monad.Free  (Free(Pure, Free))
import Data.Bool           (bool)

import Dreamnet.Conversation

--------------------------------------------------------------------------------

data ConversationF a = CName      Word (String → a)
                     | CLastname  Word (String → a)
                     | CNick      Word (String → a)
                     | CTalk      Word String a
                     | CContinue  String a
                     | CReply     String a
                     | CChoice    [String] (Int → a)
                     | CChoice_   [(String, Free ConversationF ())] a -- TODO highly likely, this one can be implemented in terms of Choice
                     | CDescribe  String a
                     deriving (Functor)


instance ConversationAPI (Free ConversationF) where
    name i = Free $ CName i Pure

    lastname i = Free $ CLastname i Pure

    nick i = Free $ CNick i Pure

    talk i s = Free $ CTalk i s (Pure ())

    continue s = Free $ CContinue s (Pure ())

    reply s = Free $ CReply s (Pure ())

    choice opts = Free $ CChoice opts Pure

    choice_ opts = Free $ CChoice_ opts (Pure ())

    describe s = Free $ CDescribe s (Pure ())

    (|=>) = (,)



-- TODO this COULD work, but I see no reason to push it forward
--      conversation monads would be much better choice
prepend ∷ ConversationNode → ConversationNode → ConversationNode
prepend End (TalkNode s i ps n)   = TalkNode s i ps (prepend End n)
prepend End (DescriptionNode s n) = DescriptionNode s (prepend End n)
prepend End (ChoiceNode o ns)     = ChoiceNode o ns
prepend cn (TalkNode s i ps n)    = TalkNode s i ps (prepend n cn)
prepend cn (DescriptionNode s n)  = DescriptionNode s (prepend n cn)
prepend cn (ChoiceNode o ns)      = ChoiceNode o ns
prepend cn _                      = cn


runConversationF_temp ∷ Free ConversationF a → ConversationNode
runConversationF_temp = runConversationF ["Carla", "Whoeverelse"] 0 1 End

-- TODO I should get away with ConversationNode alltogether and use free monads to render conversations
--      in realtime, adjusting parameters as necessary
runConversationF ∷ [String] → Word → Word → ConversationNode → Free ConversationF a → ConversationNode
runConversationF ps curr prev cn (Free (CName i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i))

runConversationF ps curr prev cn (Free (CLastname i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i)) -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CNick i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i)) -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CTalk i s n)) =
    runConversationF ps i (bool curr prev $ i == curr) (TalkNode s i ps End `prepend` cn) n

runConversationF ps curr prev cn (Free (CContinue s n)) =
    runConversationF ps curr prev (TalkNode s curr ps End `prepend` cn) n

runConversationF ps curr prev cn (Free (CReply s n)) =
    runConversationF ps prev curr (TalkNode s prev ps End `prepend` cn) n

runConversationF ps curr prev cn (Free (CChoice opts fn)) =
    runConversationF ps curr prev cn (fn 0)  -- TODO This is incorrect

runConversationF ps curr prev cn (Free (CChoice_ opts n)) =
    let (ss, prgs) = unzip opts
        nodes      = fmap (runConversationF ps curr prev cn) prgs
    in  runConversationF ps curr prev (ChoiceNode ss nodes `prepend` cn) n

runConversationF ps curr prev cn (Free (CDescribe s n)) =
    runConversationF ps curr prev (DescriptionNode s End `prepend` cn) n

runConversationF _ _ _ cn (Pure _) =
    cn



