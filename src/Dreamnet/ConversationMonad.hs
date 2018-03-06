{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ConversationMonad
( ConversationF
, runConversationF_temp
) where


import Safe                (at)
import Control.Monad.Free  (Free(Pure, Free))
import Data.Bool           (bool)
import Data.Monoid         ((<>))

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


runConversationF_temp ∷ [String] → Free ConversationF a → ConversationNode
runConversationF_temp names = runConversationF names 0 1 End


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

runConversationF _ _ _ cn (Pure _) =
    cn



