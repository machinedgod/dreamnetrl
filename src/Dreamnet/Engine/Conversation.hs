{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Engine.Conversation
( ConversationAPI(..)
, ConversationF(..)
) where


import Control.Monad.Free (Free(Free, Pure))

--------------------------------------------------------------------------------

-- TODO consider using package 'naturals' to add safety to these ints
-- Words just wrap around so they're useless
class ConversationAPI o c | c → o where
    name        ∷ Int → c String
    lastname    ∷ Int → c String
    nick        ∷ Int → c String
    talk        ∷ Int → String → c ()
    choice      ∷ [String] → c Int
    describe    ∷ String → c ()
    receiveItem ∷ Int → o → c ()
    (|=>)       ∷ String → c () → (String, c ())

--------------------------------------------------------------------------------

data ConversationF o a = CName         Int (String → a)
                       | CLastname     Int (String → a)
                       | CNick         Int (String → a)
                       | CTalk         Int String a
                       -- | CContinue     String a -- TODO can I implement these in terms of Talk?
                       -- | CReply        String a -- TODO can I implement these in terms of Talk?
                       | CChoice       [String] (Int → a)
                       | CDescribe     String a
                       | CReceiveItem  Int o a
                       deriving (Functor)


-- So that we can debug-print characters
instance Show (ConversationF o a) where
    show _ = "[CONV]"


instance ConversationAPI o (Free (ConversationF o)) where
    name i = Free $ CName i Pure

    lastname i = Free $ CLastname i Pure

    nick i = Free $ CNick i Pure

    talk i s = Free $ CTalk i s (Pure ())

    --continue s = Free $ CContinue s (Pure ())

    --reply s = Free $ CReply s (Pure ())

    choice opts = Free $ CChoice opts Pure

    describe s = Free $ CDescribe s (Pure ())
     
    receiveItem i o = Free $ CReceiveItem i o (Pure ())

    (|=>) = (,)

