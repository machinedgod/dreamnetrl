{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.Engine.Conversation
( ConversationAPI(..)
) where


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

