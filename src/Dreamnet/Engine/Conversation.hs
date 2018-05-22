{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Dreamnet.Engine.Conversation
( ConversationAPI(..)
) where


--------------------------------------------------------------------------------

class ConversationAPI o c | c → o where
    name        ∷ Word → c String
    lastname    ∷ Word → c String
    nick        ∷ Word → c String
    talk        ∷ Word → String → c ()
    continue    ∷ String → c ()
    reply       ∷ String → c ()
    choice      ∷ [String] → c Int
    choice_     ∷ [(String, c ())] → c ()
    describe    ∷ String → c ()
    receiveItem ∷ Word → o → c ()
    (|=>)       ∷ String → c () → (String, c ())

