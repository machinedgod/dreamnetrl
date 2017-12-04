{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.GamePipes
where


import Pipes
import Linear


testGetEvent ∷ Producer Char IO ()
testGetEvent = lift getChar >>= yield


testInjectTimePulse ∷ Float → Producer Float IO ()
testInjectTimePulse f = yield f

--------------------------------------------------------------------------------

data Event = Quit
           | Interact
           | Move (V2 Int)
           deriving (Show)

testMaybeEvent ∷ (Monad m) ⇒ Consumer Char m (Maybe Event)
testMaybeEvent = do
    c ← await
    return (charToEvent c)
    where
        charToEvent 'q' = Just $ Quit
        charToEvent ' ' = Just $ Interact
        charToEvent 'h' = Just $ Move (V2 -1  0)
        charToEvent 'j' = Just $ Move (V2  0  1)
        charToEvent 'k' = Just $ Move (V2  0 -1)
        charToEvent 'l' = Just $ Move (V2  1  0)
        charToEvent 'y' = Just $ Move (V2 -1 -1)
        charToEvent 'u' = Just $ Move (V2  1 -1)
        charToEvent 'b' = Just $ Move (V2 -1  1)
        charToEvent 'n' = Just $ Move (V2  1  1)
        charToEvent _   = Nothing -- ?


testRetryIfNothingEvent ∷ (Monad m) ⇒ Consumer (Maybe Event) m Event
testRetryIfNothingEvent = do
    me ← await
    case me of
        Just e  → return e
        Nothing → testRetryIfNothingEvent


testUpdate ∷ Consumer Event IO ()
testUpdate = do
    e ← await
    case e of
        Quit     → lift $ putStrLn "Quit command"
        Interact → lift $ putStrLn "Interact command"
        Move v   → lift $ putStrLn $ "Moving " <> show v
