{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Input
( module Linear
, Event(..)

, MonadInput(..)
, InputF
, runInput
) where

import Control.Monad.Trans
import Linear

import UI.NCurses.Class
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

data Event = Move (V2 Int)
           | Aim  (V2 Int)
           | Open
           | Close
           | Quit
           | Start
           deriving (Eq, Show)


class (Monad m) ⇒ MonadInput m where
    nextEvent ∷ m Event


newtype InputF a = InputF { runInputF ∷ Curses.Curses a }
                 deriving (Functor, Applicative, Monad)


instance MonadInput InputF where
    nextEvent = do
        ce ← InputF $ do
            w ← Curses.defaultWindow
            keepAskingUntilDelivered w
        case ce of
            Curses.EventCharacter c → return $ charToEvent c
            _                       → nextEvent
        where
            keepAskingUntilDelivered w = do
                e ← Curses.getEvent w Nothing 
                case e of
                    Just e  → return e
                    _       → keepAskingUntilDelivered w
        



runInput ∷ (MonadCurses m) ⇒ InputF a → m a
runInput = liftCurses . runInputF


charToEvent ∷ Char → Event
charToEvent 'h' = Move (V2 -1  0)
charToEvent 'j' = Move (V2  0  1)
charToEvent 'k' = Move (V2  0 -1)
charToEvent 'l' = Move (V2  1  0)
charToEvent 'y' = Move (V2 -1 -1)
charToEvent 'u' = Move (V2  1 -1)
charToEvent 'b' = Move (V2 -1  1)
charToEvent 'n' = Move (V2  1  1)

charToEvent 'H' = Aim (V2 -1  0)
charToEvent 'J' = Aim (V2  0  1)
charToEvent 'K' = Aim (V2  0 -1)
charToEvent 'L' = Aim (V2  1  0)
charToEvent 'Y' = Aim (V2 -1 -1)
charToEvent 'U' = Aim (V2  1 -1)
charToEvent 'B' = Aim (V2 -1  1)
charToEvent 'N' = Aim (V2  1  1)

charToEvent 'o' = Open
charToEvent 'c' = Close

charToEvent 'q' = Quit

charToEvent k   = error $ "unmapped key: " ++ [k]
