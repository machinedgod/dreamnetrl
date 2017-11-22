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
           | NextAim
           | Examine
           | Interact
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
        case cursesToEvent ce of
            (Just e) → return e
            _        → nextEvent
        where
            keepAskingUntilDelivered w = do
                e ← Curses.getEvent w Nothing 
                case e of
                    Just e  → return e
                    _       → keepAskingUntilDelivered w
        



runInput ∷ (MonadCurses m) ⇒ InputF a → m a
runInput = liftCurses . runInputF


cursesToEvent ∷ Curses.Event → Maybe Event
cursesToEvent (Curses.EventCharacter 'h') = Just $ Move (V2 -1  0)
cursesToEvent (Curses.EventCharacter 'j') = Just $ Move (V2  0  1)
cursesToEvent (Curses.EventCharacter 'k') = Just $ Move (V2  0 -1)
cursesToEvent (Curses.EventCharacter 'l') = Just $ Move (V2  1  0)
cursesToEvent (Curses.EventCharacter 'y') = Just $ Move (V2 -1 -1)
cursesToEvent (Curses.EventCharacter 'u') = Just $ Move (V2  1 -1)
cursesToEvent (Curses.EventCharacter 'b') = Just $ Move (V2 -1  1)
cursesToEvent (Curses.EventCharacter 'n') = Just $ Move (V2  1  1)
 
cursesToEvent (Curses.EventCharacter '\t') = Just $ NextAim
cursesToEvent (Curses.EventCharacter 'e')  = Just $ Examine
cursesToEvent (Curses.EventCharacter ' ')  = Just $ Interact

cursesToEvent (Curses.EventCharacter 'q') = Just $ Quit

cursesToEvent k = Nothing
