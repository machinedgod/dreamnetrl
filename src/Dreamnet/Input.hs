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

import Dreamnet.Character

--------------------------------------------------------------------------------

-- TODO Separate with types
data Event = Quit
           | Start

           -- Normal
           | Move (V2 Int)
           | NextAim
           | Examine
           | Interact

           -- Examination, conversation
           | ScrollUp
           | ScrollDown
           | SelectChoice
           | BackToNormal

           -- Interaction: pass it on (maybe pass Curses event?)
           | CustomInteraction Char
           deriving (Eq, Show)


class (Monad m) ⇒ MonadInput m where
    nextEvent ∷ CharacterState → m Event


newtype InputF a = InputF { runInputF ∷ Curses.Curses a }
                 deriving (Functor, Applicative, Monad)


instance MonadInput InputF where
    nextEvent cst = do
        ce ← InputF $ do
            w ← Curses.defaultWindow
            keepAskingUntilDelivered w
        case cursesToEvent cst ce of
            (Just e) → return e
            _        → nextEvent cst
        where
            keepAskingUntilDelivered w = do
                e ← Curses.getEvent w Nothing 
                case e of
                    Just e  → return e
                    _       → keepAskingUntilDelivered w
        



runInput ∷ (MonadCurses m) ⇒ InputF a → m a
runInput = liftCurses . runInputF


cursesToEvent ∷ CharacterState → Curses.Event → Maybe Event
cursesToEvent Normal (Curses.EventCharacter c) = normalStateEvent c
cursesToEvent (Examination _) (Curses.EventCharacter c) = examineStateEvent c
cursesToEvent Interaction (Curses.EventCharacter c) = Just $ CustomInteraction c
cursesToEvent (Talking _ _) (Curses.EventCharacter c) = converseStateEvent c
cursesToEvent (Listening _ _) (Curses.EventCharacter c) = converseStateEvent c
cursesToEvent _ _                                   = Nothing


normalStateEvent ∷ Char → Maybe Event
normalStateEvent 'h'  = Just $ Move (V2 -1  0)
normalStateEvent 'j'  = Just $ Move (V2  0  1)
normalStateEvent 'k'  = Just $ Move (V2  0 -1)
normalStateEvent 'l'  = Just $ Move (V2  1  0)
normalStateEvent 'y'  = Just $ Move (V2 -1 -1)
normalStateEvent 'u'  = Just $ Move (V2  1 -1)
normalStateEvent 'b'  = Just $ Move (V2 -1  1)
normalStateEvent 'n'  = Just $ Move (V2  1  1)
normalStateEvent '\t' = Just $ NextAim
normalStateEvent 'e'  = Just $ Examine
normalStateEvent ' '  = Just $ Interact
normalStateEvent 'q'  = Just $ Quit
normalStateEvent _    = Nothing


examineStateEvent ∷ Char → Maybe Event
examineStateEvent 'j' = Just ScrollDown
examineStateEvent 'k' = Just ScrollUp
examineStateEvent 'q' = Just BackToNormal
examineStateEvent _   = Nothing


converseStateEvent ∷ Char → Maybe Event
converseStateEvent 'j'  = Just ScrollDown
converseStateEvent 'k'  = Just ScrollUp
converseStateEvent '\n' = Just SelectChoice
converseStateEvent _    = Nothing
