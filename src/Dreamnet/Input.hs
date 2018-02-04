{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Input
( Event(..)
, WorldEvent(..)
, UIEvent(..)

, MonadInput(..)
, InputF
, runInput
) where

import Linear

import UI.NCurses.Class
import qualified UI.NCurses as Curses

import Dreamnet.GameState

--------------------------------------------------------------------------------

data WorldEvent = Move (V2 Int)
                | NextAim
                | Examine
                | Interact
                | Get
                | Wait
                | InventorySheet
                | CharacterSheet
                deriving (Eq, Show)


data UIEvent = MoveUp
             | MoveDown
             | SelectChoice
             | Back
             deriving (Eq, Show)


data Event = Quit
           | WorldEv      WorldEvent
           | UIEv         UIEvent
           | PassThrough  Char
           deriving (Eq, Show)


class (Monad m) ⇒ MonadInput m where
    nextEvent ∷ GameState → m Event


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
                me ← Curses.getEvent w Nothing 
                case me of
                    Just e → return e
                    _      → keepAskingUntilDelivered w
        



runInput ∷ (MonadCurses m) ⇒ InputF a → m a
runInput = liftCurses . runInputF


cursesToEvent ∷ GameState → Curses.Event → Maybe Event
cursesToEvent _                  (Curses.EventCharacter   '\ESC')              = Just Quit
cursesToEvent Normal             (Curses.EventCharacter   c)                   = WorldEv <$> worldEvent c
cursesToEvent (Examination _)    (Curses.EventCharacter   c)                   = UIEv <$> uiEvent c
cursesToEvent (Conversation _ _) (Curses.EventCharacter   c)                   = UIEv <$> uiEvent c
cursesToEvent InventoryUI        (Curses.EventCharacter   c)                   = UIEv <$> uiEvent c
cursesToEvent CharacterUI        (Curses.EventCharacter   c)                   = UIEv <$> uiEvent c
cursesToEvent Interaction        (Curses.EventCharacter   c)                   = Just (PassThrough c)
cursesToEvent Interaction        (Curses.EventSpecialKey  Curses.KeyBackspace) = Just (PassThrough '\b')
cursesToEvent _ _                                                              = Nothing

--normalStateEvent 'q'  = Just $ Quit

worldEvent ∷ Char → Maybe WorldEvent
worldEvent 'h'  = Just $ Move (V2 -1  0)
worldEvent 'j'  = Just $ Move (V2  0  1)
worldEvent 'k'  = Just $ Move (V2  0 -1)
worldEvent 'l'  = Just $ Move (V2  1  0)
worldEvent 'y'  = Just $ Move (V2 -1 -1)
worldEvent 'u'  = Just $ Move (V2  1 -1)
worldEvent 'b'  = Just $ Move (V2 -1  1)
worldEvent 'n'  = Just $ Move (V2  1  1)
worldEvent '\t' = Just   NextAim
worldEvent 'e'  = Just   Examine
worldEvent ' '  = Just   Interact
worldEvent 'g'  = Just   Get
worldEvent '.'  = Just   Wait
worldEvent 'i'  = Just   InventorySheet
worldEvent 'c'  = Just   CharacterSheet
worldEvent _    = Nothing


uiEvent ∷ Char → Maybe UIEvent
uiEvent 'j'  = Just MoveDown
uiEvent 'k'  = Just MoveUp
uiEvent '\n' = Just SelectChoice
uiEvent 'q'  = Just Back
uiEvent _    = Nothing

