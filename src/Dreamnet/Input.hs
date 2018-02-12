{-# LANGUAGE UnicodeSyntax, NegativeLiterals, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Input
( Event(..)
, WorldEvent(..)
, UIEvent(..)
, TargetEvent(..)

, nextEvent
) where


import Data.Maybe (fromJust)
import Linear     (V2(V2))

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace))
import Dreamnet.GameState

--------------------------------------------------------------------------------

data WorldEvent = Move (V2 Int)
                | Examine
                | Operate
                | Talk
                | UseHeld
                | Get
                | Wait
                | InventorySheet
                | CharacterSheet
                | SelectTeamMember  Int
                deriving (Eq, Show)


data UIEvent = MoveUp
             | MoveDown
             | SelectChoice
             | Back
             deriving (Eq, Show)


data TargetEvent = Aim  (V2 Int)
                 deriving (Eq, Show)


data Event = Quit
           | WorldEv      WorldEvent
           | UIEv         UIEvent
           | TargetEv     TargetEvent
           | PassThrough  Char
           deriving (Eq, Show)


nextEvent ∷ GameState → C.Curses Event
nextEvent cst = do
    C.defaultWindow >>= fmap (cursesToEvent cst) . event >>= \case
        (Just e) → pure e
        _        → nextEvent cst
    where
        event w = fromJust <$> C.getEvent w Nothing


cursesToEvent ∷ GameState → C.Event → Maybe Event
cursesToEvent _               (C.EventCharacter '\ESC') = Just Quit
cursesToEvent Normal          (C.EventCharacter c)      = WorldEv <$> worldEvent c
cursesToEvent TargetSelection (C.EventCharacter c)      = TargetEv <$> targetEvent c
cursesToEvent Examination     (C.EventCharacter c)      = UIEv <$> uiEvent c
cursesToEvent Conversation    (C.EventCharacter c)      = UIEv <$> uiEvent c
cursesToEvent InventoryUI     (C.EventCharacter c)      = UIEv <$> uiEvent c
cursesToEvent CharacterUI     (C.EventCharacter c)      = UIEv <$> uiEvent c
cursesToEvent Operation       (C.EventCharacter c)      = Just (PassThrough c)
cursesToEvent Operation       (C.EventSpecialKey C.KeyBackspace) = Just (PassThrough '\b')
cursesToEvent _ _                                       = Nothing


worldEvent ∷ Char → Maybe WorldEvent
worldEvent 'h'  = Just $ Move (V2 -1  0)
worldEvent 'j'  = Just $ Move (V2  0  1)
worldEvent 'k'  = Just $ Move (V2  0 -1)
worldEvent 'l'  = Just $ Move (V2  1  0)
worldEvent 'y'  = Just $ Move (V2 -1 -1)
worldEvent 'u'  = Just $ Move (V2  1 -1)
worldEvent 'b'  = Just $ Move (V2 -1  1)
worldEvent 'n'  = Just $ Move (V2  1  1)
worldEvent 'e'  = Just   Examine
worldEvent 'o'  = Just   Operate
worldEvent 't'  = Just   Talk
worldEvent 'f'  = Just   UseHeld
worldEvent 'g'  = Just   Get
worldEvent '.'  = Just   Wait
worldEvent 'i'  = Just   InventorySheet
worldEvent 'c'  = Just   CharacterSheet
worldEvent '1'  = Just $ SelectTeamMember 0
worldEvent '2'  = Just $ SelectTeamMember 1
worldEvent '3'  = Just $ SelectTeamMember 2
worldEvent '4'  = Just $ SelectTeamMember 3
worldEvent '5'  = Just $ SelectTeamMember 4
worldEvent '6'  = Just $ SelectTeamMember 5
worldEvent '7'  = Just $ SelectTeamMember 6
worldEvent '8'  = Just $ SelectTeamMember 7
worldEvent '9'  = Just $ SelectTeamMember 8
worldEvent '0'  = Just $ SelectTeamMember 9
worldEvent _    = Nothing


targetEvent ∷ Char → Maybe TargetEvent
targetEvent '.'  = Just $ Aim (V2  0  0)
targetEvent 'h'  = Just $ Aim (V2 -1  0)
targetEvent 'j'  = Just $ Aim (V2  0  1)
targetEvent 'k'  = Just $ Aim (V2  0 -1)
targetEvent 'l'  = Just $ Aim (V2  1  0)
targetEvent 'y'  = Just $ Aim (V2 -1 -1)
targetEvent 'u'  = Just $ Aim (V2  1 -1)
targetEvent 'b'  = Just $ Aim (V2 -1  1)
targetEvent 'n'  = Just $ Aim (V2  1  1)
targetEvent _    = Nothing


uiEvent ∷ Char → Maybe UIEvent
uiEvent 'j'  = Just MoveDown
uiEvent 'k'  = Just MoveUp
uiEvent '\n' = Just SelectChoice
uiEvent 'q'  = Just Back
uiEvent _    = Nothing

