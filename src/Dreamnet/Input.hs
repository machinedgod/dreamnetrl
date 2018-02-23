{-# LANGUAGE UnicodeSyntax, NegativeLiterals, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Input
( WorldEvent(..)
, UIEvent(..)
, InteractionEvent(..)

, nextWorldEvent
, nextUiEvent
, nextInteractionEvent
, nextTargetSelectionEvent
) where


import Data.Maybe (fromJust)
import Linear     (V2(V2))

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace, KeyBackTab))

--------------------------------------------------------------------------------

data WorldEvent = Move (V2 Int)
                | Examine
                | Operate
                | Talk
                | UseHeld
                | Get
                | Wait
                | HigherStance
                | LowerStance
                | InventorySheet
                | CharacterSheet
                -- | SelectTeamMember  Int -- TODO replace with giving commands

                | SwitchToHud

                | Quit
                deriving (Eq, Show)


data UIEvent = MoveUp
             | MoveDown
             | MoveLeft
             | MoveRight
             | TabNext
             | TabPrevious
             | SelectChoice
             | Back
             deriving (Eq, Show)


data InteractionEvent = PassThrough Char
                      deriving (Eq, Show)


--------------------------------------------------------------------------------

nextWorldEvent ∷ C.Curses WorldEvent
nextWorldEvent = repeatUntilEvent worldEvent


nextUiEvent ∷ C.Curses UIEvent
nextUiEvent = repeatUntilEvent uiEvent


nextInteractionEvent ∷ C.Curses InteractionEvent
nextInteractionEvent = repeatUntilEvent interactionEvent
    where
        interactionEvent (C.EventCharacter c)               = Just (PassThrough c)
        interactionEvent (C.EventSpecialKey C.KeyBackspace) = Just (PassThrough '\b')
        interactionEvent _                                  = Nothing


nextTargetSelectionEvent ∷ C.Curses (V2 Int)
nextTargetSelectionEvent = repeatUntilEvent targetEvent


repeatUntilEvent ∷ (C.Event → Maybe a) → C.Curses a
repeatUntilEvent f = do
    C.defaultWindow >>= fmap f . event >>= \case
        (Just e) → pure e
        _        → repeatUntilEvent f
    where
        event w = fromJust <$> C.getEvent w Nothing


--------------------------------------------------------------------------------

--cursesToEvent _               (C.EventCharacter '\ESC') = Just Quit


worldEvent ∷ C.Event → Maybe WorldEvent
worldEvent (C.EventCharacter 'h')  = Just $ Move (V2 -1  0)
worldEvent (C.EventCharacter 'j')  = Just $ Move (V2  0  1)
worldEvent (C.EventCharacter 'k')  = Just $ Move (V2  0 -1)
worldEvent (C.EventCharacter 'l')  = Just $ Move (V2  1  0)
worldEvent (C.EventCharacter 'y')  = Just $ Move (V2 -1 -1)
worldEvent (C.EventCharacter 'u')  = Just $ Move (V2  1 -1)
worldEvent (C.EventCharacter 'b')  = Just $ Move (V2 -1  1)
worldEvent (C.EventCharacter 'n')  = Just $ Move (V2  1  1)
worldEvent (C.EventCharacter 'e')  = Just   Examine
worldEvent (C.EventCharacter 'o')  = Just   Operate
worldEvent (C.EventCharacter 't')  = Just   Talk
worldEvent (C.EventCharacter 'f')  = Just   UseHeld
worldEvent (C.EventCharacter 'g')  = Just   Get
worldEvent (C.EventCharacter '.')  = Just   Wait
worldEvent (C.EventCharacter '[')  = Just   LowerStance
worldEvent (C.EventCharacter ']')  = Just   HigherStance
worldEvent (C.EventCharacter 'i')  = Just   InventorySheet
worldEvent (C.EventCharacter 'c')  = Just   CharacterSheet
--worldEvent (C.EventCharacter '1')  = Just $ SelectTeamMember 0
--worldEvent (C.EventCharacter '2')  = Just $ SelectTeamMember 1
--worldEvent (C.EventCharacter '3')  = Just $ SelectTeamMember 2
--worldEvent (C.EventCharacter '4')  = Just $ SelectTeamMember 3
--worldEvent (C.EventCharacter '5')  = Just $ SelectTeamMember 4
--worldEvent (C.EventCharacter '6')  = Just $ SelectTeamMember 5
--worldEvent (C.EventCharacter '7')  = Just $ SelectTeamMember 6
--worldEvent (C.EventCharacter '8')  = Just $ SelectTeamMember 7
--worldEvent (C.EventCharacter '9')  = Just $ SelectTeamMember 8
--worldEvent (C.EventCharacter '0')  = Just $ SelectTeamMember 9
worldEvent (C.EventCharacter '\t') = Just   SwitchToHud
worldEvent (C.EventCharacter 'q')  = Just   Quit
worldEvent _                       = Nothing


-- TODO upgrade to have an 'Abort' event too
targetEvent ∷ C.Event → Maybe (V2 Int)
targetEvent (C.EventCharacter '.') = Just (V2  0  0)
targetEvent (C.EventCharacter 'h') = Just (V2 -1  0)
targetEvent (C.EventCharacter 'j') = Just (V2  0  1)
targetEvent (C.EventCharacter 'k') = Just (V2  0 -1)
targetEvent (C.EventCharacter 'l') = Just (V2  1  0)
targetEvent (C.EventCharacter 'y') = Just (V2 -1 -1)
targetEvent (C.EventCharacter 'u') = Just (V2  1 -1)
targetEvent (C.EventCharacter 'b') = Just (V2 -1  1)
targetEvent (C.EventCharacter 'n') = Just (V2  1  1)
targetEvent _                      = Nothing


uiEvent ∷ C.Event → Maybe UIEvent
uiEvent (C.EventCharacter '\t')          = Just TabNext
uiEvent (C.EventSpecialKey C.KeyBackTab) = Just TabPrevious
uiEvent (C.EventCharacter 'h')           = Just MoveLeft
uiEvent (C.EventCharacter 'j')           = Just MoveDown
uiEvent (C.EventCharacter 'k')           = Just MoveUp
uiEvent (C.EventCharacter 'l')           = Just MoveRight
uiEvent (C.EventCharacter '\n')          = Just SelectChoice
uiEvent (C.EventCharacter 'q')           = Just Back
uiEvent _                                = Nothing

