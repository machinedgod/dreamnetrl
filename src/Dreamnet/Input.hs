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
, nextAllowedCharEvent
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
                | Get
                | UseHeld
                | Wear
                | StoreIn
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
                      | BackOut
                      deriving (Eq, Show)


--------------------------------------------------------------------------------

nextWorldEvent ∷ C.Curses WorldEvent
nextWorldEvent = repeatUntilEvent worldEvent
    where
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
        worldEvent (C.EventCharacter 'g')  = Just   Get
        worldEvent (C.EventCharacter 'f')  = Just   UseHeld
        worldEvent (C.EventCharacter 'w')  = Just   Wear
        worldEvent (C.EventCharacter 'S')  = Just   StoreIn
        worldEvent (C.EventCharacter '.')  = Just   Wait
        worldEvent (C.EventCharacter '[')  = Just   LowerStance
        worldEvent (C.EventCharacter ']')  = Just   HigherStance
        worldEvent (C.EventCharacter 'i')  = Just   InventorySheet
        worldEvent (C.EventCharacter 'c')  = Just   CharacterSheet
        worldEvent (C.EventCharacter '\t') = Just   SwitchToHud
        worldEvent (C.EventCharacter 'q')  = Just   Quit
        worldEvent _                       = Nothing


nextUiEvent ∷ C.Curses UIEvent
nextUiEvent = repeatUntilEvent uiEvent
    where
        uiEvent (C.EventCharacter '\t')          = Just TabNext
        uiEvent (C.EventSpecialKey C.KeyBackTab) = Just TabPrevious
        uiEvent (C.EventCharacter 'h')           = Just MoveLeft
        uiEvent (C.EventCharacter 'j')           = Just MoveDown
        uiEvent (C.EventCharacter 'k')           = Just MoveUp
        uiEvent (C.EventCharacter 'l')           = Just MoveRight
        uiEvent (C.EventCharacter '\n')          = Just SelectChoice
        uiEvent (C.EventCharacter 'q')           = Just Back
        uiEvent _                                = Nothing



nextInteractionEvent ∷ C.Curses InteractionEvent
nextInteractionEvent = repeatUntilEvent interactionEvent
    where
        interactionEvent (C.EventCharacter '\ESC')          = Just BackOut
        interactionEvent (C.EventCharacter c)               = Just (PassThrough c)
        interactionEvent (C.EventSpecialKey C.KeyBackspace) = Just (PassThrough '\b')
        interactionEvent _                                  = Nothing


nextTargetSelectionEvent ∷ C.Curses (V2 Int)
nextTargetSelectionEvent = repeatUntilEvent targetEvent
    where
        -- TODO upgrade to have an 'Abort' event too
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


nextAllowedCharEvent ∷ [Char] → C.Curses Char
nextAllowedCharEvent ac = repeatUntilEvent charEvent
    where
        charEvent (C.EventCharacter c) = if c `elem` ac then Just c else Nothing
        charEvent _                    = Nothing


repeatUntilEvent ∷ (C.Event → Maybe a) → C.Curses a
repeatUntilEvent f = do
    C.defaultWindow >>= fmap f . event >>= \case
        (Just e) → pure e
        _        → repeatUntilEvent f
    where
        event w = fromJust <$> C.getEvent w Nothing


--------------------------------------------------------------------------------

