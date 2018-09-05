{-# LANGUAGE UnicodeSyntax, NegativeLiterals, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Engine.Input
( WorldEvent(..), nextWorldEvent

, UIEvent(..), nextUiEvent

, TargetEvent(..), nextTargetSelectionEvent

, ChoiceEvent(..), nextChoiceEvent

, InteractionEvent(..), nextInteractionEvent
) where


import Data.Maybe (fromJust)
import Linear     (V2(V2))

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace, KeyBackTab))

--------------------------------------------------------------------------------

data WorldEvent = Move (V2 Int)
                | MoveCamera (V2 Int)
                | Examine
                | Operate
                | ExamineHeld
                | OperateHeld
                | OperateHeldOn
                | Talk
                | Get
                | Wear
                | StoreIn
                | PullFrom
                | Wait
                | HigherStance
                | LowerStance
                | InventorySheet
                | CharacterSheet

                | GiveCommand

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


data TargetEvent = MoveReticule (V2 Int)
                 | HigherTarget
                 | LowerTarget
                 | NextTarget
                 | PreviousTarget
                 | ConfirmTarget
                 | CancelTargeting
                 deriving (Eq, Show)


data ChoiceEvent = ChoiceCharacter Char
                 | CancelChoice
                 deriving (Eq, Show)


data InteractionEvent = PassThrough Char
                      | BackOut
                      deriving (Eq, Show)


--------------------------------------------------------------------------------

-- TODO for distinguishing between world and held, and maybe some other stuff,
--      should I use 'stateful' keys like with vim? Eg. 'e' is examine,
--      but '<leader>e' is examine held item?
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
        worldEvent (C.EventCharacter 'H')  = Just $ MoveCamera (V2 -1  0)
        worldEvent (C.EventCharacter 'J')  = Just $ MoveCamera (V2  0  1)
        worldEvent (C.EventCharacter 'K')  = Just $ MoveCamera (V2  0 -1)
        worldEvent (C.EventCharacter 'L')  = Just $ MoveCamera (V2  1  0)
        worldEvent (C.EventCharacter 'Y')  = Just $ MoveCamera (V2 -1 -1)
        worldEvent (C.EventCharacter 'U')  = Just $ MoveCamera (V2  1 -1)
        worldEvent (C.EventCharacter 'B')  = Just $ MoveCamera (V2 -1  1)
        worldEvent (C.EventCharacter 'N')  = Just $ MoveCamera (V2  1  1)
        worldEvent (C.EventCharacter 'e')  = Just   Examine
        worldEvent (C.EventCharacter 'o')  = Just   Operate
        worldEvent (C.EventCharacter 'E')  = Just   ExamineHeld
        worldEvent (C.EventCharacter 'f')  = Just   OperateHeld
        worldEvent (C.EventCharacter 'F')  = Just   OperateHeldOn
        worldEvent (C.EventCharacter 't')  = Just   Talk
        worldEvent (C.EventCharacter 'g')  = Just   Get
        worldEvent (C.EventCharacter 'w')  = Just   Wear
        worldEvent (C.EventCharacter 'S')  = Just   StoreIn
        worldEvent (C.EventCharacter 'P')  = Just   PullFrom
        worldEvent (C.EventCharacter '.')  = Just   Wait
        worldEvent (C.EventCharacter '[')  = Just   LowerStance
        worldEvent (C.EventCharacter ']')  = Just   HigherStance
        worldEvent (C.EventCharacter 'i')  = Just   InventorySheet
        worldEvent (C.EventCharacter 'c')  = Just   CharacterSheet
        worldEvent (C.EventCharacter 'C')  = Just   GiveCommand
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


nextTargetSelectionEvent ∷ C.Curses TargetEvent
nextTargetSelectionEvent = repeatUntilEvent targetEvent
    where
        targetEvent (C.EventCharacter 'h')           = Just $ MoveReticule (V2 -1  0)
        targetEvent (C.EventCharacter 'j')           = Just $ MoveReticule (V2  0  1)
        targetEvent (C.EventCharacter 'k')           = Just $ MoveReticule (V2  0 -1)
        targetEvent (C.EventCharacter 'l')           = Just $ MoveReticule (V2  1  0)
        targetEvent (C.EventCharacter 'y')           = Just $ MoveReticule (V2 -1 -1)
        targetEvent (C.EventCharacter 'u')           = Just $ MoveReticule (V2  1 -1)
        targetEvent (C.EventCharacter 'b')           = Just $ MoveReticule (V2 -1  1)
        targetEvent (C.EventCharacter 'n')           = Just $ MoveReticule (V2  1  1)
        targetEvent (C.EventCharacter 'H')           = Just   LowerTarget
        targetEvent (C.EventCharacter 'L')           = Just   HigherTarget
        targetEvent (C.EventCharacter '\t')          = Just   NextTarget
        targetEvent (C.EventSpecialKey C.KeyBackTab) = Just   PreviousTarget
        targetEvent (C.EventCharacter '\n')          = Just   ConfirmTarget
        targetEvent (C.EventCharacter 'q')           = Just   CancelTargeting
        targetEvent _                                = Nothing


nextChoiceEvent ∷ String → C.Curses ChoiceEvent
nextChoiceEvent ac = repeatUntilEvent charEvent
    where
        charEvent (C.EventCharacter 'q') = Just CancelChoice
        charEvent (C.EventCharacter c)   = if c `elem` ac then Just (ChoiceCharacter c) else Nothing
        charEvent _                      = Nothing


repeatUntilEvent ∷ (C.Event → Maybe a) → C.Curses a
repeatUntilEvent f =
    C.defaultWindow >>= fmap f . event >>= \case
        (Just e) → pure e
        _        → repeatUntilEvent f
    where
        event w = fromJust <$> C.getEvent w Nothing


--------------------------------------------------------------------------------

