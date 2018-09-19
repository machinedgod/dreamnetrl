{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Dreamnet.Engine.Input
( BackEvent(..), WorldEvent(..), nextWorldEvent, TypedWorldEvent(..), withTypedWorldEvent

, UIEvent(..), nextUiEvent, TypedUiEvent(..), withTypedUIEvent

, TargetEvent(..), nextTargetSelectionEvent, TypedTargetEvent(..), withTypedTargetEvent

, ChoiceEvent(..), nextChoiceEvent

, PassThrough(..), nextPassThrough
) where


import Data.Maybe          (fromJust)
import Linear              (V2(V2))

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace, KeyBackTab))

--------------------------------------------------------------------------------

data BackEvent = Back

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


-- TODO for distinguishing between world and held, and maybe some other stuff,
--      should I use 'stateful' keys like with vim? Eg. 'e' is examine,
--      but '<leader>e' is examine held item?
nextWorldEvent ∷ C.Curses (Either BackEvent WorldEvent)
nextWorldEvent = repeatUntilEvent worldEvent
    where
        worldEvent (C.EventCharacter 'h')  = Just $ Right $ Move (V2 -1  0)
        worldEvent (C.EventCharacter 'j')  = Just $ Right $ Move (V2  0  1)
        worldEvent (C.EventCharacter 'k')  = Just $ Right $ Move (V2  0 -1)
        worldEvent (C.EventCharacter 'l')  = Just $ Right $ Move (V2  1  0)
        worldEvent (C.EventCharacter 'y')  = Just $ Right $ Move (V2 -1 -1)
        worldEvent (C.EventCharacter 'u')  = Just $ Right $ Move (V2  1 -1)
        worldEvent (C.EventCharacter 'b')  = Just $ Right $ Move (V2 -1  1)
        worldEvent (C.EventCharacter 'n')  = Just $ Right $ Move (V2  1  1)
        worldEvent (C.EventCharacter 'H')  = Just $ Right $ MoveCamera (V2 -1  0)
        worldEvent (C.EventCharacter 'J')  = Just $ Right $ MoveCamera (V2  0  1)
        worldEvent (C.EventCharacter 'K')  = Just $ Right $ MoveCamera (V2  0 -1)
        worldEvent (C.EventCharacter 'L')  = Just $ Right $ MoveCamera (V2  1  0)
        worldEvent (C.EventCharacter 'Y')  = Just $ Right $ MoveCamera (V2 -1 -1)
        worldEvent (C.EventCharacter 'U')  = Just $ Right $ MoveCamera (V2  1 -1)
        worldEvent (C.EventCharacter 'B')  = Just $ Right $ MoveCamera (V2 -1  1)
        worldEvent (C.EventCharacter 'N')  = Just $ Right $ MoveCamera (V2  1  1)
        worldEvent (C.EventCharacter 'e')  = Just $ Right   Examine
        worldEvent (C.EventCharacter 'o')  = Just $ Right   Operate
        worldEvent (C.EventCharacter 'E')  = Just $ Right   ExamineHeld
        worldEvent (C.EventCharacter 'f')  = Just $ Right   OperateHeld
        worldEvent (C.EventCharacter 'F')  = Just $ Right   OperateHeldOn
        worldEvent (C.EventCharacter 't')  = Just $ Right   Talk
        worldEvent (C.EventCharacter 'g')  = Just $ Right   Get
        worldEvent (C.EventCharacter 'w')  = Just $ Right   Wear
        worldEvent (C.EventCharacter 'S')  = Just $ Right   StoreIn
        worldEvent (C.EventCharacter 'P')  = Just $ Right   PullFrom
        worldEvent (C.EventCharacter '.')  = Just $ Right   Wait
        worldEvent (C.EventCharacter '[')  = Just $ Right   LowerStance
        worldEvent (C.EventCharacter ']')  = Just $ Right   HigherStance
        worldEvent (C.EventCharacter 'i')  = Just $ Right   InventorySheet
        worldEvent (C.EventCharacter 'c')  = Just $ Right   CharacterSheet
        worldEvent (C.EventCharacter 'C')  = Just $ Right   GiveCommand
        worldEvent (C.EventCharacter '\t') = Just $ Right   SwitchToHud
        worldEvent (C.EventCharacter 'q')  = Just $ Left    Back
        worldEvent _                       = Nothing


data TypedWorldEvent ∷ WorldEvent → * where
    TyMove           ∷ V2 Int → TypedWorldEvent ('Move ('V2 x y))
    TyMoveCamera     ∷ V2 Int → TypedWorldEvent ('MoveCamera ('V2 x y))
    TyExamine        ∷ TypedWorldEvent 'Examine
    TyOperate        ∷ TypedWorldEvent 'Operate
    TyExamineHeld    ∷ TypedWorldEvent 'ExamineHeld
    TyOperateHeld    ∷ TypedWorldEvent 'OperateHeld
    TyOperateHeldOn  ∷ TypedWorldEvent 'OperateHeldOn
    TyTalk           ∷ TypedWorldEvent 'Talk
    TyGet            ∷ TypedWorldEvent 'Get
    TyWear           ∷ TypedWorldEvent 'Wear
    TyStoreIn        ∷ TypedWorldEvent 'StoreIn
    TyPullFrom       ∷ TypedWorldEvent 'PullFrom
    TyWait           ∷ TypedWorldEvent 'Wait
    TyHigherStance   ∷ TypedWorldEvent 'HigherStance
    TyLowerStance    ∷ TypedWorldEvent 'LowerStance
    TyInventorySheet ∷ TypedWorldEvent 'InventorySheet
    TyCharacterSheet ∷ TypedWorldEvent 'CharacterSheet
    TyGiveCommand    ∷ TypedWorldEvent 'GiveCommand
    TySwitchToHud    ∷ TypedWorldEvent 'SwitchToHud


withTypedWorldEvent ∷ WorldEvent → (∀ e. TypedWorldEvent e → r) → r
withTypedWorldEvent (Move v)       f = f (TyMove v)
withTypedWorldEvent (MoveCamera v) f = f (TyMoveCamera v)
withTypedWorldEvent Examine        f = f TyExamine
withTypedWorldEvent Operate        f = f TyOperate
withTypedWorldEvent ExamineHeld    f = f TyExamineHeld
withTypedWorldEvent OperateHeld    f = f TyOperateHeld
withTypedWorldEvent OperateHeldOn  f = f TyOperateHeldOn
withTypedWorldEvent Talk           f = f TyTalk
withTypedWorldEvent Get            f = f TyGet
withTypedWorldEvent Wear           f = f TyWear
withTypedWorldEvent StoreIn        f = f TyStoreIn
withTypedWorldEvent PullFrom       f = f TyPullFrom
withTypedWorldEvent Wait           f = f TyWait
withTypedWorldEvent HigherStance   f = f TyHigherStance
withTypedWorldEvent LowerStance    f = f TyLowerStance
withTypedWorldEvent InventorySheet f = f TyInventorySheet
withTypedWorldEvent CharacterSheet f = f TyCharacterSheet
withTypedWorldEvent GiveCommand    f = f TyGiveCommand
withTypedWorldEvent SwitchToHud    f = f TySwitchToHud

--------------------------------------------------------------------------------

data UIEvent = MoveUp
             | MoveDown
             | MoveLeft
             | MoveRight
             | TabNext
             | TabPrevious
             | SelectChoice


nextUiEvent ∷ C.Curses (Either BackEvent UIEvent)
nextUiEvent = repeatUntilEvent uiEvent
    where
        uiEvent (C.EventCharacter '\t')          = Just $ Right TabNext
        uiEvent (C.EventSpecialKey C.KeyBackTab) = Just $ Right TabPrevious
        uiEvent (C.EventCharacter 'h')           = Just $ Right MoveLeft
        uiEvent (C.EventCharacter 'j')           = Just $ Right MoveDown
        uiEvent (C.EventCharacter 'k')           = Just $ Right MoveUp
        uiEvent (C.EventCharacter 'l')           = Just $ Right MoveRight
        uiEvent (C.EventCharacter '\n')          = Just $ Right SelectChoice
        uiEvent (C.EventCharacter 'q')           = Just $ Left  Back
        uiEvent _                                = Nothing


data TypedUiEvent ∷ UIEvent → * where
    TyMoveUp       ∷ TypedUiEvent 'MoveUp
    TyMoveDown     ∷ TypedUiEvent 'MoveDown
    TyMoveLeft     ∷ TypedUiEvent 'MoveLeft
    TyMoveRight    ∷ TypedUiEvent 'MoveRight
    TyTabNext      ∷ TypedUiEvent 'TabNext
    TyTabPrevious  ∷ TypedUiEvent 'TabPrevious
    TySelectChoice ∷ TypedUiEvent 'SelectChoice


withTypedUIEvent ∷ UIEvent → (∀ e. TypedUiEvent e → r) → r
withTypedUIEvent TabNext      f = f TyTabNext
withTypedUIEvent TabPrevious  f = f TyTabPrevious
withTypedUIEvent MoveLeft     f = f TyMoveLeft
withTypedUIEvent MoveDown     f = f TyMoveDown
withTypedUIEvent MoveUp       f = f TyMoveUp
withTypedUIEvent MoveRight    f = f TyMoveRight
withTypedUIEvent SelectChoice f = f TySelectChoice

--------------------------------------------------------------------------------

newtype PassThrough = PassThrough Char

nextPassThrough ∷ C.Curses (Either BackEvent PassThrough)
nextPassThrough = repeatUntilEvent passThroughEvent
    where
        passThroughEvent (C.EventSpecialKey C.KeyBackspace) = Just $ Right (PassThrough '\b')
        passThroughEvent (C.EventCharacter c)
            | c ==  '\ESC' = Just $ Left  Back
            | otherwise    = Just $ Right (PassThrough c)
        passThroughEvent _ = Nothing

--------------------------------------------------------------------------------

data TargetEvent = MoveReticule (V2 Int)
                 | HigherTarget
                 | LowerTarget
                 | NextTarget
                 | PreviousTarget
                 | ConfirmTarget


nextTargetSelectionEvent ∷ C.Curses (Either BackEvent TargetEvent)
nextTargetSelectionEvent = repeatUntilEvent targetEvent
    where
        targetEvent (C.EventCharacter 'h')           = Just $ Right $ MoveReticule (V2 -1  0)
        targetEvent (C.EventCharacter 'j')           = Just $ Right $ MoveReticule (V2  0  1)
        targetEvent (C.EventCharacter 'k')           = Just $ Right $ MoveReticule (V2  0 -1)
        targetEvent (C.EventCharacter 'l')           = Just $ Right $ MoveReticule (V2  1  0)
        targetEvent (C.EventCharacter 'y')           = Just $ Right $ MoveReticule (V2 -1 -1)
        targetEvent (C.EventCharacter 'u')           = Just $ Right $ MoveReticule (V2  1 -1)
        targetEvent (C.EventCharacter 'b')           = Just $ Right $ MoveReticule (V2 -1  1)
        targetEvent (C.EventCharacter 'n')           = Just $ Right $ MoveReticule (V2  1  1)
        targetEvent (C.EventCharacter 'H')           = Just $ Right   LowerTarget
        targetEvent (C.EventCharacter 'L')           = Just $ Right   HigherTarget
        targetEvent (C.EventCharacter '\t')          = Just $ Right   NextTarget
        targetEvent (C.EventSpecialKey C.KeyBackTab) = Just $ Right   PreviousTarget
        targetEvent (C.EventCharacter '\n')          = Just $ Right   ConfirmTarget
        targetEvent (C.EventCharacter 'q')           = Just $ Left    Back
        targetEvent _                                = Nothing


data TypedTargetEvent ∷ TargetEvent → * where
    TyMoveReticule   ∷ V2 Int → TypedTargetEvent ('MoveReticule ('V2 x y))
    TyHigherTarget   ∷ TypedTargetEvent 'HigherTarget
    TyLowerTarget    ∷ TypedTargetEvent 'LowerTarget
    TyNextTarget     ∷ TypedTargetEvent 'NextTarget
    TyPreviousTarget ∷ TypedTargetEvent 'PreviousTarget
    TyConfirmTarget  ∷ TypedTargetEvent 'ConfirmTarget


withTypedTargetEvent ∷ TargetEvent → (∀ e. TypedTargetEvent e → r) → r
withTypedTargetEvent (MoveReticule v) f = f (TyMoveReticule v)
withTypedTargetEvent HigherTarget     f = f TyHigherTarget
withTypedTargetEvent LowerTarget      f = f TyLowerTarget
withTypedTargetEvent NextTarget       f = f TyNextTarget
withTypedTargetEvent PreviousTarget   f = f TyPreviousTarget
withTypedTargetEvent ConfirmTarget    f = f TyConfirmTarget

--------------------------------------------------------------------------------

newtype ChoiceEvent = ChoiceCharacter Char


nextChoiceEvent ∷ String → C.Curses (Either BackEvent ChoiceEvent)
nextChoiceEvent ac = repeatUntilEvent charEvent
    where
        charEvent (C.EventCharacter c)
            | c == 'q'    = Just (Left Back)
            | c `elem` ac = Just (Right $ ChoiceCharacter c) 
            | otherwise   = Nothing
        charEvent _ = Nothing


repeatUntilEvent ∷ (C.Event → Maybe a) → C.Curses a
repeatUntilEvent f =
    C.defaultWindow >>= fmap f . event >>= \case
        (Just e) → pure e
        _        → repeatUntilEvent f
    where
        event w = fromJust <$> C.getEvent w Nothing

