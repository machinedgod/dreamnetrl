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

, TacticalEvent(..), nextTacticalEvent
) where


import Data.Maybe (fromJust)

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace, KeyBackTab))

import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration

--------------------------------------------------------------------------------

data BackEvent = Back

--------------------------------------------------------------------------------

data WorldEvent = Move       Direction
                | MoveCamera Direction
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
                | SetStance   Iteration
                | InventorySheet
                | CharacterSheet
                | SwitchToTactical
                | SwitchToHud


-- TODO for distinguishing between world and held, and maybe some other stuff,
--      should I use 'stateful' keys like with vim? Eg. 'e' is examine,
--      but '<leader>e' is examine held item?
nextWorldEvent ∷ C.Curses (Either BackEvent WorldEvent)
nextWorldEvent = repeatUntilEvent worldEvent
    where
        worldEvent (C.EventCharacter 'h')  = Just $ Right $ Move West
        worldEvent (C.EventCharacter 'j')  = Just $ Right $ Move South
        worldEvent (C.EventCharacter 'k')  = Just $ Right $ Move North
        worldEvent (C.EventCharacter 'l')  = Just $ Right $ Move East
        worldEvent (C.EventCharacter 'y')  = Just $ Right $ Move NorthWest
        worldEvent (C.EventCharacter 'u')  = Just $ Right $ Move NorthEast
        worldEvent (C.EventCharacter 'b')  = Just $ Right $ Move SouthWest
        worldEvent (C.EventCharacter 'n')  = Just $ Right $ Move SouthEast
        worldEvent (C.EventCharacter 'H')  = Just $ Right $ MoveCamera West
        worldEvent (C.EventCharacter 'J')  = Just $ Right $ MoveCamera South
        worldEvent (C.EventCharacter 'K')  = Just $ Right $ MoveCamera North
        worldEvent (C.EventCharacter 'L')  = Just $ Right $ MoveCamera East
        worldEvent (C.EventCharacter 'Y')  = Just $ Right $ MoveCamera NorthWest
        worldEvent (C.EventCharacter 'U')  = Just $ Right $ MoveCamera NorthEast
        worldEvent (C.EventCharacter 'B')  = Just $ Right $ MoveCamera SouthWest
        worldEvent (C.EventCharacter 'N')  = Just $ Right $ MoveCamera SouthEast
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
        worldEvent (C.EventCharacter '[')  = Just $ Right $ SetStance Next
        worldEvent (C.EventCharacter ']')  = Just $ Right $ SetStance Previous
        worldEvent (C.EventCharacter 'i')  = Just $ Right   InventorySheet
        worldEvent (C.EventCharacter 'c')  = Just $ Right   CharacterSheet
        worldEvent (C.EventCharacter 'C')  = Just $ Right   SwitchToTactical
        worldEvent (C.EventCharacter '\t') = Just $ Right   SwitchToHud
        worldEvent (C.EventCharacter 'q')  = Just $ Left    Back
        worldEvent _                       = Nothing


-- TODO singletonize?
--      I mean, use Data.Singletons instead of doing this manually
data TypedWorldEvent ∷ WorldEvent → * where
    TyMove             ∷ Sing (d ∷ Direction) → TypedWorldEvent ('Move d)
    TyMoveCamera       ∷ Sing (d ∷ Direction) → TypedWorldEvent ('MoveCamera d)
    TyExamine          ∷                        TypedWorldEvent 'Examine
    TyOperate          ∷                        TypedWorldEvent 'Operate
    TyExamineHeld      ∷                        TypedWorldEvent 'ExamineHeld
    TyOperateHeld      ∷                        TypedWorldEvent 'OperateHeld
    TyOperateHeldOn    ∷                        TypedWorldEvent 'OperateHeldOn
    TyTalk             ∷                        TypedWorldEvent 'Talk
    TyGet              ∷                        TypedWorldEvent 'Get
    TyWear             ∷                        TypedWorldEvent 'Wear
    TyStoreIn          ∷                        TypedWorldEvent 'StoreIn
    TyPullFrom         ∷                        TypedWorldEvent 'PullFrom
    TyWait             ∷                        TypedWorldEvent 'Wait
    TySetStance        ∷ Sing (i ∷ Iteration) → TypedWorldEvent ('SetStance i)
    TyInventorySheet   ∷                        TypedWorldEvent 'InventorySheet
    TyCharacterSheet   ∷                        TypedWorldEvent 'CharacterSheet
    TySwitchToTactical ∷                        TypedWorldEvent 'SwitchToTactical
    TySwitchToHud      ∷                        TypedWorldEvent 'SwitchToHud


withTypedWorldEvent ∷ WorldEvent → (∀ e. TypedWorldEvent e → r) → r
withTypedWorldEvent (Move West)            f = f (TyMove SWest)     
withTypedWorldEvent (Move South)           f = f (TyMove SSouth)    
withTypedWorldEvent (Move North)           f = f (TyMove SNorth)    
withTypedWorldEvent (Move East)            f = f (TyMove SEast)     
withTypedWorldEvent (Move NorthWest)       f = f (TyMove SNorthWest)
withTypedWorldEvent (Move NorthEast)       f = f (TyMove SNorthEast)
withTypedWorldEvent (Move SouthWest)       f = f (TyMove SSouthWest)
withTypedWorldEvent (Move SouthEast)       f = f (TyMove SSouthEast)
withTypedWorldEvent (MoveCamera West)      f = f (TyMoveCamera SWest)     
withTypedWorldEvent (MoveCamera South)     f = f (TyMoveCamera SSouth)    
withTypedWorldEvent (MoveCamera North)     f = f (TyMoveCamera SNorth)    
withTypedWorldEvent (MoveCamera East)      f = f (TyMoveCamera SEast)     
withTypedWorldEvent (MoveCamera NorthWest) f = f (TyMoveCamera SNorthWest)
withTypedWorldEvent (MoveCamera NorthEast) f = f (TyMoveCamera SNorthEast)
withTypedWorldEvent (MoveCamera SouthWest) f = f (TyMoveCamera SSouthWest)
withTypedWorldEvent (MoveCamera SouthEast) f = f (TyMoveCamera SSouthEast)
withTypedWorldEvent Examine                f = f TyExamine
withTypedWorldEvent Operate                f = f TyOperate
withTypedWorldEvent ExamineHeld            f = f TyExamineHeld
withTypedWorldEvent OperateHeld            f = f TyOperateHeld
withTypedWorldEvent OperateHeldOn          f = f TyOperateHeldOn
withTypedWorldEvent Talk                   f = f TyTalk
withTypedWorldEvent Get                    f = f TyGet
withTypedWorldEvent Wear                   f = f TyWear
withTypedWorldEvent StoreIn                f = f TyStoreIn
withTypedWorldEvent PullFrom               f = f TyPullFrom
withTypedWorldEvent Wait                   f = f TyWait
withTypedWorldEvent (SetStance Next)       f = f (TySetStance SNext)
withTypedWorldEvent (SetStance Previous)   f = f (TySetStance SPrevious)
withTypedWorldEvent InventorySheet         f = f TyInventorySheet
withTypedWorldEvent CharacterSheet         f = f TyCharacterSheet
withTypedWorldEvent SwitchToTactical       f = f TySwitchToTactical
withTypedWorldEvent SwitchToHud            f = f TySwitchToHud

--------------------------------------------------------------------------------

data UIEvent = MoveCursor Direction
             | Tab        Iteration
             | SelectChoice


nextUiEvent ∷ C.Curses (Either BackEvent UIEvent)
nextUiEvent = repeatUntilEvent uiEvent
    where
        uiEvent (C.EventCharacter '\t')          = Just $ Right $ Tab Next
        uiEvent (C.EventSpecialKey C.KeyBackTab) = Just $ Right $ Tab Previous
        uiEvent (C.EventCharacter 'h')           = Just $ Right $ MoveCursor West
        uiEvent (C.EventCharacter 'j')           = Just $ Right $ MoveCursor South
        uiEvent (C.EventCharacter 'k')           = Just $ Right $ MoveCursor North
        uiEvent (C.EventCharacter 'l')           = Just $ Right $ MoveCursor East
        uiEvent (C.EventCharacter '\n')          = Just $ Right   SelectChoice
        uiEvent (C.EventCharacter 'q')           = Just $ Left    Back
        uiEvent _                                = Nothing


data TypedUiEvent ∷ UIEvent → * where
    TyMoveCursor   ∷ Sing (d ∷ Direction) → TypedUiEvent ('MoveCursor d)
    TyTab          ∷ Sing (i ∷ Iteration) → TypedUiEvent ('Tab i)
    TySelectChoice ∷                        TypedUiEvent 'SelectChoice


withTypedUIEvent ∷ UIEvent → (∀ e. TypedUiEvent e → r) → r
withTypedUIEvent (MoveCursor West)      f = f (TyMoveCursor SWest)     
withTypedUIEvent (MoveCursor South)     f = f (TyMoveCursor SSouth)    
withTypedUIEvent (MoveCursor North)     f = f (TyMoveCursor SNorth)    
withTypedUIEvent (MoveCursor East)      f = f (TyMoveCursor SEast)     
withTypedUIEvent (MoveCursor NorthWest) f = f (TyMoveCursor SNorthWest)
withTypedUIEvent (MoveCursor NorthEast) f = f (TyMoveCursor SNorthEast)
withTypedUIEvent (MoveCursor SouthWest) f = f (TyMoveCursor SSouthWest)
withTypedUIEvent (MoveCursor SouthEast) f = f (TyMoveCursor SSouthEast)
withTypedUIEvent (Tab Next)             f = f (TyTab SNext)
withTypedUIEvent (Tab Previous)         f = f (TyTab SPrevious)
withTypedUIEvent SelectChoice           f = f TySelectChoice

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

data TargetEvent = MoveReticule Direction
                 | MoveTarget   Iteration
                 | SmartTarget  Iteration
                 | ConfirmTarget


nextTargetSelectionEvent ∷ C.Curses (Either BackEvent TargetEvent)
nextTargetSelectionEvent = repeatUntilEvent targetEvent
    where
        targetEvent (C.EventCharacter 'h')           = Just $ Right $ MoveReticule West
        targetEvent (C.EventCharacter 'j')           = Just $ Right $ MoveReticule South
        targetEvent (C.EventCharacter 'k')           = Just $ Right $ MoveReticule North
        targetEvent (C.EventCharacter 'l')           = Just $ Right $ MoveReticule East
        targetEvent (C.EventCharacter 'y')           = Just $ Right $ MoveReticule NorthWest
        targetEvent (C.EventCharacter 'u')           = Just $ Right $ MoveReticule NorthEast
        targetEvent (C.EventCharacter 'b')           = Just $ Right $ MoveReticule SouthWest
        targetEvent (C.EventCharacter 'n')           = Just $ Right $ MoveReticule SouthEast
        targetEvent (C.EventCharacter 'H')           = Just $ Right $ MoveTarget Next
        targetEvent (C.EventCharacter 'L')           = Just $ Right $ MoveTarget Previous
        targetEvent (C.EventCharacter '\t')          = Just $ Right $ SmartTarget Next
        targetEvent (C.EventSpecialKey C.KeyBackTab) = Just $ Right $ SmartTarget Previous
        targetEvent (C.EventCharacter '\n')          = Just $ Right   ConfirmTarget
        targetEvent (C.EventCharacter 'q')           = Just $ Left    Back
        targetEvent _                                = Nothing


data TypedTargetEvent ∷ TargetEvent → * where
    TyMoveReticule   ∷ Sing (d ∷ Direction) → TypedTargetEvent ('MoveReticule d)
    TyMoveTarget     ∷ Sing (i ∷ Iteration) → TypedTargetEvent ('MoveTarget i)
    TySmartTarget    ∷ Sing (i ∷ Iteration) → TypedTargetEvent ('SmartTarget i)
    TyConfirmTarget  ∷               TypedTargetEvent 'ConfirmTarget


withTypedTargetEvent ∷ TargetEvent → (∀ e. TypedTargetEvent e → r) → r
withTypedTargetEvent (MoveReticule West)      f = f (TyMoveReticule SWest)
withTypedTargetEvent (MoveReticule South)     f = f (TyMoveReticule SSouth)
withTypedTargetEvent (MoveReticule North)     f = f (TyMoveReticule SNorth)
withTypedTargetEvent (MoveReticule East)      f = f (TyMoveReticule SEast)
withTypedTargetEvent (MoveReticule NorthWest) f = f (TyMoveReticule SNorthWest)
withTypedTargetEvent (MoveReticule NorthEast) f = f (TyMoveReticule SNorthEast)
withTypedTargetEvent (MoveReticule SouthWest) f = f (TyMoveReticule SSouthWest)
withTypedTargetEvent (MoveReticule SouthEast) f = f (TyMoveReticule SSouthEast)
withTypedTargetEvent (MoveTarget Next)        f = f (TyMoveTarget SNext)
withTypedTargetEvent (MoveTarget Previous)    f = f (TyMoveTarget SPrevious)
withTypedTargetEvent (SmartTarget Next)       f = f (TySmartTarget SNext)
withTypedTargetEvent (SmartTarget Previous)   f = f (TySmartTarget SPrevious)
withTypedTargetEvent ConfirmTarget            f = f TyConfirmTarget

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

--------------------------------------------------------------------------------

data TacticalEvent = MoveSelector         Direction
                   | SmartMoveSelector    Iteration
                   | ToggleSelectedDirect Int
                   | IssueOrder
                   | CancelPlan
                   | ExecutePlan


nextTacticalEvent ∷ C.Curses TacticalEvent
nextTacticalEvent = repeatUntilEvent tacticalEvent
    where
        tacticalEvent (C.EventCharacter 'h')           = Just $ MoveSelector West
        tacticalEvent (C.EventCharacter 'j')           = Just $ MoveSelector South
        tacticalEvent (C.EventCharacter 'k')           = Just $ MoveSelector North
        tacticalEvent (C.EventCharacter 'l')           = Just $ MoveSelector East
        tacticalEvent (C.EventCharacter 'y')           = Just $ MoveSelector NorthWest
        tacticalEvent (C.EventCharacter 'u')           = Just $ MoveSelector NorthEast
        tacticalEvent (C.EventCharacter 'b')           = Just $ MoveSelector SouthWest
        tacticalEvent (C.EventCharacter 'n')           = Just $ MoveSelector SouthEast
        tacticalEvent (C.EventCharacter '\t')          = Just $ SmartMoveSelector Next
        tacticalEvent (C.EventSpecialKey C.KeyBackTab) = Just $ SmartMoveSelector Previous
        tacticalEvent (C.EventCharacter ' ')           = Just   IssueOrder
        tacticalEvent (C.EventCharacter '\ESC')        = Just   CancelPlan
        tacticalEvent (C.EventCharacter '\n')          = Just   ExecutePlan

        tacticalEvent (C.EventCharacter 'a')           = Just $ ToggleSelectedDirect 1
        tacticalEvent (C.EventCharacter 'c')           = Just $ ToggleSelectedDirect 2
        tacticalEvent (C.EventCharacter 'd')           = Just $ ToggleSelectedDirect 3
        tacticalEvent (C.EventCharacter 'e')           = Just $ ToggleSelectedDirect 4
        tacticalEvent (C.EventCharacter 'f')           = Just $ ToggleSelectedDirect 5
        tacticalEvent (C.EventCharacter 'g')           = Just $ ToggleSelectedDirect 6
        tacticalEvent _                                = Nothing

--------------------------------------------------------------------------------

repeatUntilEvent ∷ (C.Event → Maybe a) → C.Curses a
repeatUntilEvent f =
    C.defaultWindow >>= fmap f . event >>= \case
        (Just e) → pure e
        _        → repeatUntilEvent f
    where
        event w = fromJust <$> C.getEvent w Nothing

