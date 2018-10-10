{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Dreamnet.Engine.Input
--( BackEvent(..), WorldEvent(..), nextWorldEvent, TypedWorldEvent(..), withTypedWorldEvent
--
--, UIEvent(..), nextUiEvent, TypedUiEvent(..), withTypedUIEvent
--
--, TargetEvent(..), nextTargetSelectionEvent, TypedTargetEvent(..), withTypedTargetEvent
--
--, ChoiceEvent(..), nextChoiceEvent
--
--, PassThrough(..), nextPassThrough
--
--, TacticalEvent(..), nextTacticalEvent
--)
where


import Data.Maybe         (fromJust)
import Data.Singletons.TH (genSingletons)

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

--------------------------------------------------------------------------------

-- TODO TacticalEvent has problems with Int
$(genSingletons [ ''WorldEvent, ''UIEvent, ''TargetEvent ])
