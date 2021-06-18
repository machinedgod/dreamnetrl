{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NegativeLiterals         #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnicodeSyntax            #-}


module Dreamnet.Engine.Input
( EventSource(nextEvent)

, WorldEvent(..), SWorldEvent(..)

, UIEvent(..), SUIEvent(..)

, TargetEvent(..), STargetEvent(..)

, ChoiceEvent(..)

, PassThrough(..)

, TacticalEvent(..)
)
where


import Data.Maybe         (fromJust)
import Data.Singletons.TH (genSingletons)

import qualified UI.NCurses as C (Curses, defaultWindow, getEvent,
                                  Event(EventCharacter, EventSpecialKey),
                                  Key(KeyBackspace, KeyBackTab))

import qualified Dreamnet.Game as G -- TODO UUUUUGH!!! Need to take it out!
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration

--------------------------------------------------------------------------------

class EventSource (gsi ∷ G.GameStateEnum) where
    -- TODO convert to data family, because then every state can have its own events.
    --      That might be good, or not?
    type Event     gsi ∷ *
    type ExtraData gsi ∷ *
    nextEvent ∷ G.GameState gsi → ExtraData gsi → C.Curses (Event gsi)

--------------------------------------------------------------------------------

data WorldEvent =
      BackToMainMenu
    | Move       Direction
    | MoveCamera Direction
    | Examine
    | DescribeEnvironment
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


data ChoiceEvent =
      ChoiceBack
    | ChoiceCharacter Char


data PassThrough =
      PassThroughBack
    | PassThrough Char


-- TODO for distinguishing between world and held, and maybe some other stuff,
--      should I use 'stateful' keys like with vim? Eg. 'e' is examine,
--      but '<leader>e' is examine held item?
instance EventSource 'G.Normal where
    type Event     'G.Normal = WorldEvent
    type ExtraData 'G.Normal = ()
    nextEvent _ _ = repeatUntilEvent worldEvent
        where
            worldEvent (C.EventCharacter 'h')  = Just $ Move West
            worldEvent (C.EventCharacter 'j')  = Just $ Move South
            worldEvent (C.EventCharacter 'k')  = Just $ Move North
            worldEvent (C.EventCharacter 'l')  = Just $ Move East
            worldEvent (C.EventCharacter 'y')  = Just $ Move NorthWest
            worldEvent (C.EventCharacter 'u')  = Just $ Move NorthEast
            worldEvent (C.EventCharacter 'b')  = Just $ Move SouthWest
            worldEvent (C.EventCharacter 'n')  = Just $ Move SouthEast
            worldEvent (C.EventCharacter 'H')  = Just $ MoveCamera West
            worldEvent (C.EventCharacter 'J')  = Just $ MoveCamera South
            worldEvent (C.EventCharacter 'K')  = Just $ MoveCamera North
            worldEvent (C.EventCharacter 'L')  = Just $ MoveCamera East
            worldEvent (C.EventCharacter 'Y')  = Just $ MoveCamera NorthWest
            worldEvent (C.EventCharacter 'U')  = Just $ MoveCamera NorthEast
            worldEvent (C.EventCharacter 'B')  = Just $ MoveCamera SouthWest
            worldEvent (C.EventCharacter 'N')  = Just $ MoveCamera SouthEast
            worldEvent (C.EventCharacter 'x')  = Just   Examine
            worldEvent (C.EventCharacter 'D')  = Just   DescribeEnvironment
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
            worldEvent (C.EventCharacter '[')  = Just $ SetStance Next
            worldEvent (C.EventCharacter ']')  = Just $ SetStance Previous
            worldEvent (C.EventCharacter 'i')  = Just   InventorySheet
            worldEvent (C.EventCharacter 'c')  = Just   CharacterSheet
            worldEvent (C.EventCharacter 'C')  = Just   SwitchToTactical
            worldEvent (C.EventCharacter '\t') = Just   SwitchToHud
            worldEvent (C.EventCharacter 'q')  = Just   BackToMainMenu
            worldEvent _                       = Nothing


instance EventSource 'G.Examination where
    type Event     'G.Examination = UIEvent
    type ExtraData 'G.Examination = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.Conversation where
    type Event     'G.Conversation = UIEvent
    type ExtraData 'G.Conversation = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.ComputerOperation where
    type Event     'G.ComputerOperation = PassThrough
    type ExtraData 'G.ComputerOperation = ()
    nextEvent _ _ = repeatUntilEvent passThroughEvent
        where
            passThroughEvent (C.EventSpecialKey C.KeyBackspace) = Just $ PassThrough '\b'
            passThroughEvent (C.EventCharacter c)
                | c == '\ESC' = Just   PassThroughBack
                | otherwise   = Just $ PassThrough c
            passThroughEvent _ = Nothing

instance EventSource 'G.HudTeam where
    type Event     'G.HudTeam = UIEvent
    type ExtraData 'G.HudTeam = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.HudMessages where
    type Event     'G.HudMessages = UIEvent
    type ExtraData 'G.HudMessages = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.HudWatch where
    type Event     'G.HudWatch = UIEvent
    type ExtraData 'G.HudWatch = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.InventoryUI where
    type Event     'G.InventoryUI = UIEvent
    type ExtraData 'G.InventoryUI = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.SkillsUI where
    type Event     'G.SkillsUI = UIEvent
    type ExtraData 'G.SkillsUI = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.EquipmentUI where
    type Event     'G.EquipmentUI = UIEvent
    type ExtraData 'G.EquipmentUI = ()
    nextEvent _ _ = nextUiEvent

instance EventSource 'G.TargetSelectionAdjactened where
    type Event     'G.TargetSelectionAdjactened = TargetEvent
    type ExtraData 'G.TargetSelectionAdjactened = ()
    nextEvent _ _ = nextTargetSelectionEvent

instance EventSource 'G.TargetSelectionDistant where
    type Event     'G.TargetSelectionDistant = TargetEvent
    type ExtraData 'G.TargetSelectionDistant = ()
    nextEvent _ _ = nextTargetSelectionEvent

instance EventSource 'G.ChoiceSelection where
    type Event     'G.ChoiceSelection = ChoiceEvent
    type ExtraData 'G.ChoiceSelection = String
    nextEvent _ ac = repeatUntilEvent charEvent
        where
            charEvent (C.EventCharacter c)
                | c == 'q'    = Just   ChoiceBack
                | c `elem` ac = Just $ ChoiceCharacter c
                | otherwise   = Nothing
            charEvent _ = Nothing

--------------------------------------------------------------------------------

data UIEvent =
      UiBack
    | MoveCursor Direction
    | Tab        Iteration
    | SelectChoice


nextUiEvent ∷ C.Curses UIEvent
nextUiEvent = repeatUntilEvent uiEvent
    where
        uiEvent (C.EventCharacter '\t')          = Just $ Tab Next
        uiEvent (C.EventSpecialKey C.KeyBackTab) = Just $ Tab Previous
        uiEvent (C.EventCharacter 'h')           = Just $ MoveCursor West
        uiEvent (C.EventCharacter 'j')           = Just $ MoveCursor South
        uiEvent (C.EventCharacter 'k')           = Just $ MoveCursor North
        uiEvent (C.EventCharacter 'l')           = Just $ MoveCursor East
        uiEvent (C.EventCharacter '\n')          = Just   SelectChoice
        uiEvent (C.EventCharacter 'q')           = Just   UiBack
        uiEvent _                                = Nothing

--------------------------------------------------------------------------------

data TargetEvent =
      TargetBack
    | MoveReticule Direction
    | MoveTarget   Iteration
    | SmartTarget  Iteration
    | ConfirmTarget


nextTargetSelectionEvent ∷ C.Curses TargetEvent
nextTargetSelectionEvent = repeatUntilEvent targetEvent
    where
        targetEvent (C.EventCharacter 'h')           = Just $ MoveReticule West
        targetEvent (C.EventCharacter 'j')           = Just $ MoveReticule South
        targetEvent (C.EventCharacter 'k')           = Just $ MoveReticule North
        targetEvent (C.EventCharacter 'l')           = Just $ MoveReticule East
        targetEvent (C.EventCharacter 'y')           = Just $ MoveReticule NorthWest
        targetEvent (C.EventCharacter 'u')           = Just $ MoveReticule NorthEast
        targetEvent (C.EventCharacter 'b')           = Just $ MoveReticule SouthWest
        targetEvent (C.EventCharacter 'n')           = Just $ MoveReticule SouthEast
        targetEvent (C.EventCharacter 'H')           = Just $ MoveTarget Next
        targetEvent (C.EventCharacter 'L')           = Just $ MoveTarget Previous
        targetEvent (C.EventCharacter '\t')          = Just $ SmartTarget Next
        targetEvent (C.EventSpecialKey C.KeyBackTab) = Just $ SmartTarget Previous
        targetEvent (C.EventCharacter '\n')          = Just   ConfirmTarget
        targetEvent (C.EventCharacter 'q')           = Just   TargetBack
        targetEvent _                                = Nothing

--------------------------------------------------------------------------------

data TeammateSelectionDirect
    = S1
    | S2
    | S3
    | S4
    | S5
    | S6


data TacticalEvent = MoveSelector         Direction
                   | SmartMoveSelector    Iteration
                   | ToggleSelectedDirect TeammateSelectionDirect
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

        tacticalEvent (C.EventCharacter 'a')           = Just $ ToggleSelectedDirect S1
        tacticalEvent (C.EventCharacter 'c')           = Just $ ToggleSelectedDirect S2
        tacticalEvent (C.EventCharacter 'd')           = Just $ ToggleSelectedDirect S3
        tacticalEvent (C.EventCharacter 'e')           = Just $ ToggleSelectedDirect S4
        tacticalEvent (C.EventCharacter 'f')           = Just $ ToggleSelectedDirect S5
        tacticalEvent (C.EventCharacter 'g')           = Just $ ToggleSelectedDirect S6
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

$(genSingletons [ ''WorldEvent
                , ''UIEvent
                , ''TargetEvent
                , ''TeammateSelectionDirect, ''TacticalEvent
                ])

