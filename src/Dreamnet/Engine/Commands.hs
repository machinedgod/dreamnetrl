{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Engine.Commands
( Order(..)
, Current(..)
, Desired(..)
, StepActionType(..)
, StepAction(..)

, moveCommand
)
where


import Prelude hiding (succ, pred, head)
import Safe           (headDef)
import Linear         (V2)
import Data.Maybe     (fromJust)

import Dreamnet.Engine.Character
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Utils


data Order v where
    MoveTo      ∷ V2 Int → Order (V2 Int)
    AttackPoint ∷ V2 Int → Order (V2 Int)
    SetStance   ∷ Stance → Order Stance
deriving instance Show (Order v)

--------------------------------------------------------------------------------

newtype Current a = Current a
                  deriving(Functor)
newtype Desired a = Desired a
                  deriving(Functor)

data StepActionType = StepMove
                    | StepOperate


data MovementGraphNode d = MovementGraphNode [MovementGraphNode d] d
                         deriving(Functor)

newtype StepAction = StepAction (StepActionType, Direction)

--newtype MoveCommand = MoveCommand { next ∷ MovementGraphNode (V2 Int) → Desired (V2 Int) → Current (V2 Int) → StepAction }


moveCommand ∷ Desired (V2 Int) → Current (V2 Int) → StepAction
--moveCommand ∷ MovementGraphNode (V2 Int) → Desired (V2 Int) → Current (V2 Int) → StepAction
moveCommand d c@(Current cv) =
    moveAc $ headDef (MovementGraphNode [] cv) $ astar (MovementGraphNode [] <$> d) (MovementGraphNode [] <$> c)
    where
        moveAc (MovementGraphNode _ v) = StepAction (StepMove, direct v)
        direct v = fromJust $ vecToDir (fmap fromIntegral v ∷ V2 Float)

-- Nuke the constraint when using graph, not positioning
astar ∷ Desired (MovementGraphNode (V2 Int)) → Current (MovementGraphNode (V2 Int)) → [MovementGraphNode (V2 Int)]
astar (Desired (MovementGraphNode _ dv)) (Current (MovementGraphNode _ cv)) = 
    MovementGraphNode [] <$> bla cv dv
