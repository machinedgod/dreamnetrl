{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectAPI(..)
, ObjectProgram
, runObjectMonadWorld
)
where


import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2)
import GHC.Generics       (Generic)
--import Data.Binary        (Binary)

import Dreamnet.GameState (GameState(Normal))
import Dreamnet.World     (WorldAPI(moveObject))

--------------------------------------------------------------------------------

class ObjectAPI o where
    move             ∷ V2 Int → o ()
    getPosition      ∷ o (V2 Int)
    requestGameState ∷ GameState → o ()
    setCollidable    ∷ Bool → o () -- Creates a state, creates and object. NO!
    setSeeThrough    ∷ Bool → o ()
    -- Keep adding primitives until you can describe all MapObjects as programs

--------------------------------------------------------------------------------

data ObjectF a = Move (V2 Int) a
               | GetPosition (V2 Int → a)
               | RequestGameState GameState a
               | SetCollidable Bool a
               | SetSeeThrough Bool a
               deriving(Functor, Generic) -- Derive binary can't work with functions


type ObjectProgram = Free ObjectF ()

instance ObjectAPI (Free ObjectF) where
    move v = Free $ Move v (Pure ())

    getPosition = Free $ GetPosition $ \v → Pure v

    requestGameState gs =  Free $ RequestGameState gs (Pure ())

    setCollidable c = Free $ SetCollidable c (Pure ())

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI a b c w) ⇒ V2 Int → a → Free ObjectF d → w (d, GameState)
runObjectMonadWorld = runWithGameState Normal False True
    where
        runWithGameState ∷ (Monad w, WorldAPI a b c w) ⇒ GameState → Bool → Bool → V2 Int → a → Free ObjectF d → w (d, GameState)
        runWithGameState gs cl st cv o (Free (Move v n)) = do
            moveObject cv o v
            runWithGameState gs cl st v o n

        runWithGameState gs cl st cv o (Free (GetPosition fv)) = do
            runWithGameState gs cl st cv o (fv cv)

        runWithGameState _ cl st cv o (Free (RequestGameState gs n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs _ st cv o (Free (SetCollidable cl n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs cl _ cv o (Free (SetSeeThrough st n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs _ _ _ _ (Pure x) = pure (x, gs)

