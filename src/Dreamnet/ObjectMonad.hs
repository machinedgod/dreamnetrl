{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectAPI(..)
, runObjectMonadWorld
)
where


import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2)

import Dreamnet.ObjectProperties
import Dreamnet.GameState (GameState(Normal))
import Dreamnet.World     (WorldReadAPI(castVisibilityRay), WorldAPI(moveObject))

--------------------------------------------------------------------------------

class ObjectAPI o where
    position         ∷ o (V2 Int)
    move             ∷ V2 Int → o ()
    requestGameState ∷ GameState → o ()
    setCollidable    ∷ Bool → o () -- Creates a state, creates and object. NO!
    setSeeThrough    ∷ Bool → o ()
    canSee           ∷ V2 Int → o Bool
    -- Keep adding primitives until you can describe all Map Objects as programs

--------------------------------------------------------------------------------

data ObjectF a = Move (V2 Int) a
               | Position (V2 Int → a)
               | RequestGameState GameState a
               | SetCollidable Bool a
               | SetSeeThrough Bool a
               | CanSee (V2 Int) (Bool → a)
               deriving(Functor) -- TODO Derive binary can't work with functions



instance ObjectAPI (Free ObjectF) where
    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    requestGameState gs =  Free $ RequestGameState gs (Pure ())

    setCollidable c = Free $ SetCollidable c (Pure ())

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (IsSeeThrough a, Monad w, WorldAPI a b c w) ⇒ V2 Int → a → Free ObjectF d → w (d, GameState)
runObjectMonadWorld = runWithGameState Normal False True
    where
        runWithGameState ∷ (IsSeeThrough a, Monad w, WorldAPI a b c w) ⇒ GameState → Bool → Bool → V2 Int → a → Free ObjectF d → w (d, GameState)
        runWithGameState gs cl st cv o (Free (Move v n)) = do
            moveObject cv o v
            runWithGameState gs cl st v o n

        runWithGameState gs cl st cv o (Free (Position fv)) = do
            runWithGameState gs cl st cv o (fv cv)

        runWithGameState _ cl st cv o (Free (RequestGameState gs n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs _ st cv o (Free (SetCollidable cl n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs cl _ cv o (Free (SetSeeThrough st n)) = do
            runWithGameState gs cl st cv o n

        runWithGameState gs cl st cv o (Free (CanSee v fs)) = do
            seesV ← and . fmap snd <$> castVisibilityRay cv v
            runWithGameState gs cl st cv o (fs seesV)

        runWithGameState gs _ _ _ _ (Pure x) = pure (x, gs)

--------------------------------------------------------------------------------

-- | Detects foes
--camera ∷ Free ObjectF Bool
--camera = position >>= canSee


