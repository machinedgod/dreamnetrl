{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectF
, runObjectMonadWorld
) where


import Control.Lens        (view, (.~))
import Control.Monad.Free  (Free(Free, Pure))
import Linear              (V2)

import Dreamnet.Engine.World

import Design.DesignAPI

--------------------------------------------------------------------------------

-- TODO this object monad really doesn't have to exist. Everything could be
--      implemented simply through WorldAPI.
data ObjectF s a = Position (V2 Int → a)
                 | Move (V2 Int) a
                 | Passable (Bool → a)
                 | SetPassable Bool a
                 | SeeThrough (Bool → a)
                 | SetSeeThrough Bool a
                 | CanSee (V2 Int) (Bool → a)
                 | ChangeSymbol Symbol a
                 | ChangeMat String a
                 | Message String a
                 | ScanRange Word (Object s → Bool) ([(V2 Int, Object s)] → a)
                 -- | Interact (InteractionType s) (V2 Int) Int a
                 deriving(Functor) -- TODO Derive binary can't work with functions


instance ObjectAPI s (Free (ObjectF s)) where
    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    passable = Free $ Passable Pure

    setPassable c = Free $ SetPassable c (Pure ())

    seeThrough = Free $ SeeThrough Pure

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

    changeSymbol s = Free $ ChangeSymbol s (Pure ())

    changeMat s = Free $ ChangeMat s (Pure ())

    message m = Free $ Message m (Pure ())

    scanRange r f = Free $ ScanRange r f Pure

    --interact i v ix = Free $ Interact i v ix (Pure ())

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI States v w) ⇒ (V2 Int, Object States) → Free (ObjectF States) GameState → w GameState
runObjectMonadWorld (cv, o) (Free (Position fv)) = do
    runObjectMonadWorld (cv, o) (fv cv)

runObjectMonadWorld (cv, o) (Free (Move v n)) = do
    moveObject cv o v
    runObjectMonadWorld (v, o) n

runObjectMonadWorld (cv, o) (Free (Passable fn)) = do
    runObjectMonadWorld (cv, o) (fn $ view o_passable o)

runObjectMonadWorld (cv, o) (Free (SetPassable cl n)) = do
    let no = o_passable .~ cl $ o
    replaceObject cv o no
    runObjectMonadWorld (cv, no) n

runObjectMonadWorld (cv, o) (Free (SeeThrough fn)) = do
    runObjectMonadWorld (cv, o) (fn $ view o_seeThrough o)

runObjectMonadWorld (cv, o) (Free (SetSeeThrough st n)) = do
    let no = o_seeThrough .~ st $ o
    replaceObject cv o no
    runObjectMonadWorld (cv, no) n

runObjectMonadWorld (cv, o) (Free (CanSee v fs)) = do
    seesV ← and . fmap snd <$> castVisibilityRay cv v
    runObjectMonadWorld (cv, o) (fs seesV)

runObjectMonadWorld (cv, o) (Free (ChangeSymbol c n)) = do
    let no = o_symbol .~ c $ o
    replaceObject cv o no
    runObjectMonadWorld (cv, no) n

runObjectMonadWorld (cv, o) (Free (ChangeMat m n)) = do
    let no = o_material .~ m $ o
    replaceObject cv o no
    runObjectMonadWorld (cv, no) n

runObjectMonadWorld (cv, o) (Free (Message m n)) = do
    setStatus m
    runObjectMonadWorld (cv, o) n

runObjectMonadWorld (cv, o) (Free (ScanRange r f fn)) = do
    points ← interestingObjects cv r f
    values ← fmap (foldr onlyJust []) $ traverse (fmap lastValue . cellAt) points
    runObjectMonadWorld (cv, o) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l

--runObjectMonadWorld (cv, o) (Free (Interact _ _ _ n)) = do
--    runObjectMonadWorld (cv, o) n

runObjectMonadWorld _ (Pure x) =
    pure x

