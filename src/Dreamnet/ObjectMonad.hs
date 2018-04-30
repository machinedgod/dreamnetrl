{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectF
, runObjectMonadWorld
)
where


import Control.Lens       (view, (.~))
import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2(V2))

import Dreamnet.ComputerModel
import Dreamnet.Character
import Dreamnet.ConversationMonad
import Dreamnet.ScrollData
import Dreamnet.World

import Design.DesignAPI

--------------------------------------------------------------------------------

-- TODO this object monad really doesn't have to exist. Everything could be
--      implemented simply through WorldAPI.
data ObjectF a = GetDesignData (DesignData → a)
               | Move (V2 Int) a
               | Position (V2 Int → a)
               | ShowInfoWindow String a
               | ShowComputerWindow ComputerData a
               | StartConversation DreamnetCharacter a
               | Passable (Bool → a)
               | SetPassable Bool a
               | SeeThrough (Bool → a)
               | SetSeeThrough Bool a
               | CanSee (V2 Int) (Bool → a)
               | ChangeSymbol Symbol a
               | ChangeMat String a
               | Message String a
               | Put States a
               | Get (States → a)
               | ScanRange Word (Object States → Bool) ([(V2 Int, Object States)] → a)
               deriving(Functor) -- TODO Derive binary can't work with functions


instance ObjectAPI (Free ObjectF) where
    designData = Free $ GetDesignData Pure

    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    showInfoWindow s = Free $ ShowInfoWindow s (Pure ())

    showComputerWindow cd = Free $ ShowComputerWindow cd (Pure ())

    startConversation ch = Free $ StartConversation ch (Pure ())

    passable = Free $ Passable Pure

    setPassable c = Free $ SetPassable c (Pure ())

    seeThrough = Free $ SeeThrough Pure

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

    changeSymbol s = Free $ ChangeSymbol s (Pure ())

    changeMat s = Free $ ChangeMat s (Pure ())

    message m = Free $ Message m (Pure ())

    put v = Free $ Put v (Pure ())

    get = Free $ Get Pure

    scanRange r f = Free $ ScanRange r f Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI States v w) ⇒ DesignData → (Integer, Integer) → Free ObjectF a → V2 Int → Object States → w (a, GameState)
runObjectMonadWorld dd ss op v o = runWithGameState dd Normal ss (v, o) op


runWithGameState ∷ (Monad w, WorldAPI States v w) ⇒ DesignData → GameState → (Integer, Integer) → (V2 Int, Object States) → Free ObjectF a → w (a, GameState)
runWithGameState dd gs ss (cv, o) (Free (GetDesignData fn)) = do
    runWithGameState dd gs ss (cv, o) (fn dd)

runWithGameState dd gs ss (cv, o) (Free (Move v n)) = do
    moveObject cv o v
    runWithGameState dd gs ss (v, o) n

runWithGameState dd gs ss (cv, o) (Free (Position fv)) = do
    runWithGameState dd gs ss (cv, o) (fv cv)

runWithGameState dd _ ss (cv, o) (Free (ShowInfoWindow txt n)) = do
    runWithGameState dd (Examination (newScrollData (V2 1 1) (V2 60 30) Nothing txt)) ss (cv, o) n

runWithGameState dd _ ss (cv, o) (Free (ShowComputerWindow cd n)) = do
    runWithGameState dd (ComputerOperation cv 1 cd) ss (cv, o) n -- TODO use *ACTUAL* IX

runWithGameState dd _ ss (cv, o) (Free (StartConversation ch n)) = do
    let cnodes = runConversationF_temp ["Carla", "Whoeverelse"] (view ch_conversation ch)
    runWithGameState dd (createConversationState ss cnodes) ss (cv, o) n

runWithGameState dd gs ss (cv, o) (Free (Passable fn)) = do
    runWithGameState dd gs ss (cv, o) (fn $ view o_passable o)

runWithGameState dd gs ss (cv, o) (Free (SetPassable cl n)) = do
    let no = o_passable .~ cl $ o
    replaceObject cv o no
    runWithGameState dd gs ss (cv, no) n

runWithGameState dd gs ss (cv, o) (Free (SeeThrough fn)) = do
    runWithGameState dd gs ss (cv, o) (fn $ view o_seeThrough o)

runWithGameState dd gs ss (cv, o) (Free (SetSeeThrough st n)) = do
    let no = o_seeThrough .~ st $ o
    replaceObject cv o no
    runWithGameState dd gs ss (cv, no) n

runWithGameState dd gs ss (cv, o) (Free (CanSee v fs)) = do
    seesV ← and . fmap snd <$> castVisibilityRay cv v
    runWithGameState dd gs ss (cv, o) (fs seesV)

runWithGameState dd gs ss (cv, o) (Free (ChangeSymbol c n)) = do
    let no = o_symbol .~ c $ o
    replaceObject cv o no
    runWithGameState dd gs ss (cv, no) n

runWithGameState dd gs ss (cv, o) (Free (ChangeMat m n)) = do
    let no = o_material .~ m $ o
    replaceObject cv o no
    runWithGameState dd gs ss (cv, no) n

runWithGameState dd gs ss (cv, o) (Free (Message m n)) = do
    setStatus m
    runWithGameState dd gs ss (cv, o) n

runWithGameState dd gs ss (cv, o) (Free (Put v n)) = do
    let no = o_state .~ v $ o
    replaceObject cv o no
    runWithGameState dd gs ss (cv, no) n

runWithGameState dd gs ss (cv, o) (Free (Get fn)) = do
    runWithGameState dd gs ss (cv, o) (fn . view o_state $ o)

runWithGameState dd gs ss (cv, o) (Free (ScanRange r f fn)) = do
    points ← interestingObjects cv r f
    values ← fmap (foldr onlyJust []) $ traverse (fmap lastValue . cellAt) points
    runWithGameState dd gs ss (cv, o) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l

runWithGameState _ gs _ _ (Pure x) =
    pure (x, gs)

