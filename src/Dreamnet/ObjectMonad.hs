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
import Dreamnet.ScrollData
import Dreamnet.World
import Dreamnet.WorldMap
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
               | ChangeChar Char a
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

    changeChar c = Free $ ChangeChar c (Pure ())

    changeMat s = Free $ ChangeMat s (Pure ())

    message m = Free $ Message m (Pure ())

    put v = Free $ Put v (Pure ())

    get = Free $ Get Pure

    scanRange r f = Free $ ScanRange r f Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI States v w) ⇒ DesignData → Free ObjectF a → V2 Int → Object States → w (a, GameState)
runObjectMonadWorld dd op v o = runWithGameState dd Normal (v, o) op


runWithGameState ∷ (Monad w, WorldAPI States v w) ⇒ DesignData → GameState → (V2 Int, Object States) → Free ObjectF a → w (a, GameState)
runWithGameState dd gs (cv, o) (Free (GetDesignData fn)) = do
    runWithGameState dd gs (cv, o) (fn dd)

runWithGameState dd gs (cv, o) (Free (Move v n)) = do
    moveObject cv o v
    runWithGameState dd gs (v, o) n

runWithGameState dd gs (cv, o) (Free (Position fv)) = do
    runWithGameState dd gs (cv, o) (fv cv)

runWithGameState dd _ (cv, o) (Free (ShowInfoWindow txt n)) = do
    runWithGameState dd (Examination (newScrollData (V2 1 1) (V2 60 30) Nothing txt)) (cv, o) n

runWithGameState dd _ (cv, o) (Free (ShowComputerWindow cd n)) = do
    runWithGameState dd (ComputerOperation cd) (cv, o) n

runWithGameState dd _ (cv, o) (Free (StartConversation ch n)) = do
    runWithGameState dd (createConversationState (V2 1 1) (V2 60 30) ch (view ch_conversation ch)) (cv, o) n

runWithGameState dd gs (cv, o) (Free (Passable fn)) = do
    runWithGameState dd gs (cv, o) (fn $ view o_passable o)

runWithGameState dd gs (cv, o) (Free (SetPassable cl n)) = do
    let no = o_passable .~ cl $ o
    changeObject_ cv o no
    runWithGameState dd gs (cv, no) n

runWithGameState dd gs (cv, o) (Free (SeeThrough fn)) = do
    runWithGameState dd gs (cv, o) (fn $ view o_seeThrough o)

runWithGameState dd gs (cv, o) (Free (SetSeeThrough st n)) = do
    let no = o_seeThrough .~ st $ o
    changeObject_ cv o no
    runWithGameState dd gs (cv, no) n

runWithGameState dd gs (cv, o) (Free (CanSee v fs)) = do
    seesV ← and . fmap snd <$> castVisibilityRay cv v
    runWithGameState dd gs (cv, o) (fs seesV)

runWithGameState dd gs (cv, o) (Free (ChangeChar c n)) = do
    let no = o_symbol .~ c $ o
    changeObject_ cv o no
    runWithGameState dd gs (cv, no) n

runWithGameState dd gs (cv, o) (Free (ChangeMat m n)) = do
    let no = o_material .~ m $ o
    changeObject_ cv o no
    runWithGameState dd gs (cv, no) n

runWithGameState dd gs (cv, o) (Free (Message m n)) = do
    setStatus m
    runWithGameState dd gs (cv, o) n

runWithGameState dd gs (cv, o) (Free (Put v n)) = do
    let no = o_state .~ v $ o
    changeObject_ cv o no
    runWithGameState dd gs (cv, no) n

runWithGameState dd gs (cv, o) (Free (Get fn)) = do
    runWithGameState dd gs (cv, o) (fn . view o_state $ o)

runWithGameState dd gs (cv, o) (Free (ScanRange r f fn)) = do
    m ← worldMap
    let points = interestingObjects cv r f m
    let v      = zip points (last . (`valuesAt` m) <$> points)
    runWithGameState dd gs (cv, o) (fn v)

runWithGameState _ gs _ (Pure x) =
    pure (x, gs)

