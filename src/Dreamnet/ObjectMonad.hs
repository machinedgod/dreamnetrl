{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectF
, runObjectMonadWorld

, ObjectM
, runObject
)
where


import Control.Lens        (view, (.~)) --_1, _2, use, (.=))
import Control.Monad.Free  (Free(Free, Pure))
import Control.Monad.State (MonadState, State, runState)
import Linear              (V2)

import Dreamnet.Engine.World

import Design.DesignAPI

--------------------------------------------------------------------------------

newtype ObjectM s a = ObjectM { runObjectM ∷ State (V2 Int, Object s) a }
                    deriving (Functor, Applicative, Monad, MonadState (V2 Int, Object s))


--instance ObjectAPI States (ObjectM States) where
--    position = use _1
--
--    move v = _1 .= v
--
--    showInfoWindow s = Free $ ShowInfoWindow s (Pure ())
--
--    showCustomUi = Free $ ShowCustomUi (Pure ())
--
--    -- startConversation ch = Free $ StartConversation ch (Pure ())
--
--    passable = Free $ Passable Pure
--
--    setPassable c = Free $ SetPassable c (Pure ())
--
--    seeThrough = Free $ SeeThrough Pure
--
--    setSeeThrough s = Free $ SetSeeThrough s (Pure ())
--
--    canSee v = Free $ CanSee v Pure
--
--    changeSymbol s = Free $ ChangeSymbol s (Pure ())
--
--    changeMat s = Free $ ChangeMat s (Pure ())
--
--    message m = Free $ Message m (Pure ())
--
--    put v = Free $ Put v (Pure ())
--
--    get = Free $ Get Pure
--
--    scanRange r f = Free $ ScanRange r f Pure
    


runObject ∷ (Monad w, WorldAPI States v w) ⇒ ObjectM States a → V2 Int → Object States → w (a, GameState)
runObject prg v o =
    let (x, (_, _)) = runState (runObjectM prg) (v, o)
    in  pure (x, Normal)  -- TODO bad

--------------------------------------------------------------------------------

-- TODO this object monad really doesn't have to exist. Everything could be
--      implemented simply through WorldAPI.
data ObjectF s a = Move (V2 Int) a
                 | Position (V2 Int → a)
                 | ShowInfoWindow String a
                 | ShowCustomUi a
                 -- | StartConversation DreamnetCharacter a
                 | Passable (Bool → a)
                 | SetPassable Bool a
                 | SeeThrough (Bool → a)
                 | SetSeeThrough Bool a
                 | CanSee (V2 Int) (Bool → a)
                 | ChangeSymbol Symbol a
                 | ChangeMat String a
                 | Message String a
                 | Put s a
                 | Get (s → a)
                 | ScanRange Word (Object s → Bool) ([(V2 Int, Object s)] → a)
                 deriving(Functor) -- TODO Derive binary can't work with functions


instance ObjectAPI s (Free (ObjectF s)) where
    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    showInfoWindow s = Free $ ShowInfoWindow s (Pure ())

    showCustomUi = Free $ ShowCustomUi (Pure ())

    -- startConversation ch = Free $ StartConversation ch (Pure ())

    passable = Free $ Passable Pure

    setPassable c = Free $ SetPassable c (Pure ())

    seeThrough = Free $ SeeThrough Pure

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

    changeSymbol s = Free $ ChangeSymbol s (Pure ())

    changeMat s = Free $ ChangeMat s (Pure ())

    message m = Free $ Message m (Pure ())

    --put v = Free $ Put v (Pure ())

    --get = Free $ Get Pure

    scanRange r f = Free $ ScanRange r f Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI States v w) ⇒ Free (ObjectF States) a → V2 Int → Object States → w (a, GameState)
runObjectMonadWorld op v o = runWithGameState Normal (v, o) op


runWithGameState ∷ (Monad w, WorldAPI States v w) ⇒ GameState → (V2 Int, Object States) → Free (ObjectF States) a → w (a, GameState)
runWithGameState gs (cv, o) (Free (Move v n)) = do
    moveObject cv o v
    runWithGameState gs (v, o) n

runWithGameState gs (cv, o) (Free (Position fv)) = do
    runWithGameState gs (cv, o) (fv cv)

runWithGameState _ (cv, o) (Free (ShowInfoWindow txt n)) = do
    runWithGameState (Examination txt) (cv, o) n
    --runWithGameState (Examination (newScrollData (V2 1 1) (V2 60 30) Nothing txt)) ss (cv, o) n

-- TODO actually implement this, via some handler or something?
runWithGameState gs (cv, o) (Free (ShowCustomUi n)) = do
    --runWithGameState (ComputerOperation cv 1 cd) (cv, o) n -- TODO use *ACTUAL* IX
    runWithGameState gs (cv, o) n

--runWithGameState _ (cv, o) (Free (StartConversation ch n)) = do
--    let cnodes = runConversationF_temp ["Carla", "Whoeverelse"] (view ch_conversation ch)
--    runWithGameState (Conversation cnodes) (cv, o) n

runWithGameState gs (cv, o) (Free (Passable fn)) = do
    runWithGameState gs (cv, o) (fn $ view o_passable o)

runWithGameState gs (cv, o) (Free (SetPassable cl n)) = do
    let no = o_passable .~ cl $ o
    replaceObject cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (SeeThrough fn)) = do
    runWithGameState gs (cv, o) (fn $ view o_seeThrough o)

runWithGameState gs (cv, o) (Free (SetSeeThrough st n)) = do
    let no = o_seeThrough .~ st $ o
    replaceObject cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (CanSee v fs)) = do
    seesV ← and . fmap snd <$> castVisibilityRay cv v
    runWithGameState gs (cv, o) (fs seesV)

runWithGameState gs (cv, o) (Free (ChangeSymbol c n)) = do
    let no = o_symbol .~ c $ o
    replaceObject cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (ChangeMat m n)) = do
    let no = o_material .~ m $ o
    replaceObject cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (Message m n)) = do
    setStatus m
    runWithGameState gs (cv, o) n

runWithGameState gs (cv, o) (Free (Put v n)) = do
    let no = o_state .~ v $ o
    replaceObject cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (Get fn)) = do
    runWithGameState gs (cv, o) (fn . view o_state $ o)

runWithGameState gs (cv, o) (Free (ScanRange r f fn)) = do
    points ← interestingObjects cv r f
    values ← fmap (foldr onlyJust []) $ traverse (fmap lastValue . cellAt) points
    runWithGameState gs (cv, o) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l

runWithGameState gs _ (Pure x) =
    pure (x, gs)

