{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectAPI(..)
, runObjectMonadWorld

, InteractionType(..)
, door
)
where


import Control.Lens       (view, (.~))
import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2)

import Dreamnet.GameState (GameState(Normal))
import Dreamnet.World     (Object, o_passable, o_seeThrough,
                           changeObject_,
                           WorldReadAPI(castVisibilityRay),
                           WorldAPI(moveObject))

--------------------------------------------------------------------------------

data InteractionType = IT_Operate
                     | IT_Examine

--------------------------------------------------------------------------------

class ObjectAPI o where
    position         ∷ o (V2 Int)
    move             ∷ V2 Int → o ()
    requestGameState ∷ GameState → o ()
    collidable       ∷ o Bool
    setPassable    ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough       ∷ o Bool
    setSeeThrough    ∷ Bool → o ()
    canSee           ∷ V2 Int → o Bool
    -- Keep adding primitives until you can describe all Map Objects as programs

--------------------------------------------------------------------------------

data ObjectF a = Move (V2 Int) a
               | Position (V2 Int → a)
               | RequestGameState GameState a
               | Passable (Bool → a)
               | SetPassable Bool a
               | SeeThrough (Bool → a)
               | SetSeeThrough Bool a
               | CanSee (V2 Int) (Bool → a)
               deriving(Functor) -- TODO Derive binary can't work with functions



instance ObjectAPI (Free ObjectF) where
    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    requestGameState gs =  Free $ RequestGameState gs (Pure ())

    collidable = Free $ Passable Pure

    setPassable c = Free $ SetPassable c (Pure ())

    seeThrough = Free $ SeeThrough Pure

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI v c w) ⇒ Free ObjectF a → V2 Int → Object → w (a, GameState)
runObjectMonadWorld op v o = runWithGameState Normal (v, o) op


runWithGameState ∷ (Monad w, WorldAPI v c w) ⇒ GameState → (V2 Int, Object) → Free ObjectF a → w (a, GameState)
runWithGameState gs (cv, o) (Free (Move v n)) = do
    moveObject cv o v
    runWithGameState gs (v, o) n

runWithGameState gs (cv, o) (Free (Position fv)) = do
    runWithGameState gs (cv, o) (fv cv)

runWithGameState _ (cv, o) (Free (RequestGameState gs n)) = do
    runWithGameState gs (cv, o) n

runWithGameState gs (cv, o) (Free (Passable fn)) = do
    runWithGameState gs (cv, o) (fn $ view o_passable o)

runWithGameState gs (cv, o) (Free (SetPassable cl n)) = do
    let no = o_passable .~ cl $ o
    changeObject_ cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (SeeThrough fn)) = do
    runWithGameState gs (cv, o) (fn $ view o_seeThrough o)

runWithGameState gs (cv, o) (Free (SetSeeThrough st n)) = do
    let no = o_seeThrough .~ st $ o
    changeObject_ cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (CanSee v fs)) = do
    seesV ← and . fmap snd <$> castVisibilityRay cv v
    runWithGameState gs (cv, o) (fs seesV)

runWithGameState gs _ (Pure x) = pure (x, gs)

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ InteractionType → Free ObjectF ()
door IT_Operate = collidable >>= setPassable . not
door _          = pure ()

--objectToChar (Door o)         = bool '+' '\'' o
--objectToChar (Stairs u)       = bool '<' '>' u
--objectToChar (Camera l)       = intToDigit l
--objectToChar Computer         = '$'
--objectToChar (ItemO _)        = '['

-- | Detects foes
--camera ∷ Free ObjectF Bool
--camera = position >>= canSee
    --runAi v c@(Camera l) = do
    --    pv         ← view e_position <$> active  -- TODO whole team!
    --    seesPlayer ← and . fmap snd <$> castVisibilityRay v pv
    --    if seesPlayer
    --        then changeObject_ v c (Camera (min 9 (l + 1)))
    --        else changeObject_ v c (Camera (max 0 (l - 1)))


--isPassable (Person c)       = or . fmap nameMatches <$> team
--    where
--        nameMatches c' = view ch_name c == view (e_object.ch_name) c'
