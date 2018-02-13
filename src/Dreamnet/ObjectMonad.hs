{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
( ObjectAPI(..)
, runObjectMonadWorld

, InteractionType(..)

, ObjectF
, door
, computer
, person
, generic

)
where


import Control.Lens       (view, (.~))
import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2)
import Data.Bool          (bool)
import Data.Monoid        ((<>))

import Dreamnet.GameState (GameState(..))
import Dreamnet.World     (Object, o_symbol, o_material, o_passable, o_seeThrough, 
                           changeObject_,
                           WorldReadAPI(castVisibilityRay),
                           WorldAPI(moveObject, setStatus))

--------------------------------------------------------------------------------

data InteractionType = Operate
                     | Talk
                     | OperateOn

--------------------------------------------------------------------------------

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI o where
    position         ∷ o (V2 Int)
    move             ∷ V2 Int → o ()
    requestGameState ∷ GameState → o ()
    passable         ∷ o Bool
    setPassable      ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough       ∷ o Bool
    setSeeThrough    ∷ Bool → o ()
    canSee           ∷ V2 Int → o Bool
    changeChar       ∷ Char → o ()
    changeMat        ∷ String → o ()
    message          ∷ String → o ()
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
               | ChangeChar Char a
               | ChangeMat String a
               | Message String a
               deriving(Functor) -- TODO Derive binary can't work with functions


instance ObjectAPI (Free ObjectF) where
    position = Free $ Position Pure

    move v = Free $ Move v (Pure ())

    requestGameState gs =  Free $ RequestGameState gs (Pure ())

    passable = Free $ Passable Pure

    setPassable c = Free $ SetPassable c (Pure ())

    seeThrough = Free $ SeeThrough Pure

    setSeeThrough s = Free $ SetSeeThrough s (Pure ())

    canSee v = Free $ CanSee v Pure

    changeChar c = Free $ ChangeChar c (Pure ())

    changeMat s = Free $ ChangeMat s (Pure ())

    message m = Free $ Message m (Pure ())

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

runWithGameState gs (cv, o) (Free (ChangeChar c n)) = do
    changeObject_ cv o (o_symbol .~ c $ o)
    runWithGameState gs (cv, o) n

runWithGameState gs (cv, o) (Free (ChangeMat m n)) = do
    changeObject_ cv o (o_material .~ m $ o)
    runWithGameState gs (cv, o) n

runWithGameState gs (cv, o) (Free (Message m n)) = do
    setStatus m
    runWithGameState gs (cv, o) n

runWithGameState gs _ (Pure x) = pure (x, gs)

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ InteractionType → Free ObjectF ()
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeChar $ bool '+' '\'' c
    --passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
    message $ "Doors are now " <>  bool "closed." "opened." c
door Talk =
    message "\"Open sesame!\" you yell, but doors are deaf and numb to your pleas."
door OperateOn =
    message "Operating door on something else..."


computer ∷ InteractionType → Free ObjectF ()
computer Operate =
    message "You can't login to this machine."
computer Talk =
    message "You'd think that in this age, computers would actually respond to voice commands. As it is, this one actually does not."
computer OperateOn =
    message "Operating computer on something else. Yikes."

person ∷ InteractionType → Free ObjectF ()
person Operate =
    message "Unsure yourself about what exactly you're trying to pull off, ??? meets your 'operation' attempts with suspicious look."
person Talk =
    requestGameState Conversation
person OperateOn =
    message "You try and operate ??? on whatever. Lol."


generic ∷ InteractionType → Free ObjectF () 
generic Operate =
    message "Trying to interact with whatever."
generic Talk =
    message "Trying to talk to whatever."
generic OperateOn =
    message "You try and operate whatever on whatever."

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
