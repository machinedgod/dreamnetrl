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
, camera

)
where


import Control.Lens       (view, views, (.~), (%~))
import Control.Monad.Free (Free(Free, Pure))
import Linear             (V2)
import Data.Bool          (bool)
import Data.Monoid        ((<>))
import Data.Maybe         (fromMaybe)

import qualified Data.Map as M ((!), lookup, insert)

import Dreamnet.GameState (GameState(..))
import Dreamnet.World     (Object, o_symbol, o_material, o_passable,
                           o_seeThrough, o_state, changeObject_,
                           WorldReadAPI(castVisibilityRay, worldMap),
                           WorldAPI(moveObject, setStatus))
import Dreamnet.WorldMap  (valuesAt, interestingObjects)

--------------------------------------------------------------------------------

data InteractionType = Operate
                     | Talk
                     | OperateOn
                     | AiTick

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
    put              ∷ String → String → o ()
    get              ∷ String → o String
    scanRange        ∷ Word → (Object → Bool) → o [(V2 Int, Object)]
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
               | Put String String a
               | Get String (String → a)
               | ScanRange Word (Object → Bool) ([(V2 Int, Object)] → a)
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

    put k v = Free $ Put k v (Pure ())

    get k = Free $ Get k Pure

    scanRange r f = Free $ ScanRange r f Pure

--------------------------------------------------------------------------------

runObjectMonadWorld ∷ (Monad w, WorldAPI v w) ⇒ Free ObjectF a → V2 Int → Object → w (a, GameState)
runObjectMonadWorld op v o = runWithGameState Normal (v, o) op


runWithGameState ∷ (Monad w, WorldAPI v w) ⇒ GameState → (V2 Int, Object) → Free ObjectF a → w (a, GameState)
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
    let no = o_symbol .~ c $ o
    changeObject_ cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (ChangeMat m n)) = do
    let no = o_material .~ m $ o
    changeObject_ cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (Message m n)) = do
    setStatus m
    runWithGameState gs (cv, o) n

runWithGameState gs (cv, o) (Free (Put k v n)) = do
    let no = o_state %~ M.insert k v $ o
    changeObject_ cv o no
    runWithGameState gs (cv, no) n

runWithGameState gs (cv, o) (Free (Get k fn)) = do
    let v = fromMaybe "" $ M.lookup k $ view o_state o
    runWithGameState gs (cv, o) (fn v)

runWithGameState gs (cv, o) (Free (ScanRange r f fn)) = do
    m ← worldMap
    let points = interestingObjects cv r f m
    let v      = zip points (last . (`valuesAt` m) <$> points)
    runWithGameState gs (cv, o) (fn v)

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
door AiTick =
    pure ()


computer ∷ InteractionType → Free ObjectF ()
computer Operate =
    message "You can't login to this machine."
computer Talk =
    message "You'd think that in this age, computers would actually respond to voice commands. As it is, this one actually does not."
computer OperateOn =
    message "Operating computer on something else. Yikes."
computer AiTick =
    pure ()


person ∷ InteractionType → Free ObjectF ()
person Operate = do
    name ← get "name"
    message $ "Unsure yourself about what exactly you're trying to pull off, " <> name <> " meets your 'operation' attempts with suspicious look."
person Talk =
    requestGameState Conversation
person OperateOn = do
    name ← get "name"
    message $ "You try and operate " <> name <> " on whatever. Lol."
person AiTick =
    pure ()


generic ∷ InteractionType → Free ObjectF () 
generic Operate =
    message "Trying to interact with whatever."
generic Talk =
    message "Trying to talk to whatever."
generic OperateOn =
    message "You try and operate whatever on whatever."
generic AiTick =
    pure ()


camera ∷  InteractionType → Free ObjectF ()
camera Operate = do
    os   ← scanRange 8 ((=='@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    traverse isFoe viso >>= put "level" . show . length . filter id
    get "level" >>= message . ("Camera alarm level: " <>)
    where
        isFoe o = (views o_state (M.! "alliance") o /=) <$> get "alliance"
camera AiTick =
    pure ()
camera _ = 
    message "That won't work."


--objectToChar (Stairs u)       = bool '<' '>' u
--objectToChar (Camera l)       = intToDigit l
--objectToChar Computer         = '$'
--objectToChar (ItemO _)        = '['


--isPassable (Person c)       = or . fmap nameMatches <$> team
--    where
--        nameMatches c' = view ch_name c == view (e_object.ch_name) c'
