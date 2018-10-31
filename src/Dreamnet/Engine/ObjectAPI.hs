{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.Engine.ObjectAPI
( InteractionType(..)
, TargetSelectionStyle(..)
, ObjectAPI(..)

, ObjectF(..)
) where

import Control.Monad.Free (Free(..))
import Linear             (V2, V3)

import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Object

--------------------------------------------------------------------------------
-- Object API and objects

data InteractionType a = Examine
                       | Operate
                       | Talk
                       | OperateOn   a
                       | OperateWith a

-- TODO doesn't fit in the world, but in the Game (where Player exists)
data TargetSelectionStyle = Freeform
                          | LineOfSight


-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
-- Note2: WHy?
-- Note3: Because, if it involves player actions, then those objects can't be
--        ran by NPC's to move simulation forward, because NPC's might end up
--        triggering UI windows and what not.
class ObjectAPI o where
    type ObjectAPIState o ∷ *
    type ObjectAPIConversation o ∷ * → *

    position        ∷ o (Safe (V3 Int))
    move            ∷ Safe (V3 Int) → o ()
    passable        ∷ o Bool
    setPassable     ∷ Bool → o () -- Creates a state, creates and object. NO! ..... What?
    seeThrough      ∷ o Bool
    setSeeThrough   ∷ Bool → o ()
    canSee          ∷ Safe (V3 Int) → o Bool
    changeSymbol    ∷ Symbol → o ()
    changeMat       ∷ String → o ()
    message         ∷ String → o ()
    doTalk          ∷ (ObjectAPIConversation o) () → o ()
    operateComputer ∷ o ()
    scanRange       ∷ Int → (ObjectAPIState o → Bool) → o [(Safe (V3 Int), ObjectAPIState o)]
    acquireTarget   ∷ TargetSelectionStyle → o (Safe (V3 Int))
    spawnNewObject  ∷ Safe (V3 Int) → ObjectAPIState o → o ()
    removeObject    ∷ Safe (V3 Int) → o ()
    findObject      ∷ ObjectAPIState o → o (Maybe (Safe (V3 Int)))

    --interact      ∷ InteractionType a → V2 Int → Int → o ()
    -- Keep adding primitives until you can describe all Map Objects as programs

--------------------------------------------------------------------------------
-- TODO this object monad really doesn't have to exist. Everything could be
--      implemented simply through WorldAPI.
data ObjectF s a =
      Position        (Safe (V3 Int) → a)
    | Move            (Safe (V3 Int)) a
    | Passable        (Bool → a)
    | SetPassable     Bool a
    | SeeThrough      (Bool → a)
    | SetSeeThrough   Bool a
    | CanSee          (Safe (V3 Int)) (Bool → a)
    | ChangeSymbol    Symbol a
    | ChangeMat       String a
    | Message         String a
    | DoTalk          (Free (ConversationF s) ()) a
    | OperateComputer a
    | ScanRange       Int (s → Bool) ([(Safe (V3 Int), s)] → a)
    | AcquireTarget   TargetSelectionStyle (Safe (V3 Int) → a)
    | SpawnNewObject  (Safe (V3 Int)) s a
    | RemoveObject    (Safe (V3 Int)) a
    | FindObject      s (Maybe (Safe (V3 Int)) → a)
    -- | Interact (InteractionType s) (V2 Int) Int a
    deriving(Functor)


instance ObjectAPI (Free (ObjectF s)) where
    type ObjectAPIState (Free (ObjectF s)) = s
    type ObjectAPIConversation (Free (ObjectF s)) = Free (ConversationF s)

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

    doTalk c = Free $ DoTalk c (Pure ())

    operateComputer = Free $ OperateComputer (Pure ())

    scanRange r f = Free $ ScanRange r f Pure

    acquireTarget s = Free $ AcquireTarget s Pure

    spawnNewObject v s = Free $ SpawnNewObject v s (Pure ())

    removeObject v = Free $ RemoveObject v (Pure ())

    findObject s = Free $ FindObject s Pure

    --interact i v ix = Free $ Interact i v ix (Pure ())

