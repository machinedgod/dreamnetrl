{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Engine.ObjectAPI
( Symbol(Symbol)

, Object(Object)
, o_symbol
, o_material
, o_passable
, o_seeThrough
, o_height
, o_state

, InteractionType(..)
, TargetSelectionStyle(..)
, ObjectAPI(..)

, ObjectF(..)
) where

import Control.Lens       (makeLenses, view)
import Control.Monad.Free (Free(..))
import Linear             (V2)

import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Visibility

--------------------------------------------------------------------------------

newtype Symbol = Symbol Char
               deriving (Eq, Show)

instance Semigroup Symbol where
    (Symbol ' ') <> (Symbol ch') = Symbol ch'
    (Symbol ch)  <> (Symbol ' ') = Symbol ch
    _            <> (Symbol ch') = Symbol ch'  -- TODO make correct, add char codes

instance Monoid Symbol where
    mempty = Symbol ' '

--------------------------------------------------------------------------------

data Object a = Object {
      _o_symbol      ∷ Symbol
    , _o_material    ∷ String
    , _o_passable    ∷ Bool
    , _o_seeThrough  ∷ Bool
    , _o_height      ∷ Int

    , _o_state ∷ a
    }
    deriving (Eq, Show, Functor)
makeLenses ''Object


instance Applicative Object where
    pure = Object mempty "" False False 0
    (Object s m ps st h f) <*> (Object s' m' ps' st' h' x) =
        Object (s <> s') (m <> m') (ps || ps') (st || st') (h + h') (f x)


instance Monad Object where
    (Object _ _ _ _ _ x)  >>= f = f x


instance VisibleAPI (Object a) where
    isSeeThrough = view o_seeThrough
    height       = view o_height

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
class ObjectAPI s o | o → s where
    position        ∷ o (V2 Int, Int)
    move            ∷ V2 Int → o ()
    passable        ∷ o Bool
    setPassable     ∷ Bool → o () -- Creates a state, creates and object. NO! ..... What?
    seeThrough      ∷ o Bool
    setSeeThrough   ∷ Bool → o ()
    canSee          ∷ V2 Int → o Bool
    changeSymbol    ∷ Symbol → o ()
    changeMat       ∷ String → o ()
    message         ∷ String → o ()
    doTalk          ∷ Free (ConversationF s) () → o ()
    operateComputer ∷ o ()
    scanRange       ∷ Word → (Object s → Bool) → o [(V2 Int, Object s)]
    acquireTarget   ∷ TargetSelectionStyle → o (V2 Int)
    spawnNewObject  ∷ V2 Int → s → o ()
    removeObject    ∷ V2 Int → Int → o ()
    findObject      ∷ s → o (Maybe (V2 Int, Int))

    --interact      ∷ InteractionType a → V2 Int → Int → o ()
    -- Keep adding primitives until you can describe all Map Objects as programs

--------------------------------------------------------------------------------
-- TODO this object monad really doesn't have to exist. Everything could be
--      implemented simply through WorldAPI.
data ObjectF s a = Position ((V2 Int, Int) → a)
                 | Move (V2 Int) a
                 | Passable (Bool → a)
                 | SetPassable Bool a
                 | SeeThrough (Bool → a)
                 | SetSeeThrough Bool a
                 | CanSee (V2 Int) (Bool → a)
                 | ChangeSymbol Symbol a
                 | ChangeMat String a
                 | Message String a
                 | DoTalk (Free (ConversationF s) ()) a
                 | OperateComputer a
                 | ScanRange Word (Object s → Bool) ([(V2 Int, Object s)] → a)
                 | AcquireTarget TargetSelectionStyle (V2 Int → a)
                 | SpawnNewObject (V2 Int) s a
                 | RemoveObject (V2 Int) Int a
                 | FindObject s (Maybe (V2 Int, Int) → a)
                 -- | Interact (InteractionType s) (V2 Int) Int a
                 deriving(Functor)


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

    doTalk c = Free $ DoTalk c (Pure ())

    operateComputer = Free $ OperateComputer (Pure ())

    scanRange r f = Free $ ScanRange r f Pure

    acquireTarget s = Free $ AcquireTarget s Pure

    spawnNewObject v s = Free $ SpawnNewObject v s (Pure ())

    removeObject v i = Free $ RemoveObject v i (Pure ())

    findObject s = Free $ FindObject s Pure

    --interact i v ix = Free $ Interact i v ix (Pure ())

