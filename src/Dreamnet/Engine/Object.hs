{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Engine.Object
( Symbol(Symbol), s_char

, Object(Object), o_symbol, o_material, o_passable, o_seeThrough, o_height
, o_state
)
where


import Control.Lens (makeLenses, view)

import Dreamnet.Engine.Visibility

--------------------------------------------------------------------------------

newtype Symbol = Symbol { _s_char ∷ Char }
               deriving (Eq, Show)
makeLenses ''Symbol


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

