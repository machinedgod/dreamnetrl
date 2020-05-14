{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Engine.Object
( Symbol(Symbol), sChar

, Object(Object), oSymbol, oMaterial, oPassable, oSeeThrough, oState
)
where


import Control.Lens (makeLenses, view)

import Dreamnet.Engine.Visibility

--------------------------------------------------------------------------------

newtype Symbol = Symbol { _sChar ∷ Char }
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
      _oSymbol      ∷ Symbol
    , _oMaterial    ∷ String
    , _oPassable    ∷ Bool
    , _oSeeThrough  ∷ Bool

    , _oState ∷ a
    }
    deriving (Eq, Show, Functor)
makeLenses ''Object


instance Applicative Object where
    pure = Object mempty "" False False
    (Object s m ps st f) <*> (Object s' m' ps' st' x) =
        Object (s <> s') (m <> m') (ps || ps') (st || st') (f x)


instance Monad Object where
    (Object _ _ _ _ x) >>= f = f x


instance VisibleAPI (Object a) where
    isSeeThrough = view oSeeThrough

