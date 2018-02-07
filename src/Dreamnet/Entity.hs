{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Entity
( Entity
, e_position
, e_object

, new
, move
) where


import Control.Lens (makeLenses, (%~))
import Linear       (V2(V2))

--------------------------------------------------------------------------------

data Entity a = Entity {
      _e_position ∷ (V2 Int)
    , _e_object ∷ a
    }
    deriving(Eq, Show, Functor)
makeLenses ''Entity

instance Applicative Entity where
    pure = Entity (V2 0 0)
    (Entity v f) <*> (Entity v2 o) = Entity (v + v2) (f o)

instance Monad Entity where
    (Entity _ x) >>= f = f x

--------------------------------------------------------------------------------

new ∷ (V2 Int, a) → Entity a
new = uncurry Entity


move ∷ V2 Int → Entity a → Entity a
move v = e_position %~ (+v)

