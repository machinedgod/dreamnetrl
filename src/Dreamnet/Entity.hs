{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Dreamnet.Entity
( Entity
, e_position
, e_object

, newEntity
, moveEntity
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

newEntity ∷ (V2 Int, a) → Entity a
newEntity = uncurry Entity


moveEntity ∷ V2 Int → Entity a → Entity a
moveEntity v = e_position %~ (+v)

