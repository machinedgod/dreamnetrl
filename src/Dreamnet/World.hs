{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Dreamnet.World
( Object(Object)
, o_name
, o_symbol
, o_material
, o_passable
, o_seeThrough
, o_description

, WorldReadAPI(..)

, WorldAPI(..)
, changeObject_

, World
-- TODO take out and enforce interaction through API class
, w_team
, w_active
, w_map
, w_vis
, w_status
, newWorld

, WorldM
, runWorld
) where

import Prelude hiding (interact, rem)

import Control.Lens               (makeLenses, (^.), (%=), (.=), use, uses,
                                   view)
import Control.Monad              (when, void)
import Control.Monad.State        (MonadState, State, runState)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Semigroup             ((<>))
import Data.Bool                  (bool)
import Data.List                  (find)

import qualified Data.Set            as S  (fromList, member)
import qualified Data.Vector         as V  (Vector, imap, toList, replicate)


import Dreamnet.Entity
import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.GameState
import Dreamnet.Visibility

--------------------------------------------------------------------------------

data Object = Object {
      _o_name        ∷ String
    , _o_symbol      ∷ Char
    , _o_material    ∷ String
    , _o_passable    ∷ Bool
    , _o_seeThrough  ∷ Bool
    , _o_description ∷ String
    }
    deriving (Eq, Show)

makeLenses ''Object

--------------------------------------------------------------------------------

class WorldReadAPI v c w | w → v, w → c where
    worldMap ∷ w (WorldMap Object)
    team ∷ w [Entity c]
    active ∷ w (Entity c)
    castVisibilityRay ∷ V2 Int → V2 Int → w [(V2 Int, Bool)]


-- TODO seriously refactor this into much better DSL
class (WorldReadAPI v c w) ⇒ WorldAPI v c w | w → v, w → c where
    setStatus ∷ String → w ()
    changeObject ∷ V2 Int → (Object → w Object) → w ()
    selChar ∷ w c
    selectCharacter ∷ (c → Bool) → w ()
    moveSelected ∷ V2 Int → w ()
    addObject ∷ V2 Int → Object → w ()
    deleteObject ∷ V2 Int → Object → w ()
    moveObject ∷ V2 Int → Object → V2 Int → w ()
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateVisible ∷ w ()
    -- TODO not really happy with 'update*' anything. Provide a primitive!
    --updateAi ∷ w ()

--------------------------------------------------------------------------------
 
-- | Type variables
--   v: visibility data
--   c: character data
data World v c = World {
      _w_team   ∷ [Entity c] -- TODO if I make this a set, I can prevent equal objects, but put Ord constraint
    , _w_active ∷ Entity c
    , _w_map    ∷ WorldMap Object
    , _w_vis    ∷ V.Vector v
    , _w_status ∷ String
    }

makeLenses ''World


-- TODO consolidate Player characters into the WorldMap, somehow
newWorld ∷ (Monoid v) ⇒ WorldMap Object → [c] → World v c
newWorld m chs =
    let t = new <$> zip (V.toList $ m^.wm_spawns) chs
    in  World {
          _w_team   = drop 1 t
        , _w_active = head t
        , _w_map    = m
        , _w_vis    = V.replicate (fromIntegral $ (width m) * (height m)) mempty
        , _w_status = ""
        }

--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM v c a = WorldM { runWorldM ∷ State (World v c) a }
                     deriving (Functor, Applicative, Monad, MonadState (World v c))

-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
instance WorldReadAPI Visibility c (WorldM Visibility c) where
    worldMap = use w_map

    team = use w_team

    active = use (w_active)

    castVisibilityRay o d = (\m → castVisibilityRay' m o d) <$> use w_map


instance WorldAPI Visibility c (WorldM Visibility c) where
    setStatus s = w_status .= s

    changeObject v fo = do
        m  ← use w_map
        nc ← traverse fo (valuesAt v m)
        w_map %= replaceCell v nc

    moveSelected v = do
        npp     ← uses (w_active.e_position) (+v)
        obj     ← uses w_map (valuesAt npp)
        when (and $ fmap (view o_passable) obj) $
            w_active %= move v

    addObject v o = w_map %= addToCell v o

    deleteObject v o = w_map %= deleteFromCell v o

    -- TODO crashes if np is out of map bounds!!!
    moveObject cp o np = do
        objs  ← uses w_map (valuesAt cp)
        when (o `elem` objs) $ do
            w_map %= addToCell np o . deleteFromCell cp o

    selChar = use (w_active.e_object)

    selectCharacter f = void $ runMaybeT $ do
        nc ← MaybeT $ uses w_team (find (f . view e_object))
        oc ← use w_active
        w_team %= filter (not . f . view e_object)
        w_team %= (<> [oc])
        w_active .= nc

    updateVisible = do
        m ← use w_map
        t ← pure (:)
            <*> use (w_active.e_position)
            <*> uses w_team (fmap (view e_position))

        let linPoints = mconcat $ pointsForOne m <$> t
        -- NOTE resolving 'x' causes lag
        w_vis %= V.imap (\i x → if i `S.member` linPoints
                                  then Visible
                                  else case x of
                                    Visible → Known
                                    _       → x)
        where
            pointsForOne m p =
                let !points    = circle 20 p
                    !los       = concat $ (fmap fst . visibleAndOneExtra . castVisibilityRay' m p) <$> points
                in  S.fromList $ linCoord m <$> los
            visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
            visibleAndOneExtra l =
                let front = takeWhile ((==True) . snd) l
                    rem   = dropWhile ((==True) . snd) l
                in  bool (head rem : front) front (null rem)

    --updateAi = do
    --    m ← use w_map
    --    use (w_map.wm_data) >>= V.imapM_ (\i → traverse_ (runAi (coordLin m i)))


castVisibilityRay' ∷ WorldMap Object → V2 Int → V2 Int → [(V2 Int, Bool)]
castVisibilityRay' m o d = let seeThrough = and . fmap (view o_seeThrough) . (`valuesAt` m)
                           --in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ line o d
                           in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d


runWorld ∷ WorldM v c GameState → World v c → (GameState, World v c)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject_ ∷ (Applicative w, WorldAPI v c w) ⇒ V2 Int → Object → Object → w ()
changeObject_ v oo no = changeObject v (\c → pure $ if c == oo
                                                      then no
                                                      else c)

