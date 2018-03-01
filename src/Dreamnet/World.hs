{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Dreamnet.World
( Object(Object)
, o_symbol
, o_material
, o_passable
, o_seeThrough
, o_height
, o_state

, WorldReadAPI(..)

, WorldAPI(..)
, replaceObject

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
import Safe           (at)

import Control.Lens               (makeLenses, (^.), (%=), (.=), use, uses,
                                   view)
import Control.Monad              (when, void)
import Control.Monad.State.Strict (MonadState, State, runState, execState, modify)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Semigroup             ((<>))
import Data.Bool                  (bool)
import Data.List                  (find)

import qualified Data.Set    as S  (fromList, member)
import qualified Data.Vector as V  (Vector, imap, toList, replicate)


import Dreamnet.Entity
import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Visibility

--------------------------------------------------------------------------------

data Object a = Object {
      _o_symbol      ∷ Char
    , _o_material    ∷ String
    , _o_passable    ∷ Bool
    , _o_seeThrough  ∷ Bool
    , _o_height      ∷ Word

    , _o_state ∷ a
    --, _o_state ∷ M.Map String String
    }
    deriving (Eq, Show)

makeLenses ''Object

--------------------------------------------------------------------------------
 
-- | Type variables
--   v: visibility data
--   c: character data
--   TODO place team and active in the world map
data World o v = World {
      _w_team    ∷ [Entity (Object o)] -- TODO if I make this a set, I can prevent equal objects, but put Ord constraint
    , _w_active  ∷ Entity (Object o)
    , _w_map     ∷ WorldMap (Object o)
    , _w_vis     ∷ V.Vector v
    , _w_status  ∷ String
    }

makeLenses ''World


-- TODO consolidate Player characters into the WorldMap, somehow
newWorld ∷ (Monoid v) ⇒ WorldMap (Object o) → [Object o] → World o v
newWorld m chs =
    let t  = newEntity <$> zip (V.toList $ m^.wm_spawns) chs
    in  World {
          _w_team   = drop 1 t
        , _w_active = head t
        , _w_map    = execState (traverse (modify . (addToCell <$> view e_position <*> view e_object)) t) m
        , _w_vis    = V.replicate (fromIntegral $ (width m) * (height m)) mempty
        , _w_status = ""
        }


castVisibilityRay' ∷ WorldMap (Object o) → V2 Int → V2 Int → [(V2 Int, Bool)]
castVisibilityRay' m o d = let seeThrough = and . fmap (view o_seeThrough) . (`valuesAt` m)
                           --in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ line o d
                           in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d

--------------------------------------------------------------------------------

class WorldReadAPI o v w | w → o, w → v where
    worldMap ∷ w (WorldMap (Object o))
    team ∷ w [Entity (Object o)]
    active ∷ w (Entity (Object o))
    castVisibilityRay ∷ V2 Int → V2 Int → w [(V2 Int, Bool)]


-- TODO seriously refactor this into much better DSL
class (WorldReadAPI o v w) ⇒ WorldAPI o v w | w → o, w → v where
    setStatus ∷ String → w ()
    changeObject ∷ V2 Int → (Object o → w (Object o)) → w ()
    modifyObjectAt ∷ V2 Int → Int → (Object o → w (Object o)) → w ()
    selectCharacter ∷ o → w ()
    moveActive ∷ V2 Int → w ()
    changeActive ∷ (Object o → Object o) → w ()
    addObject ∷ V2 Int → Object o → w ()
    deleteObject ∷ V2 Int → Object o → w ()
    moveObject ∷ V2 Int → Object o → V2 Int → w ()
    replaceObjectAt ∷ V2 Int → Int → Object o → w ()
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateVisible ∷ w ()
    -- TODO not really happy with 'update*' anything. Provide a primitive!
    --updateAi ∷ w ()


replaceObject ∷ (Eq o, Applicative w, WorldAPI o v w) ⇒ V2 Int → Object o → Object o → w ()
replaceObject v oo no = changeObject v (\c → pure $ if c == oo
                                                      then no
                                                      else c)


--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM o v a = WorldM { runWorldM ∷ State (World o v) a }
                     deriving (Functor, Applicative, Monad, MonadState (World o v))

-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
instance WorldReadAPI o Visibility (WorldM o Visibility) where
    worldMap = use w_map

    team = use w_team

    active = use (w_active)

    castVisibilityRay o d = (\m → castVisibilityRay' m o d) <$> use w_map


instance (Eq o) ⇒ WorldAPI o Visibility (WorldM o Visibility) where
    setStatus s = w_status .= s

    changeObject v fo = do
        m  ← use w_map
        nc ← traverse fo (valuesAt v m)
        w_map %= replaceCell v nc

    modifyObjectAt v ix f = do
        no ← uses w_map (valuesAt v) >>= f . (`at` ix)
        replaceObjectAt v ix no

    selectCharacter n = void $ runMaybeT $ do
        nc ← MaybeT $ uses w_team (find ((n==) . view (e_object.o_state)))
        oc ← use w_active
        w_team %= filter (not . (==nc))
        w_team %= (<> [oc])
        w_active .= nc

    moveActive v = do
        cp   ← use (w_active.e_position)
        o    ← use (w_active.e_object)
        tobj ← uses w_map (valuesAt (cp + v))
        when (and $ fmap (view o_passable) tobj) $ do
            w_active %= moveEntity v
            moveObject cp o (cp + v)

    changeActive f = do
        cp ← use (w_active.e_position)
        o  ← use (w_active.e_object)
        w_active %= fmap f
        use (w_active.e_object) >>= replaceObject cp o

    addObject v o = w_map %= addToCell v o

    deleteObject v o = w_map %= deleteFromCell v o

    -- TODO crashes if np is out of map bounds!!!
    moveObject cp o np = do
        objs ← uses w_map (valuesAt cp)
        when (o `elem` objs) $ do
            w_map %= addToCell np o . deleteFromCell cp o

    replaceObjectAt v ix o =
        w_map %= modifyCell v (\l → take ix l <> [o] <> drop (ix + 1) l)
        
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

runWorld ∷ WorldM o v a → World o v → (a, World o v)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------
