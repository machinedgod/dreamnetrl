{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Dreamnet.World
( WorldReadAPI(..)

, WorldAPI(..)
, changeObject_
, interact
, examine
, get

, World
-- TODO take out and enforce interaction through API class
, w_team
, w_active
, w_aim
, w_map
, w_vis
, w_status
, newWorld

, WorldM
, runWorld
) where

import Prelude hiding (interact, rem)
import Safe

import Control.Lens               (makeLenses, (^.), (%=), (.=), use, uses,
                                   view)
import Control.Monad              (when, (<=<), void)
import Control.Monad.State        (MonadState, State, runState)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Foldable              (traverse_)
import Data.Semigroup             ((<>))
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)
import Data.List                  (find)

import qualified Data.Set            as S  (fromList, member)
import qualified Data.Vector         as V  (Vector, imap, imapM_,
                                            toList, replicate)


import Dreamnet.Entity
import Dreamnet.ObjectProperties
import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.GameState
import Dreamnet.Visibility

--------------------------------------------------------------------------------

class WorldReadAPI a b c w | w → a, w → b, w → c where
    worldMap ∷ w (WorldMap a)
    team ∷ w [Entity c]
    active ∷ w (Entity c)
    castVisibilityRay ∷ (IsSeeThrough a) ⇒ V2 Int → V2 Int → w [(V2 Int, Bool)]


-- TODO seriously refactor this into much better DSL
class (Eq a, WorldReadAPI a b c w) ⇒ WorldAPI a b c w | w → a, w → b, w → c where
    setStatus ∷ String → w ()
    changeObject ∷ V2 Int → (a → w a) → w ()
    selChar ∷ w c
    selectCharacter ∷ (c → Bool) → w ()
    moveSelected ∷ (IsPassable w a) ⇒ V2 Int → w ()
    addObject ∷ V2 Int → a → w ()
    deleteObject ∷ V2 Int → a → w ()
    moveObject ∷ V2 Int → a → V2 Int → w ()
    switchAim ∷ Maybe (a → Bool) → w ()
    moveAim ∷ V2 Int → w ()
    interactOrElse ∷ (V2 Int → [a] → w d) → w d → w d
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateVisible ∷ (IsSeeThrough a) ⇒ w ()
    -- TODO not really happy with 'update*' anything. Provide a primitive!
    updateAi ∷ (HasAi w a) ⇒ w ()

--------------------------------------------------------------------------------
 
-- | Type variables
--   a: gameplay data
--   b: visibility data
--   c: character data
data World a b c = World {
      _w_team   ∷ [Entity c] -- TODO if I make this a set, I can prevent equal objects, but put Ord constraint
    , _w_active ∷ Entity c
    , _w_aim    ∷ Maybe (V2 Int)
    , _w_map    ∷ WorldMap a
    , _w_vis    ∷ V.Vector b
    , _w_status ∷ String
    }

makeLenses ''World


-- TODO consolidate Player characters into the WorldMap, somehow
newWorld ∷ (Monoid b) ⇒ WorldMap a → [c] → World a b c
newWorld m chs =
    let t = new <$> zip (V.toList $ m^.wm_spawns) chs
    in  World {
          _w_team   = drop 1 t
        , _w_active = head t
        , _w_aim    = Nothing
        , _w_map    = m
        , _w_vis    = V.replicate (fromIntegral $ (width m) * (height m)) mempty
        , _w_status = ""
        }

--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM a b c d = WorldM { runWorldM ∷ State (World a b c) d }
                       deriving (Functor, Applicative, Monad, MonadState (World a b c))

-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
instance (Eq a) ⇒ WorldReadAPI a Visibility b (WorldM a Visibility b) where
    worldMap = use w_map

    team = use w_team

    active = use (w_active)

    castVisibilityRay o d = (\m → castVisibilityRay' m o d) <$> use w_map


instance (Eq a) ⇒ WorldAPI a Visibility b (WorldM a Visibility b) where
    setStatus s = w_status .= s

    changeObject v fo = do
        m  ← use w_map
        nc ← traverse fo (valuesAt v m)
        w_map %= replaceCell v nc

    moveSelected v = do
        npp     ← uses (w_active.e_position) (+v)
        obj     ← uses w_map (valuesAt npp)
        canWalk ← traverse isPassable obj
        when (and canWalk) $
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

    switchAim (Just nof) = do
        pp ← use (w_active.e_position)
        os ← uses w_map (interestingObjects pp 2 nof)
        ca ← use w_aim
        case ca of
            Just a → w_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
            _      → w_aim .= headMay os

    switchAim Nothing = w_aim .= Nothing

    moveAim v = w_aim %= fmap (+v)

    interactOrElse f e = fromMaybe e <=< runMaybeT $ do
        v  ← MaybeT (use w_aim)
        os ← uses w_map (valuesAt v)
        pure (f v os)

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

    updateAi = do
        m ← use w_map
        use (w_map.wm_data) >>= V.imapM_ (\i → traverse_ (runAi (coordLin m i)))


castVisibilityRay' ∷ (IsSeeThrough a) ⇒ WorldMap a → V2 Int → V2 Int → [(V2 Int, Bool)]
castVisibilityRay' m o d = let seeThrough = areSeeThrough . (`valuesAt` m)
                           --in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ line o d
                           in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d


runWorld ∷ WorldM a b c GameState → World a b c → (GameState, World a b c)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject_ ∷ (Applicative w, WorldAPI a b c w) ⇒ V2 Int → a → a → w ()
changeObject_ v oo no = changeObject v (\c → pure $ if c == oo
                                                      then no
                                                      else c)


interact ∷ (Applicative w, WorldAPI a b c w) ⇒ (V2 Int → [a] → w ()) → w ()
interact f = interactOrElse f (pure ())


examine ∷ (Applicative w, Describable a, WorldAPI a b c w) ⇒ w String
examine = interactOrElse (\_ → pure . describeAll) (desc <$> worldMap)


get ∷ (Applicative w, WorldAPI a b c w) ⇒ w (Maybe d)
get = pure Nothing

