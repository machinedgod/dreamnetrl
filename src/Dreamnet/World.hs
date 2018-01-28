{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Dreamnet.World
( WorldReadAPI(..)

, WorldAPI(..)
, changeObject_
, interact

, World
-- TODO take out and enforce interaction through API class
, w_playerPos
, w_playerCharacter
, w_aim
, w_map
, w_status
, newWorld

, WorldM
, runWorld
) where

import Prelude hiding (interact, rem)
import Safe

import Control.Lens               (makeLenses, (^.), (%=), (+=), (.=), use, uses)
import Control.Monad              (when, (<=<), void)
import Control.Monad.State        (MonadState, State, runState)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)

import qualified Data.Set    as S (fromList, member)
import qualified Data.Vector as V ((!?), (//), imap, imapM)

import Dreamnet.ObjectProperties
import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.GameState
import Dreamnet.Visibility

--------------------------------------------------------------------------------

class WorldReadAPI a b c w | w → a, w → b, w → c where
    worldMap ∷ w (WorldMap a b)
    playerPos ∷ w (V2 Int)
    castVisibilityRay ∷ (IsSeeThrough a) ⇒ V2 Int → V2 Int → w [(V2 Int, Bool)]

-- TODO seriously refactor this into much better DSL
class (WorldReadAPI a b c w) ⇒ WorldAPI a b c w | w → a, w → b, w → c where
    setStatus ∷ String → w ()
    changeObject ∷ V2 Int → (a → w a) → w ()
    playerCharacter ∷ w c
    movePlayer ∷ (IsPassable a) ⇒ V2 Int → w ()
    moveObject ∷ V2 Int → (a → Bool) → V2 Int → w ()
    switchAim ∷ Maybe (a → Bool) → w ()
    interactOrElse ∷ (V2 Int → a → w d) → w d → w d
    examine ∷ (Describable a) ⇒ w String
    get ∷ w (Maybe d)
    updateVisible ∷ (IsSeeThrough a) ⇒ w ()
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateAi ∷ (HasAi w a) ⇒ w ()

    

--------------------------------------------------------------------------------
 
-- | Type variables
--   a: gameplay data
--   b: visibility data
--   c: character data
data World a b c = World {
      _w_playerPos       ∷ V2 Int
    , _w_playerCharacter ∷ c
    , _w_aim             ∷ Maybe (V2 Int)
    , _w_map             ∷ WorldMap a b
    , _w_status          ∷ String
    }

makeLenses ''World


newWorld ∷ WorldMap a b → c → World a b c
newWorld m ch = 
    World {
      _w_playerPos       = fromMaybe (error "Map is missing spawn points!") $ (m^.wm_spawns) V.!? 0
    , _w_playerCharacter = ch
    , _w_aim             = Nothing
    , _w_map             = m
    , _w_status          = ""
    }

--------------------------------------------------------------------------------

newtype WorldM a b c d = WorldM { runWorldM ∷ State (World a b c) d }
                       deriving (Functor, Applicative, Monad, MonadState (World a b c))

instance WorldReadAPI a Visibility b (WorldM a Visibility b) where
    worldMap = use w_map
    playerPos = use w_playerPos
    castVisibilityRay o d = (\m → castVisibilityRay' m o d) <$> use w_map

instance (IsPassable a, Describable a) ⇒ WorldAPI a Visibility b (WorldM a Visibility b) where
    setStatus s = w_status .= s
    changeObject v fo = do
        m  ← use w_map
        no ← fo (objectAt v m)
        -- Hackage says this is O(m + 1) for a single update :-(
        w_map.wm_data %= (V.// [(linCoord m v, no)])
    movePlayer v = do
        npp ← uses w_playerPos (+v)
        obj ← uses w_map (objectAt npp)
        when (isPassable obj) $
            w_playerPos += v
    moveObject cp ff np = do
        m  ← use w_map
        md ← use (w_map.wm_data)
        void $ runMaybeT $ do
            os ← MaybeT (pure $ md V.!? linCoord m cp)
            when (ff os) $ do
                -- TODO ACTUALLY DO MOVE SHIT AROUND!
                pure ()
    playerCharacter = use w_playerCharacter
    switchAim (Just nof) = do
        pp ← use w_playerPos
        os ← uses w_map (interestingObjects pp 2 nof)
        ca ← use w_aim
        case ca of
            Just a → w_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
            _      → w_aim .= headMay os
    switchAim Nothing = w_aim .= Nothing
    interactOrElse f e = fromMaybe e <=< runMaybeT $ do
        v ← MaybeT (use w_aim)
        o ← uses w_map (objectAt v)
        return (f v o)
    examine = interactOrElse (const (pure . description)) (use (w_map.wm_desc))
    get = pure Nothing
    updateVisible = do
        pp ← playerPos
        m  ← worldMap
        let points    = circle 20 pp
            los       = concat $ (fmap fst . visibleAndOneExtra . castVisibilityRay' m pp) <$> points
            linPoints = S.fromList $ linCoord m <$> los
        -- TODO resolving 'x' causes lag
        w_map.wm_visible %= V.imap (\i x → if i `S.member` linPoints
                                        then Visible
                                        else case x of
                                            Visible → Known
                                            _       → x)
        where
            visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
            visibleAndOneExtra l = let front = takeWhile ((==True) . snd) l
                                       rem   = dropWhile ((==True) . snd) l
                                   in  bool (head rem : front) front (null rem)
    updateAi = do
        m  ← use w_map
        use (w_map.wm_data) >>=
          V.imapM (\i → runAi (coordLin m i)) >>=
          (w_map.wm_data .=)


castVisibilityRay' ∷ (IsSeeThrough a) ⇒ WorldMap a b → V2 Int → V2 Int → [(V2 Int, Bool)]
castVisibilityRay' m o d = let seeThrough = isSeeThrough . (`objectAt` m)
                           in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ line o d
                           --in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d



runWorld ∷ WorldM a b c GameState → World a b c → (GameState, World a b c)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject_ ∷ (Applicative w, WorldAPI a b c w) ⇒ V2 Int → a → w ()
changeObject_ v o = changeObject v (const (pure o))


interact ∷ (Applicative w, WorldAPI a b c w) ⇒ (V2 Int → a → w ()) → w ()
interact f = interactOrElse f (pure ())

