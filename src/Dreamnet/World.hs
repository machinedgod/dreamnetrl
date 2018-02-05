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
, examine
, get

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

import Control.Lens               (makeLenses, (^.), (%=), (+=), (.=), use,
                                   uses, view)
import Control.Monad              (when, (<=<), void)
import Control.Monad.State        (MonadState, State, runState)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Foldable              (traverse_)
import Data.Semigroup             ((<>))
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)
import Data.List                  (delete)

import qualified Data.Set            as S  (fromList, member)
import qualified Data.Vector         as V  ((!?), modify, imap, imapM_)
import qualified Data.Vector.Mutable as MV (write, read)

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
    moveObject ∷ (Eq a) ⇒ V2 Int → a → V2 Int → w ()
    switchAim ∷ Maybe (a → Bool) → w ()
    interactOrElse ∷ (V2 Int → [a] → w d) → w d → w d
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateVisible ∷ (IsSeeThrough a) ⇒ w ()
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

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM a b c d = WorldM { runWorldM ∷ State (World a b c) d }
                       deriving (Functor, Applicative, Monad, MonadState (World a b c))

instance WorldReadAPI a Visibility b (WorldM a Visibility b) where
    worldMap = use w_map
    playerPos = use w_playerPos
    castVisibilityRay o d = (\m → castVisibilityRay' m o d) <$> use w_map

instance (IsPassable a, Describable a) ⇒ WorldAPI a Visibility b (WorldM a Visibility b) where
    setStatus s = w_status .= s

    changeObject v fo = do
        m   ← use w_map
        nos ← traverse fo (objectsAt v m)
        replaceObjects v nos

    movePlayer v = do
        npp ← uses w_playerPos (+v)
        obj ← uses w_map (objectsAt npp)
        when (allPassable obj) $
            w_playerPos += v

    moveObject cp o np = do
        objs  ← uses w_map (objectsAt cp)
        when (o `elem` objs) $ do
            modifyObjects cp (o `delete`)
            modifyObjects np (<> [o])

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
        v  ← MaybeT (use w_aim)
        os ← uses w_map (objectsAt v)
        return (f v os)

    updateVisible = do
        pp ← playerPos
        m  ← worldMap
        let !points    = circle 20 pp
            !los       = concat $ (fmap fst . visibleAndOneExtra . castVisibilityRay' m pp) <$> points
            !linPoints = S.fromList $ linCoord m <$> los
        -- TODO resolving 'x' causes lag
        w_map.wm_visible %= V.imap (\i x → if i `S.member` linPoints
                                             then Visible
                                             else case x of
                                                 Visible → Known
                                                 _       → x)
        where
            visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
            visibleAndOneExtra l =
                let front = takeWhile ((==True) . snd) l
                    rem   = dropWhile ((==True) . snd) l
                in  bool (head rem : front) front (null rem)

    updateAi = do
        m ← use w_map
        use (w_map.wm_data) >>= V.imapM_ (\i → traverse_ (runAi (coordLin m i)))


modifyObjects ∷ V2 Int → ([a] → [a]) → WorldM a b c ()
modifyObjects v f = do
    m ← use w_map
    -- This one should update in place if its safe to do so
    w_map.wm_data %= V.modify (\vec → do
                                      let i = linCoord m v
                                      os ← MV.read vec i
                                      MV.write vec i (f os))
    -- Hackage says this is O(m + 1) for a single update :-(
    --w_map.wm_data %= (V.// [(linCoord m v, os)])


replaceObjects ∷ V2 Int → [a] → WorldM a b c ()
replaceObjects v os = modifyObjects v (const os)


castVisibilityRay' ∷ (IsSeeThrough a) ⇒ WorldMap a b → V2 Int → V2 Int → [(V2 Int, Bool)]
castVisibilityRay' m o d = let seeThrough = areSeeThrough . (`objectsAt` m)
                           in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ line o d
                           --in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d


runWorld ∷ WorldM a b c GameState → World a b c → (GameState, World a b c)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject_ ∷ (Eq a, Applicative w, WorldAPI a b c w) ⇒ V2 Int → a → a → w ()
changeObject_ v oo no = changeObject v (\c → pure $ if c == oo
                                                      then no
                                                      else c)


interact ∷ (Applicative w, WorldAPI a b c w) ⇒ (V2 Int → [a] → w ()) → w ()
interact f = interactOrElse f (pure ())


examine ∷ (Applicative w, Describable a, WorldAPI a b c w) ⇒ w String
examine = interactOrElse (\_ os → pure $ description (last os)) (view wm_desc <$> worldMap)


get ∷ (Applicative w, WorldAPI a b c w) ⇒ w (Maybe d)
get = pure Nothing

