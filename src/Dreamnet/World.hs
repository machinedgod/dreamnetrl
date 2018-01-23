{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dreamnet.World
( module Dreamnet.Character
, MonadWorld

, World
, w_playerPos
, w_playerCharacter
, w_aim
, w_map
, w_status
, newWorld

, WorldM
, runWorld
, objectAt
, changeObject
, changeObject_
, movePlayer
, switchAim
, interact
, interactOrElse
, examine
, get
, updateVisible
) where

import Prelude hiding (interact, rem)
import Safe

import Control.Lens               (makeLenses, (^.), (%=), (+=), (.=), use, uses)
import Control.Monad.State hiding (get)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT)
import Linear                     (V2)
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)

import qualified Data.Set    as S (fromList, member)
import qualified Data.Vector as V ((!?), (//), imap)

import Dreamnet.ObjectProperties
import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Item
import Dreamnet.Character
import Dreamnet.GameState
import Dreamnet.Conversation
import Dreamnet.Visibility

--------------------------------------------------------------------------------

class (MonadState (World a b) w) ⇒ MonadWorld a b w

    
-- TODO add functions that establish pipelines input->update->render

--------------------------------------------------------------------------------
 
data World a b = World {
      _w_playerPos       ∷ V2 Int
    , _w_playerCharacter ∷ Character
    , _w_aim             ∷ Maybe (V2 Int)
    , _w_map             ∷ WorldMap a b
    , _w_status          ∷ String
    }

makeLenses ''World


newWorld ∷ WorldMap a b → World a b
newWorld m = 
    World {
      _w_playerPos       = fromMaybe (error "Map is missing spawn points!") $ (m^.wm_spawns) V.!? 0
    , _w_playerCharacter = newCharacter "Carla" End
    , _w_aim             = Nothing
    , _w_map             = m
    , _w_status          = ""
    }

--------------------------------------------------------------------------------

newtype WorldM a b c = WorldM { runWorldM ∷ State (World a b) c }
                     deriving (Functor, Applicative, Monad, MonadState (World a b))


instance (IsPassable a, Describable a) ⇒ MonadWorld a Visibility (WorldM a Visibility)


runWorld ∷ WorldM a b GameState → World a b → (GameState, World a b)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject ∷ (MonadWorld a b w) ⇒ V2 Int → a → w a
changeObject v o = do
    m        ← use w_map
    let oldo = objectAt v m
    -- Hackage says this is O(m + 1) for a single update :-(
    w_map.wm_data %= (V.// [(linCoord m v, o)])
    return oldo


changeObject_ ∷ (MonadWorld a b w) ⇒ V2 Int → a → w ()
changeObject_ v = void . changeObject v


movePlayer ∷ (IsPassable a, MonadWorld a b w) ⇒ V2 Int → w ()
movePlayer v = do
    npp ← uses w_playerPos (+v)
    obj ← uses w_map (objectAt npp)
    when (isPassable obj) $
        w_playerPos += v


switchAim ∷ (MonadWorld a b w) ⇒ (a → Bool) → w ()
switchAim nof = do
    pp ← use w_playerPos
    os ← uses w_map (interestingObjects pp 2 nof)
    ca ← use w_aim
    case ca of
        Just a → w_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
        _      → w_aim .= headMay os


interact ∷ (MonadWorld a b w) ⇒ (V2 Int → a → w ()) → w ()
interact f = interactOrElse f (return ())


interactOrElse ∷ (MonadWorld a b w) ⇒ (V2 Int → a → w c) → w c → w c
interactOrElse f e = fromMaybe e <=< runMaybeT $ do
    v ← MaybeT (use w_aim)
    o ← uses w_map (objectAt v)
    return (f v o)


examine ∷ (Describable a, MonadWorld a b w) ⇒ w String
examine = interactOrElse (const examineText) (use (w_map.wm_desc))
    where
        examineText = pure . description
        --examineText v o    = (objectDescription o <>) <$> itemsText v
        --itemsText v        = maybe "" itemsDescription <$> uses w_items (M.lookup v)
        --itemsDescription []  = ""
        --itemsDescription [i] = "\nThere's a " <> (toLower <$> i^.i_name) <> " here."
        --itemsDescription l   = "\nThere are " <> itemListToText l <> " here."
        --itemListToText l     = let sl = fmap toLower . view i_name <$> l
        --                       in  intercalate ", " (take (length sl - 1) sl) <> " and " <> last sl


get ∷ (MonadWorld a b w) ⇒ w (Maybe Item)
get = return Nothing
--get = interactOrElse getItem (return Nothing)
--    where
--        getItem v o = runMaybeT $ do
--            i ← MaybeT (uses w_items (M.lookup v >=> headMay))
--            addToCharacterInventory i
--            lift (removeFromWorldPile v i)
--            return i
--        addToCharacterInventory i = w_playerCharacter.ch_inventory %= (i:)

--------------------------------------------------------------------------------

-- TODO redo this, to be a function, and calculate on demand, not prefront
updateVisible ∷ (IsSeeThrough a, MonadWorld a Visibility w) ⇒ w ()
updateVisible = do
    pp ← use w_playerPos
    m  ← use w_map

    let points    = circle 20 pp
        los       = concat $ (fmap fst . visibleAndOneExtra . coordVisible m pp) <$> points
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


coordVisible ∷ (IsSeeThrough a) ⇒ WorldMap a b → V2 Int → V2 Int → [(V2 Int, Bool)]
coordVisible m o d = let seeThrough = isSeeThrough . (`objectAt` m)
                     in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d

