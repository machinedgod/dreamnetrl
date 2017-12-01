{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.World
( module Dreamnet.Character
, MonadWorld(..)

, Visibility(..)

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

, objectInteraction


) where

import Prelude hiding (interact)
import Safe

import Control.Lens
import Control.Monad.State hiding (get)
import Control.Monad.Trans.Maybe
import Linear
import Data.Bool
import Data.Maybe (isJust, fromMaybe)
import Data.List  (intercalate, unfoldr)
import Data.Char  (toLower)

import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V

import Dreamnet.Utils
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Input
import Dreamnet.Item
import Dreamnet.Character
import Dreamnet.GameState
import Dreamnet.Conversation

--------------------------------------------------------------------------------

class (MonadState World u) ⇒ MonadWorld u

--------------------------------------------------------------------------------
 
data World = World {
      _w_playerPos       ∷ V2 Int
    , _w_playerCharacter ∷ Character
    , _w_aim             ∷ Maybe (V2 Int)
    , _w_map             ∷ WorldMap
    , _w_status          ∷ String
    }

makeLenses ''World


newWorld ∷ WorldMap → World
newWorld m = 
    World {
      _w_playerPos       = fromMaybe (error "Map is missing spawn points!") $ (m^.wm_spawns) V.!? 0
    , _w_playerCharacter = newCharacter "Carla" End
    , _w_aim             = Nothing
    , _w_map             = m
    , _w_status          = ""
    }

--------------------------------------------------------------------------------

newtype WorldM a = WorldM { runWorldM ∷ State World a }
                 deriving (Functor, Applicative, Monad, MonadState World)

instance MonadWorld WorldM


runWorld ∷ WorldM GameState → World → (GameState, World)
runWorld wm = runState (runWorldM wm)

--------------------------------------------------------------------------------

changeObject ∷ (MonadWorld w) ⇒ V2 Int → Object → w Object
changeObject v o = do
    m        ← use w_map
    let oldo = objectAt v m
    -- Hackage says this is O(m + 1) for a single update :-(
    w_map.wm_data %= (V.// [(linCoord m v, o)])
    return oldo


changeObject_ ∷ (MonadWorld u) ⇒ V2 Int → Object → u ()
changeObject_ v = void . changeObject v


movePlayer ∷ (MonadWorld u) ⇒ V2 Int → u ()
movePlayer v = do
    npp ← uses w_playerPos (+v)
    obj ← uses w_map (objectAt npp)
    when (isPassable obj) $
        w_playerPos += v


switchAim ∷ (MonadWorld u) ⇒ u ()
switchAim = do
    pp ← use w_playerPos
    os ← uses w_map (interestingObjects pp 2)
    ca ← use w_aim
    case ca of
        Just a → w_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
        _      → w_aim .= headMay os


interact ∷ (MonadWorld w) ⇒ (V2 Int → Object → w ()) → w ()
interact f = interactOrElse f (return ())


interactOrElse ∷ (MonadWorld u) ⇒ (V2 Int → Object → u a) → u a → u a
interactOrElse f e = fromMaybe e <=< runMaybeT $ do
    v ← MaybeT (use w_aim)
    o ← uses w_map (objectAt v)
    return (f v o)


examine ∷ (MonadWorld w) ⇒ w String
examine = interactOrElse examineText (use (w_map.wm_desc))
    where
        examineText v = return . fromMaybe "<no description>" . objectDescription
        --examineText v o    = (objectDescription o ++) <$> itemsText v
        --itemsText v        = maybe "" itemsDescription <$> uses w_items (M.lookup v)
        --itemsDescription []  = ""
        --itemsDescription [i] = "\nThere's a " ++ (toLower <$> i^.i_name) ++ " here."
        --itemsDescription l   = "\nThere are " ++ itemListToText l ++ " here."
        --itemListToText l     = let sl = fmap toLower . view i_name <$> l
        --                       in  intercalate ", " (take (length sl - 1) sl) ++ " and " ++ last sl


get ∷ (MonadWorld w) ⇒ w (Maybe Item)
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
updateVisible ∷ (MonadWorld u) ⇒ u ()
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


coordVisible ∷ WorldMap → V2 Int → V2 Int → [(V2 Int, Bool)]
coordVisible m o d = let seeThrough = isSeeThrough . (`objectAt` m)
                     in  fmap ((,) <$> id <*> seeThrough) $ filter (not . outOfBounds m) $ bla o d

--------------------------------------------------------------------------------

objectInteraction ∷ (MonadWorld u) ⇒ V2 Int → Object → u GameState
objectInteraction v (Door o)     = changeObject_ v (Door (not o)) >> return Normal
objectInteraction _ Computer     = return Interaction 
objectInteraction _ (Person c)   = return (Conversation <$> (view ch_name) <*> (view ch_conversation) $ c)
objectInteraction v (Union o o2) = objectInteraction v o2
objectInteraction _ _            = return Normal

