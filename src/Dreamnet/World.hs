{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}

module Dreamnet.World
( module Dreamnet.Engine.WorldMap
, module Dreamnet.Engine.ObjectAPI

, WorldBase, wb_contents, mkBase, TeamMember, tm_memberPosition
, WorldCell, wc_contents, emptyCell, mkCell, predIfJust

, playerObject, teamObjects, collectNonNothings

, World, w_turn, w_player, w_team, w_map, w_vis, newWorld

, WorldM, runWorld, evalWorld, execWorld

, increaseTurn, movePlayer, changePlayer, joinTeam, moveObject, setVisibility
, runObjectMonadForAI

, cellAtM, modifyCellM, checkBoundsM, collectNonNothingsM
) where


import Prelude hiding (interact, rem, map)
import Safe           (fromJustNote)

import Control.Lens               (makeLenses, (%=), (.=), (+=), use, uses, view,
                                   views, (.~), _Just, (^?), preview, (^.))
import Control.Monad              (when, (>=>), join, foldM, void)
import Control.Monad.Trans        (lift)
import Control.Monad.Except       (ExceptT(ExceptT))
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT), runMaybeT, exceptToMaybeT)
import Control.Monad.Free         (Free(..))
import Control.Monad.State.Strict (MonadState, State, runState, evalState,
                                   execState)
import Linear                     (V2(V2), V3(V3), _x, _y, _z)
import Data.Foldable              (traverse_, for_)
import Data.List                  (elemIndex)
import Data.Maybe                 (fromMaybe, catMaybes)

import qualified Data.Vector as V (Vector, imap, replicate, head)
import qualified Data.Set    as S (Set, member, map)

import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Object
import Dreamnet.Engine.ObjectAPI
import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Utils       (maybeToEither)

import qualified Dreamnet.Engine.Visibility as Vis

import Dreamnet.ObjectStates

--------------------------------------------------------------------------------

type SpawnLocation = Bool


newtype WorldBase = WorldBase { _wb_contents ∷ (Symbol, SpawnLocation) }
makeLenses ''WorldBase


mkBase ∷ Symbol → SpawnLocation → WorldBase
mkBase s = WorldBase . (,) s

--------------------------------------------------------------------------------

newtype TeamMember = TeamMember { _tm_memberPosition ∷ Safe (V3 Int) }
makeLenses ''TeamMember

--------------------------------------------------------------------------------

newtype WorldCell = WorldCell { _wc_contents ∷ Maybe (Object States) }
                  deriving(Eq)
makeLenses ''WorldCell

instance Vis.VisibleAPI WorldCell where
    isSeeThrough (WorldCell (Just o)) = Vis.isSeeThrough o
    isSeeThrough _                    = True

    height (WorldCell (Just o)) = Vis.height o
    height _                    = 0 -- when empty (nothing) is height zero or one?


emptyCell ∷ WorldCell
emptyCell = WorldCell Nothing


mkCell ∷ Object States → WorldCell
mkCell = WorldCell . Just


predIfJust ∷ (Object States → Bool) → (WorldCell → Bool)
predIfJust f = maybe False f . view wc_contents

--------------------------------------------------------------------------------

-- | Type variables
--   v: visibility data
--   c: character data
--
--   TODO because of various lookups, this might need to contain many more fields
data World = World {
      _w_turn   ∷ Int
    , _w_player ∷ Safe (V3 Int)
    , _w_team   ∷ [TeamMember]
    , _w_map    ∷ WorldMap WorldBase WorldCell
    , _w_vis    ∷ V.Vector Vis.Visibility
    }
makeLenses ''World


instance CoordVector World where
    width  = views w_map width
    height = views w_map height


newWorld ∷ WorldMap WorldBase WorldCell → Object States → World
newWorld wm p =
    let ppos = views wm_spawns V.head wm 
        map = execWorldMap (modifyCell ppos (const (mkCell p))) wm
    in  World {
          _w_turn   = 0
        , _w_player = ppos
        , _w_team   = []
        , _w_map    = map
        , _w_vis    = V.replicate (fromIntegral $ squared <$> width <*> height $ map) mempty
        }


playerObject ∷ World → Object States
playerObject w =
    let mPlayer = views w_player (_wc_contents . cellAt (view w_map w)) w
    in  fromJustNote "Error retrieving player data, bad code!" mPlayer


teamObjects ∷ World → [Object States]
teamObjects w = teamObject <$> view w_team w
    where
        teamObject (TeamMember tp) =
            fromJustNote
                "Team member referenced, but does not exist in the map at that position!"
                (_wc_contents $ cellAt (view w_map w) tp)


collectNonNothings ∷ WorldMap b WorldCell → Safe (V3 Int) → [(Safe (V3 Int), Object States)] → [(Safe (V3 Int), Object States)]
collectNonNothings wm p l = maybe l collect $ _wc_contents (cellAt wm p)
    where
        collect = (:l) . (p,)

--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM a = WorldM { runWorldM ∷ State World a }
                 deriving (Functor, Applicative, Monad, MonadState World)


runWorld ∷ WorldM a → World → (a, World)
runWorld wm = runState (runWorldM wm)


evalWorld ∷ WorldM a → World → a
evalWorld wm = evalState (runWorldM wm)


execWorld ∷ WorldM a → World → World
execWorld wm = execState (runWorldM wm)

--------------------------------------------------------------------------------
-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!


cellAtM ∷ Safe (V3 Int) → WorldM WorldCell
cellAtM p = uses w_map (`cellAt` p)


modifyCellM ∷ Safe (V3 Int) → (WorldCell → WorldCell) → WorldM () 
modifyCellM p f = w_map %= execWorldMap (modifyCell p f)


checkBoundsM ∷ V3 Int → WorldM (Either OobError (Safe (V3 Int)))
checkBoundsM v = uses w_map (`checkBounds` v)


collectNonNothingsM ∷ [(Safe (V3 Int), Object States)] → Safe (V3 Int) → WorldM [(Safe (V3 Int), Object States)]
collectNonNothingsM l p = uses w_map $ \wm → collectNonNothings wm p l

--------------------------------------------------------------------------------

increaseTurn ∷ WorldM ()
increaseTurn = w_turn += 1


--movePlayer ∷ V3 Int → WorldM ()
movePlayer ∷ Direction → WorldM ()
movePlayer (dirToVec → v) = use w_player >>= \pp → 
    cellAtM pp >>= \o →
        checkBoundsM (unpack pp + V3 (v ^. _x) (v ^. _y) (unpack pp ^. _z)) >>= \case 
            Right np → do
                moveObject pp np
                w_player .= np
            Left e → error (show e)
    {-
    cellAtM pp >>= \o →
        void $ runMaybeT do
            np   ← exceptToMaybeT $ ExceptT (checkBoundsM (unpack pp + unpack v))
            tv   ← lift (cellAtM np)
            pass ← MaybeT $ pure (preview (wc_contents._Just.o_passable) tv)
            when pass $ lift do
                -- TODO replace with height management
                moveObject pp np
                w_player .= np
                -}


changePlayer ∷ (WorldCell → WorldCell) → WorldM ()
changePlayer f = use w_player >>= \pp → modifyCellM pp f


joinTeam ∷ Object States → WorldM ()
joinTeam _ = pure ()
    --uses w_team (++[o])


-- TODO crashes if np is out of map bounds!!!
moveObject ∷ Safe (V3 Int) → Safe (V3 Int) → WorldM ()
moveObject cp np = do
    o ← cellAtM cp
    modifyCellM np (const o)
    modifyCellM cp (const emptyCell)


-- TODO Nuke when updateVisible disappears
setVisibility ∷ S.Set (V2 Int) → WorldM ()
setVisibility _ = pure ()
{-
setVisibility xs = do
    m ← use w_map
    let xs' = S.map (linCoord m) xs
    w_vis %= V.imap (\i x → if i `S.member` xs'
                              then Visible
                              else case x of
                                Visible → Known
                                _       → x)
                                -}


runObjectMonadForAI ∷ (Safe (V3 Int), Object States) → Free (ObjectF (Object States)) a → WorldM a
runObjectMonadForAI (cp, o) (Free (Position fv)) =
    runObjectMonadForAI (cp, o) (fv cp)
runObjectMonadForAI (cp, o) (Free (Move v n)) =
    moveObject cp v *>
        runObjectMonadForAI (v, o) n
runObjectMonadForAI (cp, o) (Free (Passable fn)) =
    runObjectMonadForAI (cp, o) (fn $ view o_passable o)
runObjectMonadForAI (cp, o) (Free (SetPassable cl n)) =
    let no = o_passable .~ cl $ o
    in  modifyCellM cp (const (mkCell no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (SeeThrough fn)) =
    runObjectMonadForAI (cp, o) (fn $ view o_seeThrough o)
runObjectMonadForAI (cp, o) (Free (SetSeeThrough st n)) =
    let no = o_seeThrough .~ st $ o
    in  modifyCellM cp (const (mkCell no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (CanSee v fs)) =
    uses w_map (\wm → castRay wm cp v) >>= -- TODO add height!
        runObjectMonadForAI (cp, o) . fs . and . fmap (snd . unpack)
runObjectMonadForAI (cp, o) (Free (ChangeSymbol c n)) =
    let no = o_symbol .~ c $ o
    in  modifyCellM cp (const (mkCell no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (ChangeMat m n)) =
    let no = o_material .~ m $ o
    in  modifyCellM cp (const (mkCell no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (Message _ n)) =
    -- TODO repair, use some kind of "action" on NPC that marks what are they doing ATM
    --      one action could be "talking" and this could render a small speech bubble
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (DoTalk _ n)) =
    -- TODO repair, use some kind of "action" on NPC that marks what are they doing ATM
    --      one action could be "talking" and this could render a small speech bubble
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (OperateComputer n)) =
    --setStatus ("Computer " <> show o <> " is being operated.") *>
    -- TODO repair NPC operating a computer
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (ScanRange r f fn)) =
    uses w_map (\wm → interestingObjects wm cp r (predIfJust f)) >>= 
        foldM collectNonNothingsM [] >>=
            runObjectMonadForAI (cp, o) . fn
runObjectMonadForAI (cp, o) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → use w_player >>= runObjectMonadForAI (cp, o) . fn
        LineOfSight → use w_player >>= runObjectMonadForAI (cp, o) . fn
runObjectMonadForAI (cp, o) (Free (SpawnNewObject v s n)) = do
    modifyCellM v (const (mkCell s))
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (RemoveObject v n)) = do
    modifyCellM v (const emptyCell)
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (FindObject s fn)) = do
    xs ← use w_player >>= \pp →
            uses w_map (\wm → interestingObjects wm pp 60 (predIfJust (s==)))
    case xs of
        []    → runObjectMonadForAI (cp, o) (fn Nothing)
        (v:_) → runObjectMonadForAI (cp, o) (fn (Just v))
            --cellvs ← cellAt v
            --let mi = s `elemIndex` cellvs
            --let r = (v,) <$> mi
            
runObjectMonadForAI _ (Pure x) =
    pure x


