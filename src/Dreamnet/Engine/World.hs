{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.Engine.World
( module Dreamnet.Engine.WorldMap
, module Dreamnet.Engine.ObjectAPI

, WorldReadAPI(..), WorldAPI(..)

, World, newWorld

, WorldM, runWorld, evalWorld, execWorld
) where


import Prelude hiding (interact, rem, map)
import Safe           (fromJustNote)

import Control.Lens               (makeLenses, (%=), (.=), (+=), use, uses, view,
                                   views, (.~))
import Control.Monad              (when, (>=>))
import Control.Monad.Free         (Free(..))
import Control.Monad.State.Strict (MonadState, State, runState, evalState, execState)
import Linear                     (V2(V2))
import Data.Foldable              (traverse_, for_)
import Data.List                  (elemIndex)
import Data.Maybe                 (fromMaybe)

import qualified Data.Vector as V (Vector, imap, replicate, head)
import qualified Data.Set    as S (Set, member, map)

import Dreamnet.Engine.Object
import Dreamnet.Engine.ObjectAPI
import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Visibility hiding (height)

--------------------------------------------------------------------------------

class (WorldMapReadAPI w) ⇒ WorldReadAPI w where
    type WorldObject w      ∷ *
    type VisibilityObject w ∷ *

    currentTurn       ∷ w Int
    currentMap        ∷ w (WorldMap (WorldObject w)) -- TODO not liking this
    visibility        ∷ w (V.Vector (VisibilityObject w)) -- TODO or this
    playerPosition    ∷ w (V2 Int, Int)
    playerObject      ∷ w (WorldObject w)
    teamPositions     ∷ w [(V2 Int, Int)]
    teamObjects       ∷ w [WorldObject w]


class (WorldMapAPI w, WorldReadAPI w) ⇒ WorldAPI w where
    increaseTurn        ∷ w ()
    status              ∷ w String
    -- TODO consider nuking this and using rendering of the state to display info
    setStatus           ∷ String → w ()
    changeObject        ∷ V2 Int → (WorldObject w → w (WorldObject w)) → w ()
    modifyObjectAt      ∷ V2 Int → Int → (WorldObject w → w (WorldObject w)) → w ()
    replaceObject       ∷ V2 Int → WorldObject w → WorldObject w → w ()
    movePlayer          ∷ V2 Int → w ()
    changePlayer        ∷ (WorldObject w → WorldObject w) → w ()
    --joinParty         ∷ w ()
    joinTeam            ∷ WorldObject w → w ()
    moveObject          ∷ V2 Int → WorldObject w → V2 Int → w ()
    -- TODO Nuke when updateVisible disappears
    setVisibility       ∷ S.Set (V2 Int) → w ()
    runObjectMonadForAI ∷ (V2 Int, Int, WorldMapObject w) → Free (ObjectF (WorldMapObject w)) a → w a


--------------------------------------------------------------------------------

-- | Type variables
--   v: visibility data
--   c: character data
--
--   TODO because of various lookups, this might need to contain many more fields
data World o v = World {
      _w_turn   ∷ Int
    , _w_player ∷ (V2 Int, Int)
    , _w_team   ∷ [(V2 Int, Int)]
    , _w_map    ∷ WorldMap (Object o)
    , _w_vis    ∷ V.Vector v
    , _w_status ∷ String
    }
makeLenses ''World


newWorld ∷ (Monoid v) ⇒ WorldMap (Object o) → Object o → World o v
newWorld m p =
    let ppos       = views wm_spawns V.head m
        (pix, map) = flip runWorldMap m $ do
                        modifyCell ppos (addToCell p)
                        subtract 1 . length <$> cellAt ppos
    in  World {
          _w_turn   = 0
        , _w_player = (ppos, pix)
        , _w_team   = []
        , _w_map    = map
        , _w_vis    = V.replicate (fromIntegral $ width m * height m) mempty
        , _w_status = ""
        }

--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM o v a = WorldM { runWorldM ∷ State (World o v) a }
                     deriving (Functor, Applicative, Monad, MonadState (World o v))


instance WorldMapReadAPI (WorldM o v) where
    type WorldMapObject (WorldM o v) = Object o

    desc = uses w_map (evalWorldMap desc)

    cellAt v = uses w_map (evalWorldMap (cellAt v))

    interestingObjects v r ff = uses w_map (evalWorldMap (interestingObjects v r ff))

    oob v = uses w_map (evalWorldMap (oob v))

    castRay s sh t th = uses w_map (evalWorldMap (castRay s sh t th))



instance WorldMapAPI (WorldM o v) where
    modifyCell v f = w_map %= execWorldMap (modifyCell v f)

    replaceCell v l = w_map %= execWorldMap (replaceCell v l)


-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
instance WorldReadAPI (WorldM o Visibility) where
    type WorldObject (WorldM o Visibility) = Object o
    type VisibilityObject (WorldM o Visibility) = Visibility

    currentTurn = use w_turn

    currentMap = use w_map

    visibility = use w_vis

    playerPosition = use w_player

    playerObject = do
        mpl ← playerPosition >>= \tp → fmap (valueAt (snd tp)) (cellAt (fst tp))
        pure $ fromJustNote "Error retrieving player data, bad code!" mpl
        --pure $ view o_state $ fromJustNote "Error retrieving player data, bad code!" mpl

    teamPositions = use w_team

    teamObjects = use w_team >>=
        traverse (\tp → fromMaybe
            (error "Team member referenced, but does not exist in the map at that position!") . valueAt (snd tp) <$> cellAt (fst tp))
            --(error "Team member referenced, but does not exist in the map at that position!") (view o_state) . valueAt (snd tp) <$> cellAt (fst tp))


instance (Eq o, Show o) ⇒ WorldAPI (WorldM o Visibility) where
    increaseTurn = w_turn += 1

    status = use w_status

    setStatus s = w_status .= s

    changeObject v fo = do
        nc ← cellAt v >>= traverse fo
        replaceCell v nc

    modifyObjectAt v ix f =
        cellAt v >>=
        traverse_ (f >=> \no → modifyCell v (replaceInCell ix no)) . valueAt ix -- traversal over Maybe

    replaceObject v oo no = changeObject v (\c → pure $ if c == oo
                                                          then no
                                                          else c)

    movePlayer v = do
        (pp, ix) ← playerPosition
        cellAt pp >>= \c → for_ (valueAt ix c) $ \o → do
            tv ← cellAt (pp + v)
            when (and (view o_passable <$> tv)) $ do -- TODO replace with height management
                moveObject pp o (pp + v)
                nix ← subtract 1 . length <$> cellAt (pp + v)
                w_player .= (pp + v, nix)

    changePlayer f = do
        (pp, ix) ← playerPosition
        cellAt pp >>= \c → for_ (valueAt ix c) $ \o →
            replaceObject pp o (f o)

    joinTeam _ = pure ()
        --uses w_team (++[o])

    -- TODO crashes if np is out of map bounds!!!
    moveObject cp o np = do
        -- TODO addToCell *only* if deleteFromCell is successful,
        --      otherwise it'll just perform a copy!
        modifyCell cp (deleteFromCell o)
        modifyCell np (addToCell o)

    setVisibility xs = do
        m ← currentMap
        let xs' = S.map (linCoord m) xs
        w_vis %= V.imap (\i x → if i `S.member` xs'
                                  then Visible
                                  else case x of
                                    Visible → Known
                                    _       → x)

    runObjectMonadForAI (cv, h, o) (Free (Position fv)) =
        runObjectMonadForAI (cv, h, o) (fv (cv, h))
    runObjectMonadForAI (cv, h, o) (Free (Move v n)) =
        moveObject cv o v *>
            runObjectMonadForAI (v, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (Passable fn)) =
        runObjectMonadForAI (cv, h, o) (fn $ view o_passable o)
    runObjectMonadForAI (cv, h, o) (Free (SetPassable cl n)) =
        let no = o_passable .~ cl $ o
        in  replaceObject cv o no *>
                runObjectMonadForAI (cv, h, no) n
    runObjectMonadForAI (cv, h, o) (Free (SeeThrough fn)) =
        runObjectMonadForAI (cv, h, o) (fn $ view o_seeThrough o)
    runObjectMonadForAI (cv, h, o) (Free (SetSeeThrough st n)) =
        let no = o_seeThrough .~ st $ o
        in  replaceObject cv o no *>
                runObjectMonadForAI (cv, h, no) n
    runObjectMonadForAI (cv, h, o) (Free (CanSee v fs)) =
        castRay cv h v 0 >>= -- TODO add height!
            runObjectMonadForAI (cv, h, o) . fs . and . fmap snd
    runObjectMonadForAI (cv, h, o) (Free (ChangeSymbol c n)) =
        let no = o_symbol .~ c $ o
        in  replaceObject cv o no *>
                runObjectMonadForAI (cv, h, no) n
    runObjectMonadForAI (cv, h, o) (Free (ChangeMat m n)) =
        let no = o_material .~ m $ o
        in  replaceObject cv o no *>
                runObjectMonadForAI (cv, h, no) n
    runObjectMonadForAI (cv, h, o) (Free (Message m n)) =
        setStatus m *>
            runObjectMonadForAI (cv, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (DoTalk _ n)) =
        setStatus ("NPC " <> show o <> " is talking.") *>
            runObjectMonadForAI (cv, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (OperateComputer n)) =
        setStatus ("Computer " <> show o <> " is being operated.") *>
            runObjectMonadForAI (cv, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (ScanRange r f fn)) = do
        points ← interestingObjects cv r f
        values ← foldr onlyJust [] <$> traverse (fmap lastValue . cellAt) points
        runObjectMonadForAI (cv, h, o) (fn (zip points values))
        where
            onlyJust (Just x) l = x : l
            onlyJust Nothing  l = l
    runObjectMonadForAI (cv, h, o) (Free (AcquireTarget s fn)) =
        case s of
            Freeform    → runObjectMonadForAI (cv, h, o) (fn (V2 0 0))
            LineOfSight → runObjectMonadForAI (cv, h, o) (fn (V2 1 1))
    runObjectMonadForAI (cv, h, o) (Free (SpawnNewObject v s n)) = do
        modifyCell v (addToCell s)
        runObjectMonadForAI (cv, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (RemoveObject v i n)) = do
        x ← fromJustNote "RemoveObject runObjectMonadForAI" . valueAt i <$> cellAt v
        modifyCell v (deleteFromCell x)
        runObjectMonadForAI (cv, h, o) n
    runObjectMonadForAI (cv, h, o) (Free (FindObject s fn)) = do
        pp ← fst <$> playerPosition
        xs ← interestingObjects pp 60 (s==)
        if null xs
            then runObjectMonadForAI (cv, h, o) (fn Nothing)
            else do
                let v = head xs
                cellvs ← cellValues <$> cellAt v
                let mi = s `elemIndex` cellvs
                let r = (v,) <$> mi
                runObjectMonadForAI (cv, h, o) (fn r)
    runObjectMonadForAI _ (Pure x) =
        pure x



runWorld ∷ WorldM o v a → World o v → (a, World o v)
runWorld wm = runState (runWorldM wm)


evalWorld ∷ WorldM o v a → World o v → a
evalWorld wm = evalState (runWorldM wm)


execWorld ∷ WorldM o v a → World o v → World o v
execWorld wm = execState (runWorldM wm)

