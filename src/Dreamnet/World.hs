{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Dreamnet.World
( module Dreamnet.Engine.WorldMap
, module Dreamnet.Engine.ObjectAPI

, TeamMember(memberPosition)

, WorldAPI(..), playerObject, teamObjects

, World, w_turn, w_player, w_team, w_map, w_vis, newWorld

, WorldM, runWorld, evalWorld, execWorld

) where


import Prelude hiding (interact, rem, map)
import Safe           (fromJustNote)

import Control.Lens               (makeLenses, (%=), (.=), (+=), use, uses, view,
                                   views, (.~))
import Control.Monad              (when, (>=>))
import Control.Monad.Free         (Free(..))
import Control.Monad.State.Strict (MonadState, State, get, runState, evalState, execState)
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

import Dreamnet.ObjectStates

--------------------------------------------------------------------------------

newtype TeamMember = TeamMember { memberPosition ∷ WorldPosition }

--------------------------------------------------------------------------------

class (WorldMapAPI w) ⇒ WorldAPI w where
    increaseTurn        ∷ w ()
    --status              ∷ w String
    -- TODO consider nuking this and using rendering of the state to display info
    --setStatus           ∷ String → w ()
    changeObject        ∷ V2 Int → (Object States → w (Object States)) → w ()
    modifyObjectAt      ∷ WorldPosition → (Object States → w (Object States)) → w ()
    replaceObject       ∷ V2 Int → Object States → Object States → w ()
    movePlayer          ∷ V2 Int → w ()
    changePlayer        ∷ (Object States → Object States) → w ()
    joinTeam            ∷ Object States → w ()
    moveObject          ∷ V2 Int → Object States → V2 Int → w ()
    -- TODO Nuke when updateVisible disappears
    setVisibility       ∷ S.Set (V2 Int) → w ()
    runObjectMonadForAI ∷ (WorldPosition, WorldMapObject w) → Free (ObjectF (WorldMapObject w)) a → w a


--------------------------------------------------------------------------------

-- | Type variables
--   v: visibility data
--   c: character data
--
--   TODO because of various lookups, this might need to contain many more fields
data World = World {
      _w_turn   ∷ Int
    , _w_player ∷ (V2 Int, Int)
    , _w_team   ∷ [TeamMember]
    , _w_map    ∷ WorldMap (Object States)
    , _w_vis    ∷ V.Vector Visibility
    }
makeLenses ''World


newWorld ∷ WorldMap (Object States) → Object States → World
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
        , _w_vis    = V.replicate (fromIntegral $ squared <$> width <*> height $ m) mempty
        }


playerObject ∷ (WorldMapReadAPI w, Functor w) ⇒ World → w (WorldMapObject w)
playerObject = playerObject . view w_player
    where
        playerObject ps = fromJustNote "Error retrieving player data, bad code!" <$> objectAt ps


teamObjects ∷ (WorldMapReadAPI w, Applicative w) ⇒ World → w [WorldMapObject w]
teamObjects = traverse teamObject . view w_team
    where
        teamObject (TeamMember tp) = fromMaybe (error "Team member referenced, but does not exist in the map at that position!") <$> objectAt tp


--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM a = WorldM { runWorldM ∷ State World a }
                 deriving (Functor, Applicative, Monad, MonadState World)


instance WorldMapReadAPI WorldM where
    type WorldMapObject WorldM = Object States

    desc = uses w_map (evalWorldMap desc)

    cellAt v = uses w_map (evalWorldMap (cellAt v))

    interestingObjects v r ff = uses w_map (evalWorldMap (interestingObjects v r ff))

    oob v = uses w_map (evalWorldMap (oob v))

    castRay s t = uses w_map (evalWorldMap (castRay s t))



instance WorldMapAPI WorldM where
    modifyCell v f = w_map %= execWorldMap (modifyCell v f)

    replaceCell v l = w_map %= execWorldMap (replaceCell v l)



-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!

instance WorldAPI WorldM where
    increaseTurn = w_turn += 1

    changeObject v fo = do
        nc ← cellAt v >>= traverse fo
        replaceCell v nc

    modifyObjectAt (v, ix) f =
        cellAt v >>=
        traverse_ (f >=> \no → modifyCell v (replaceInCell ix no)) . valueAt ix -- traversal over Maybe

    replaceObject v oo no = changeObject v (\c → pure $ if c == oo
                                                          then no
                                                          else c)

    movePlayer v = do
        (pp, ix) ← use w_player
        cellAt pp >>= \c → for_ (valueAt ix c) $ \o → do
            tv ← cellAt (pp + v)
            when (and (view o_passable <$> tv)) $ do -- TODO replace with height management
                moveObject pp o (pp + v)
                nix ← subtract 1 . length <$> cellAt (pp + v)
                w_player .= (pp + v, nix)

    changePlayer f = do
        (pp, ix) ← use w_player
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
        m ← use w_map
        let xs' = S.map (linCoord m) xs
        w_vis %= V.imap (\i x → if i `S.member` xs'
                                  then Visible
                                  else case x of
                                    Visible → Known
                                    _       → x)

    runObjectMonadForAI (cp, o) (Free (Position fv)) =
        runObjectMonadForAI (cp, o) (fv cp)
    runObjectMonadForAI (cp, o) (Free (Move v n)) =
        moveObject (fst cp) o v *>
            runObjectMonadForAI ((v, snd cp), o) n
    runObjectMonadForAI (cp, o) (Free (Passable fn)) =
        runObjectMonadForAI (cp, o) (fn $ view o_passable o)
    runObjectMonadForAI (cp, o) (Free (SetPassable cl n)) =
        let no = o_passable .~ cl $ o
        in  replaceObject (fst cp) o no *>
                runObjectMonadForAI (cp, no) n
    runObjectMonadForAI (cp, o) (Free (SeeThrough fn)) =
        runObjectMonadForAI (cp, o) (fn $ view o_seeThrough o)
    runObjectMonadForAI (cp, o) (Free (SetSeeThrough st n)) =
        let no = o_seeThrough .~ st $ o
        in  replaceObject (fst cp) o no *>
                runObjectMonadForAI (cp, no) n
    runObjectMonadForAI (cp, o) (Free (CanSee v fs)) =
        castRay cp (v, 0) >>= -- TODO add height!
            runObjectMonadForAI (cp, o) . fs . and . fmap snd
    runObjectMonadForAI (cp, o) (Free (ChangeSymbol c n)) =
        let no = o_symbol .~ c $ o
        in  replaceObject (fst cp) o no *>
                runObjectMonadForAI (cp, no) n
    runObjectMonadForAI (cp, o) (Free (ChangeMat m n)) =
        let no = o_material .~ m $ o
        in  replaceObject (fst cp) o no *>
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
    runObjectMonadForAI (cp, o) (Free (ScanRange r f fn)) = do
        points ← interestingObjects (fst cp) r f
        values ← foldr onlyJust [] <$> traverse (fmap lastValue . cellAt) points
        runObjectMonadForAI (cp, o) (fn (zip points values))
        where
            onlyJust (Just x) l = x : l
            onlyJust Nothing  l = l
    runObjectMonadForAI (cp, o) (Free (AcquireTarget s fn)) =
        case s of
            Freeform    → runObjectMonadForAI (cp, o) (fn (V2 0 0))
            LineOfSight → runObjectMonadForAI (cp, o) (fn (V2 1 1))
    runObjectMonadForAI (cp, o) (Free (SpawnNewObject v s n)) = do
        modifyCell v (addToCell s)
        runObjectMonadForAI (cp, o) n
    runObjectMonadForAI (cp, o) (Free (RemoveObject v i n)) = do
        x ← fromJustNote "RemoveObject runObjectMonadForAI" . valueAt i <$> cellAt v
        modifyCell v (deleteFromCell x)
        runObjectMonadForAI (cp, o) n
    runObjectMonadForAI (cp, o) (Free (FindObject s fn)) = do
        pp ← fst <$> use w_player
        xs ← interestingObjects pp 60 (s==)
        if null xs
            then runObjectMonadForAI (cp, o) (fn Nothing)
            else do
                let v = head xs
                cellvs ← cellValues <$> cellAt v
                let mi = s `elemIndex` cellvs
                let r = (v,) <$> mi
                runObjectMonadForAI (cp, o) (fn r)
    runObjectMonadForAI _ (Pure x) =
        pure x



runWorld ∷ WorldM a → World → (a, World)
runWorld wm = runState (runWorldM wm)


evalWorld ∷ WorldM a → World → a
evalWorld wm = evalState (runWorldM wm)


execWorld ∷ WorldM a → World → World
execWorld wm = execState (runWorldM wm)

