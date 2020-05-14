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
{-# LANGUAGE RankNTypes #-}

module Dreamnet.World
( module Dreamnet.Engine.WorldMap
, module Dreamnet.Engine.ObjectAPI

, TeamMember, tmMemberPosition

, playerObject, teamObjects

, World(..), wTurn, wPlayer, wTeam, wMap, wVis, newWorld

, WorldM, runWorld, evalWorld, execWorld, doMap

, increaseTurn, movePlayer, changePlayer, joinTeam, setVisibility
, runObjectMonadForAI
) where


import Prelude hiding (interact, rem, map)
import Safe           (fromJustNote)

import Control.Lens               (makeLenses, use, uses, view, views, _Just)
import Control.Lens.Operators
import Control.Monad              (void)
import Control.Monad.Free         (Free(..))
import Control.Monad.State.Strict (MonadState, State, runState, evalState,
                                   execState)
import Linear                     (V2, V3, _z, _xy)

import qualified Data.Vector as V (Vector, replicate, head)
import qualified Data.Set    as S (Set)

import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Object
import Dreamnet.Engine.ObjectAPI
import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Visibility

import Dreamnet.ObjectStates

--------------------------------------------------------------------------------

newtype TeamMember = TeamMember { _tmMemberPosition ∷ Safe (V3 Int) }
makeLenses ''TeamMember

--------------------------------------------------------------------------------

-- | Type variables
--   v: visibility data
--   c: character data
--
--   TODO because of various lookups, this might need to contain many more fields
data World = World {
      _wTurn   ∷ Int
    , _wPlayer ∷ Safe (V3 Int)
    , _wTeam   ∷ [TeamMember]
    , _wMap    ∷ WorldMap Symbol (Object States)
    , _wVis    ∷ V.Vector Visibility
    }
makeLenses ''World


instance CoordVector World where
    width  = views wMap width
    height = views wMap height


newWorld ∷ WorldMap Symbol (Object States) → Object States → World
newWorld wm p =
    World {
      _wTurn   = 0
    , _wPlayer = ppos
    , _wTeam   = []
    , _wMap    = execWorldMap spawnPlayer wm
    --, _wMap    = execWorldMap (spawnPlayer *> setupLinks) wm
    , _wVis    = V.replicate (fromIntegral $ squared <$> width <*> height $ wm) mempty
    }
    where
        ppos        = views wmSpawns V.head wm 
        spawnPlayer = spawnObject ppos p
        {-
        setupLinks  = do
            wm ← get
            w  ← width  <$> get
            h  ← height <$> get
            d  ← depth  <$> get
            forM_ [0..w] $ \x →
                forM_ [0..h] $ \y → do
                    let sv     = clipToBounds wm (V3 x y 0)
                        objCol = dropWhile (==EmptyCell) (column wm sv)
                    objCol  
                    --case objCol of
                    --    []  → pure ()
                    --    [x] → pure ()
                    --    ls  → zip (repeat (head ls)) (drop 1 (column wm sv))
                    -}


playerObject ∷ World → Object States
playerObject w =
    let map     = view wMap w
        mPlayer = w ^? wPlayer.cellAtL map.followLinksL map._Just
    in  fromJustNote "Error retrieving player data, bad code!" mPlayer


teamObjects ∷ World → [Object States]
teamObjects w = teamObject <$> view wTeam w
    where
        teamObject (TeamMember tp) =
            let wm = view wMap w
            in  fromJustNote
                    "Team member referenced, but does not exist in the map at that position!"
                    (objectAt wm tp)


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


doMap ∷ WorldMapM Symbol (Object States) a → WorldM a
doMap prg = do
    (r, nm) ← uses wMap (runWorldMap prg)
    wMap .= nm
    pure r

--------------------------------------------------------------------------------

increaseTurn ∷ WorldM ()
increaseTurn = wTurn += 1


movePlayer ∷ Direction → WorldM ()
movePlayer d = do
    pp ← use wPlayer
    --checkBoundsM (unpack pp + d `sameLevelAs` unpack pp) >>= \case
    -- TODO when moving few levels down, make noise?
    let np = _z .~ 0 $ unpack pp + sameLevelAs d (unpack pp)
    tryMove pp np >>= \case
        False → void $ tryMove pp (_z +~ 1 $ np) -- On top of...
        True  → pure ()
    where
        tryMove pp v =
            uses wMap (`checkBounds` v) >>= \case
                Left _ → pure False
                Right np → do
                    canWalk ← uses wMap (`passableOrEmptyCell` np)
                    if canWalk
                        then do
                            doMap (moveObject pp np)
                            wPlayer .= np
                            pure True
                        -- TODO repeat the move part of code for the cell on top, and if it doesn'twork, bail!
                        else
                            pure False
        passableOrEmptyCell = isEmptyAt
        sameLevelAs d'      = _xy .~ view _xy (dirToVec d')


changePlayer ∷ (DreamnetCharacter → DreamnetCharacter) → WorldM ()
--changePlayer ∷ (Object States → Object States) → WorldM ()
changePlayer f = use wPlayer >>= doMap . (`modifyObject` (oState._Person %~ f))


joinTeam ∷ Object States → WorldM ()
joinTeam _ = pure ()
    --uses wTeam (++[o])


-- TODO Nuke when updateVisible disappears
setVisibility ∷ S.Set (V2 Int) → WorldM ()
setVisibility _ = pure ()
{-
setVisibility xs = do
    m ← use wMap
    let xs' = S.map (linCoord m) xs
    wVis %= V.imap (\i x → if i `S.member` xs'
                              then Visible
                              else case x of
                                Visible → Known
                                _       → x)
                                -}


runObjectMonadForAI ∷ (Safe (V3 Int), Object States) → Free (ObjectF (Object States)) a → WorldM a
runObjectMonadForAI (cp, o) (Free (Position fv)) =
    runObjectMonadForAI (cp, o) (fv cp)
runObjectMonadForAI (cp, o) (Free (Move v n)) =
    doMap (moveObject cp v) *>
        runObjectMonadForAI (v, o) n
runObjectMonadForAI (cp, o) (Free (Passable fn)) =
    runObjectMonadForAI (cp, o) (fn $ view oPassable o)
runObjectMonadForAI (cp, o) (Free (SetPassable cl n)) =
    let no = oPassable .~ cl $ o
    in  doMap (modifyObject cp (const no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (SeeThrough fn)) =
    runObjectMonadForAI (cp, o) (fn $ view oSeeThrough o)
runObjectMonadForAI (cp, o) (Free (SetSeeThrough st n)) =
    let no = oSeeThrough .~ st $ o
    in  doMap (modifyObject cp (const no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (CanSee v fs)) =
    uses wMap (\m → castRay m cp v) >>= -- TODO add height!
        runObjectMonadForAI (cp, o) . fs . and . fmap (snd . unpack)
runObjectMonadForAI (cp, o) (Free (ChangeSymbol c n)) =
    let no = oSymbol .~ c $ o
    in  doMap (modifyObject cp (const no)) *>
            runObjectMonadForAI (cp, no) n
runObjectMonadForAI (cp, o) (Free (ChangeMat m n)) =
    let no = oMaterial .~ m $ o
    in  doMap (modifyObject cp (const no)) *>
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
    wm ← use wMap
    let obPoses = interestingObjects wm cp r (predIfJust wm f)
    let obs     = foldr (collectNonEmpty wm) [] obPoses
    runObjectMonadForAI (cp, o) (fn obs)
    
runObjectMonadForAI (cp, o) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → use wPlayer >>= runObjectMonadForAI (cp, o) . fn
        LineOfSight → use wPlayer >>= runObjectMonadForAI (cp, o) . fn
runObjectMonadForAI (cp, o) (Free (SpawnNewObject v s n)) = do
    doMap (spawnObject v s)
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (RemoveObject v n)) = do
    doMap (deleteObject v)
    runObjectMonadForAI (cp, o) n
runObjectMonadForAI (cp, o) (Free (FindObject s fn)) = do
    xs ← use wPlayer >>= \pp →
        uses wMap (\wm → interestingObjects wm pp 60 (predIfJust wm (s==)))
    case xs of
        []    → runObjectMonadForAI (cp, o) (fn Nothing)
        (v:_) → runObjectMonadForAI (cp, o) (fn (Just v))
            --cellvs ← cellAt v
            --let mi = s `elemIndex` cellvs
            --let r = (v,) <$> mi
            
runObjectMonadForAI _ (Pure x) =
    pure x


