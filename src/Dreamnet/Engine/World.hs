{-# LANGUAGE UnicodeSyntax, OverloadedStrings, NegativeLiterals, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Dreamnet.Engine.World
( module Dreamnet.Engine.WorldMap

, Symbol(Symbol)

, Object(Object)
, o_symbol
, o_material
, o_passable
, o_seeThrough
, o_height
, o_state

, ObjectAPI(..)

, WorldReadAPI(..)

, WorldAPI(..)
, replaceObject

, World
, newWorld

, WorldM
, runWorld
, evalWorld
, execWorld
) where


import Prelude hiding (interact, rem, map)
import Safe           (fromJustNote)

import Control.Lens               (makeLenses, (%=), (.=), use, uses, view,
                                   views)
import Control.Monad              (when, (>=>), filterM)
import Control.Monad.State.Strict (MonadState, State, runState, evalState, execState)
import Linear                     (V2)
import Data.Monoid                ((<>))
import Data.Bool                  (bool)
import Data.Foldable              (traverse_, for_)

import qualified Data.Map    as M  (Map, empty, keys, lookup)
import qualified Data.Set    as S  (fromList, member)
import qualified Data.Vector as V  (Vector, imap, replicate, head)

import Dreamnet.Engine.Utils
import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Visibility

--------------------------------------------------------------------------------

newtype Symbol = Symbol Char
               deriving (Eq, Show)

instance Monoid Symbol where
    mempty = Symbol ' '
    (Symbol ' ') `mappend` (Symbol ch') = Symbol ch'
    (Symbol ch)  `mappend` (Symbol ' ') = Symbol ch
    _            `mappend` (Symbol ch') = Symbol ch'  -- TODO make correct, add char codes

--------------------------------------------------------------------------------

data Object a = Object {
      _o_symbol      ∷ Symbol
    , _o_material    ∷ String
    , _o_passable    ∷ Bool
    , _o_seeThrough  ∷ Bool
    , _o_height      ∷ Word

    , _o_state ∷ a
    }
    deriving (Eq, Show, Functor)
makeLenses ''Object


instance Applicative Object where
    pure = Object mempty "" False False 0
    (Object s m ps st h f) <*> (Object s' m' ps' st' h' x) =
        Object (s <> s') (m <> m') (ps || ps') (st || st') (h + h') (f x)


instance Monad Object where
    (Object _ _ _ _ _ x)  >>= f = f x


--------------------------------------------------------------------------------
-- Object API and objects

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI a o | o → a where
    position           ∷ o (V2 Int)
    move               ∷ V2 Int → o ()
    passable           ∷ o Bool
    setPassable        ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough         ∷ o Bool
    setSeeThrough      ∷ Bool → o ()
    canSee             ∷ V2 Int → o Bool
    changeSymbol       ∷ Symbol → o ()
    changeMat          ∷ String → o ()
    message            ∷ String → o ()
    scanRange          ∷ Word → (Object a → Bool) → o [(V2 Int, Object a)]
    -- Keep adding primitives until you can describe all Map Objects as programs

--------------------------------------------------------------------------------

-- TODO should probably move out all mentions of player or team out of here
-- into the Game data

--class WorldReadAPI o v w | w → o, w → v where
class (WorldMapReadAPI (Object o) w) ⇒ WorldReadAPI o v w | w → o, w → v where
    currentMap         ∷ w (WorldMap (Object o)) -- TODO not liking this
    visibility         ∷ w (V.Vector v) -- TODO or this
    playerPosition     ∷ w (V2 Int, Int)
    playerObject       ∷ w o
    team               ∷ w [String]
    teamMemberPosition ∷ String → w (Maybe (V2 Int, Int))
    castVisibilityRay  ∷ V2 Int → V2 Int → w [(V2 Int, Bool)]


-- TODO seriously refactor this into much better DSL
--class (WorldReadAPI o v w) ⇒ WorldAPI o v w | w → o, w → v where
class (WorldMapAPI (Object o) w, WorldReadAPI o v w) ⇒ WorldAPI o v w | w → o, w → v where
    status          ∷ w String
    setStatus       ∷ String → w ()
    changeObject    ∷ V2 Int → (Object o → w (Object o)) → w ()
    modifyObjectAt  ∷ V2 Int → Int → (Object o → w (Object o)) → w ()
    movePlayer      ∷ V2 Int → w ()
    changePlayer    ∷ (Object o → Object o) → w ()
    moveObject      ∷ V2 Int → Object o → V2 Int → w ()
    -- TODO redo this, to be a function, and calculate on demand, not prefront
    updateVisible   ∷ w ()
    -- TODO not really happy with 'update*' anything. Provide a primitive!
    --updateAi ∷ w ()


replaceObject ∷ (Eq o, Applicative w, WorldAPI o v w) ⇒ V2 Int → Object o → Object o → w ()
replaceObject v oo no = changeObject v (\c → pure $ if c == oo
                                                      then no
                                                      else c)

--------------------------------------------------------------------------------
 
-- | Type variables
--   v: visibility data
--   c: character data
data World o v = World {
      _w_player ∷ (V2 Int, Int)
    , _w_team   ∷ M.Map String (V2 Int, Int)
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
          _w_player = (ppos, pix)
        , _w_team   = M.empty
        , _w_map    = map
        , _w_vis    = V.replicate (fromIntegral $ (width m) * (height m)) mempty
        , _w_status = ""
        }


castVisibilityRay' ∷ (Monad wm, WorldMapReadAPI (Object o) wm) ⇒ V2 Int → V2 Int → wm [(V2 Int, Bool)]
castVisibilityRay' o d = do
    filterM (fmap not . oob) (bla o d) >>= traverse (\p → (p,) <$> isSeeThrough p)
    --fmap ((,) <$> id <*> isSeeThrough) $ filter (not . outOfBounds m) $ line o d
    where
        isSeeThrough x = and . fmap (view o_seeThrough) <$> cellAt x

--------------------------------------------------------------------------------

-- TODO if I use ST monad, I can get mutable state for cheap
newtype WorldM o v a = WorldM { runWorldM ∷ State (World o v) a }
                     deriving (Functor, Applicative, Monad, MonadState (World o v))


instance WorldMapReadAPI (Object o) (WorldM o v) where
    desc = uses w_map (evalWorldMap desc)

    cellAt v = uses w_map (evalWorldMap (cellAt v))

    interestingObjects v r ff = uses w_map (evalWorldMap (interestingObjects v r ff))

    oob v = uses w_map (evalWorldMap (oob v))



instance WorldMapAPI (Object o) (WorldM o v) where
    modifyCell v f = w_map %= execWorldMap (modifyCell v f)

    replaceCell v l = w_map %= execWorldMap (replaceCell v l)

    

-- TODO if I somehow replace visibility with ORD and maybe Min/Max, this would make these instances
--      that much more flexible!
instance WorldReadAPI o Visibility (WorldM o Visibility) where
    currentMap = use w_map

    visibility = use w_vis

    playerPosition = use w_player

    playerObject = do
        mpl ← playerPosition >>= \(pp, ix) → cellAt pp >>= pure . valueAt ix
        pure $ view o_state $ fromJustNote "Error retrieving player data, bad code!" mpl

    team = uses w_team M.keys

    teamMemberPosition n = uses w_team (M.lookup n)

    castVisibilityRay o d = uses w_map (evalWorldMap (castVisibilityRay' o d))



instance (Eq o) ⇒ WorldAPI o Visibility (WorldM o Visibility) where
    status = use w_status

    setStatus s = w_status .= s

    changeObject v fo = do
        nc ← cellAt v >>= traverse fo 
        replaceCell v nc

    modifyObjectAt v ix f =
        cellAt v >>=
        traverse_ (f >=> \no → modifyCell v (replaceInCell ix no)) . valueAt ix -- traversal over Maybe

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
        cellAt pp >>= \c → for_ (valueAt ix c) $ \o → do
            replaceObject pp o (f o)

    -- TODO crashes if np is out of map bounds!!!
    moveObject cp o np = do
        -- TODO addToCell *only* if deleteFromCell is successful,
        --      otherwise it'll just perform a copy!
        modifyCell cp (deleteFromCell o)
        modifyCell np (addToCell o)
 
    updateVisible = do
        t ← pure (:)
            <*> (fst <$> playerPosition)
            <*> (team >>= traverse (fmap (maybe (error "Team member without position!") fst) . teamMemberPosition))

        linPoints ← uses w_map (\m → mconcat $ fmap (pointsForOne m) t)
        -- NOTE resolving 'x' causes lag
        w_vis %= V.imap (\i x → if i `S.member` linPoints
                                  then Visible
                                  else case x of
                                    Visible → Known
                                    _       → x)
        where
            pointsForOne m p =
                let !points    = circle 20 p
                    !los       = concat $ fmap fst . visibleAndOneExtra . (\o → evalWorldMap (castVisibilityRay' p o) m) <$> points
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


evalWorld ∷ WorldM o v a → World o v → a
evalWorld wm = evalState (runWorldM wm)


execWorld ∷ WorldM o v a → World o v → World o v
execWorld wm = execState (runWorldM wm)
