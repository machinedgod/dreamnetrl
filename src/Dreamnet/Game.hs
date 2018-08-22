{-# LANGUAGE UnicodeSyntax, LambdaCase, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Dreamnet.Game
( module Dreamnet.Engine.World
, GameAPI(..)

, TargetSelectionType(..)
, DistantTargetSelection(..)
, lineOfSight

, Game
, newGame

, GameM
, runGame
, evalGame
, execGame


) where

import Safe                (fromJustNote)
import Control.Lens        (makeLenses, view, use, uses, assign, (+=), (%=),
                            (.=), (+=), (-=), (.~))
import Control.Lens.Prism  (_Just)
import Control.Monad.Trans (lift)
import Control.Monad.Free  (Free(Free, Pure))
import Control.Monad.State (MonadState, StateT, runStateT, evalStateT, execStateT)
import Data.Monoid         ((<>))
import Data.Maybe          (fromMaybe)
import Data.List           (genericLength, find)
import Data.Foldable       (traverse_)
import Linear              (V2(V2), _x, _y)

import qualified Data.Vector as V (fromList)
import qualified Data.Map    as M (lookup)
import qualified UI.NCurses  as C (Curses, clear, resizeWindow, moveWindow,
                                   drawBorder, Glyph(Glyph), render, Attribute(AttributeColor))


import Dreamnet.Engine.World
import Dreamnet.Engine.Visibility
import qualified Dreamnet.Engine.Input       as Input

import Dreamnet.Rendering.Renderer
import qualified Dreamnet.Rendering.Renderer as R

import Design.DesignAPI
import Design.GameCharacters
import Design.ComputerModel
import Design.Items


--------------------------------------------------------------------------------

-- TODO apply DeGoes principle here: extract everything with a neat api, that
-- then runs and produces WorldAPI state values
-- TODO explore the idea of moving all this data into appropriate Game States
--      that interact with each other and never having any 'overlord-level'
--      data
-- TODO try making World Objects keep ObjectPrograms in them, rather than states
--      then, somehow, programs can keep the state by themselves. Its a monad
--      after all.
data Game = Game {
      _g_turn         ∷ Word -- TODO move turn into World?
    , _g_world        ∷ World States Visibility -- TODO could "States" here be parametric?
    , _g_gameState    ∷ GameState
    , _g_rendererData ∷ RendererEnvironment

    , _g_currentTarget ∷ Maybe (V2 Int)
    }
makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← newRenderEnvironment
    sm  ← loadTileMap (view dd_dev_startingMap dd)
    pure Game {
        _g_turn  = 0
      , _g_world = newWorld
                       (fromTileMap sm  objectFromTile)
                       (playerPerson ("Carla" `characterForName` view dd_characters dd))
      , _g_gameState    = Normal 
      , _g_rendererData = rdf

      , _g_currentTarget = Nothing
    }
    where
        -- 1) This *could* all be just a single thing. Object type really does not matter here.
        -- 2) Actually, it does, because Object carries a specific state, later used by object programs
        objectFromTile ∷ Tile → Object States
        objectFromTile t@(ttype → "Base") =
            let m  = "concrete"
                p  = 1 `readBoolProperty` t
                s  = 2 `readBoolProperty` t
                h  = 0
                st = Empty
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Prop") =
            let m  = 4 `readStringProperty` t
                p  = 2 `readBoolProperty` t
                s  = 3 `readBoolProperty` t
                h  = 5 `readWordProperty` t
                st = Prop (1 `readStringProperty` t)
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Person") = 
            let m  = "blue"
                p  = False
                s  = True
                h  = 3
                st = Person $ characterForName (1 `readStringProperty` t) (view dd_characters dd)
            in  Object (Symbol '@') m p s h st
        objectFromTile (ttype → "Spawn") = -- TODO shitty hardcoding, spawns should probably be generalized somehow!)
            objectFromTile (Tile '.' (V.fromList [ "Base", "True", "True" ]))
        objectFromTile t@(ttype → "Camera") =
            let m  = "green light"
                p  = True
                s  = True
                h  = 1
                st = Camera (Faction $ 1 `readStringProperty` t) 0
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Computer") =
            let m  = "metal"
                p  = False
                s  = True
                h  = 1
                st = Computer (ComputerData "" [])
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Clothes") = 
            let m   = "cloth"
                p   = True
                s   = True
                h   = 0
                cid = 1 `readStringProperty` t
                st  = Clothes $
                        fromMaybe (error $ "WearableItem " <> cid <> " isn't defined!") $
                            M.lookup cid clothesDict
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Weapon") = 
            let m   = "metal"
                p   = True
                s   = True
                h   = 0
                wid = 1 `readStringProperty` t
                st  = Weapon $
                        fromMaybe (error $ "WeaponItem " <> wid <> " isn't defined!") $
                            M.lookup wid weaponsDict
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Ammo") = 
            let m   = "metal"
                p   = True
                s   = True
                h   = 0
                aid = 1 `readStringProperty` t
                st  = Ammo $
                        fromMaybe (error $ "AmmoItem " <> aid <> " isn't defined!") $
                            M.lookup aid ammoDict
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Throwable") = 
            let m   = "metal"
                p   = True
                s   = True
                h   = 0
                tid = 1 `readStringProperty` t
                st  = Throwable $
                        fromMaybe (error $ "ThrowableItem " <> tid <> " isn't defined!") $
                            M.lookup tid throwableDict
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Consumable") = 
            let m   = "red"
                p   = True
                s   = True
                h   = 0
                tid = 1 `readStringProperty` t
                st  = Consumable $
                        fromMaybe (error $ "ConsumableItem " <> tid <> " isn't defined!") $
                            M.lookup tid consumableDict
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t =
            error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object (Symbol '@') "metal" False True 3 . Person

--------------------------------------------------------------------------------

data TargetSelectionType = Adjactened
                         | Distant DistantTargetSelection


data DistantTargetSelection = Range       Word
                            | Filtered    (States → Bool)
                            | Composed    TargetSelectionType TargetSelectionType


lineOfSight ∷ (States → Bool) → TargetSelectionType
lineOfSight isVisibleF = Distant (Filtered isVisibleF)


--------------------------------------------------------------------------------

class GameAPI g where
    currentTurn      ∷ g Word
    increaseTurn     ∷ g ()
    moveCamera       ∷ V2 Int → g ()
    nextEvent        ∷ C.Curses a → g a -- TODO type leak
    gameState        ∷ g GameState
    changeGameState  ∷ (GameState → g GameState) → g GameState
    world            ∷ g (World States Visibility)
    doWorld          ∷ WorldM States Visibility a → g a
    -- TODO change to withTarget to make more functional
    obtainTarget     ∷ TargetSelectionType → g (Maybe (V2 Int, Object States))
    -- TODO offer abort!
    askChoice        ∷ [(Char, String, a)] → g a
    runProgramAsPlayer       ∷ V2 Int → Free (ObjectF States) GameState → g GameState
    doRender         ∷ RendererF a → g a
    doRenderData     ∷ (RendererEnvironment → RendererEnvironment) → g ()
    queryRenderData  ∷ g RendererEnvironment

--------------------------------------------------------------------------------

newtype GameM a = GameM { runGameM ∷ StateT Game C.Curses a }
                deriving (Functor, Applicative, Monad, MonadState Game)
            

instance GameAPI GameM where
    currentTurn = use g_turn

    increaseTurn = g_turn += 1

    moveCamera v = g_rendererData %= R.moveCamera v

    nextEvent = GameM . lift

    gameState = use g_gameState

    changeGameState f = use g_gameState >>= f >>= \g → assign g_gameState g >> pure g

    world = use g_world

    doWorld m =
        uses g_world (runWorld m) >>= \(x, w') →
            g_world .= w' >>
                pure x

    obtainTarget Adjactened = do
        ap ← uses g_world (evalWorld (fst <$> playerPosition))
        doRender $ updateMain $ RenderAction $
            (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) ap
                [ "yku"
                , "h.l"
                , "bjn"
                ]
        t ← GameM (lift Input.nextTargetSelectionEvent)
        uses g_world (evalWorld (lastValue <$> cellAt (ap + t))) >>=
            \case
                Nothing → pure Nothing
                Just x  → pure (Just (ap + t, x))
    
    -- TODO should probably get into some target selection state?
    obtainTarget ts@(Distant _) =
        use g_currentTarget >>= \case 
            Nothing → do
                ap ← uses g_world (evalWorld (fst <$> playerPosition))
                g_currentTarget .= Just ap
                obtainTarget ts
            Just t → do
                redColorId ← use (g_rendererData.rd_styles.s_colorRed)
                doRender $ updateMain $
                    draw' t 'X' [ C.AttributeColor redColorId ]
                
                ev ← GameM (lift Input.nextUiEvent)
                case ev of
                    Input.TabNext      → obtainTarget ts -- TODO select next target 'smart'
                    Input.TabPrevious  → obtainTarget ts -- TODO select next target 'smart'
                    Input.MoveLeft     → g_currentTarget._Just._x -= 1 >> obtainTarget ts
                    Input.MoveDown     → g_currentTarget._Just._y += 1 >> obtainTarget ts
                    Input.MoveUp       → g_currentTarget._Just._y -= 1 >> obtainTarget ts
                    Input.MoveRight    → g_currentTarget._Just._x += 1 >> obtainTarget ts
                    Input.Back         → pure Nothing
                    Input.SelectChoice → do
                        (Just v) ← use g_currentTarget
                        uses g_world (evalWorld (lastValue <$> cellAt v)) >>=
                            \case
                                Nothing → pure Nothing
                                Just x  → pure (Just (v, x))


    askChoice lst = do
        doRender $ updateUi $ RenderAction $ do
            C.clear
            C.resizeWindow (genericLength lst + 4) 30 -- TODO Enough to fit all
            C.moveWindow 10 10 -- TODO Center
            C.drawBorder (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '╭' [])
                         (Just $ C.Glyph '╮' [])
                         (Just $ C.Glyph '╰' [])
                         (Just $ C.Glyph '╯' [])
            traverse_ (\(i, (ch, str, _)) → drawString (2 ∷ Int) (i + 2) (ch : " - " <> str)) $ zip [0..] lst
        t ← GameM (lift $ Input.nextAllowedCharEvent  (fst3 $ unzip3 lst))
        pure $ trd3 $ fromJustNote "Picking up correct choice from askChoice" $ find ((== t) . fst3) lst 
        where
            fst3 (x, _, _) = x
            trd3 (_, _, x) = x

    runProgramAsPlayer v prg = do
        mo ← uses g_world (evalWorld (lastValue <$> cellAt v)) -- TODO not really correct
        case mo of
            Nothing → pure Normal
            Just o  → do
                gs ← runObjectMonadForPlayer (v, o) prg 
                doWorld updateVisible
                pure gs

    doRender r = do
        rd ← use g_rendererData
        GameM $ lift $ do
            x ← runRenderer rd  r
            C.render
            pure x

    doRenderData f = g_rendererData %= f

    queryRenderData = use g_rendererData


runGame ∷ GameM a → Game → C.Curses (a, Game)
runGame p = runStateT (runGameM p)


evalGame ∷ GameM a → Game → C.Curses a
evalGame p = evalStateT (runGameM p)


execGame ∷ GameM a → Game → C.Curses Game
execGame p = execStateT (runGameM p)

--------------------------------------------------------------------------------

runObjectMonadForPlayer ∷ (V2 Int, Object States) → Free (ObjectF States) a → GameM a
runObjectMonadForPlayer t (Free (Position fn)) =
    runObjectMonadForPlayer t (fn (fst t))
runObjectMonadForPlayer (cv, o) (Free (Move v n)) =
    doWorld (moveObject cv o v) *>
        runObjectMonadForPlayer (v, o) n
runObjectMonadForPlayer t (Free (Passable fn)) =
    runObjectMonadForPlayer t (fn $ view o_passable (snd t))
runObjectMonadForPlayer (cv, o) (Free (SetPassable b n)) =
    let no = o_passable .~ b $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer t (Free (SeeThrough fn)) =
    runObjectMonadForPlayer t (fn $ view o_seeThrough (snd t))
runObjectMonadForPlayer (cv, o) (Free (SetSeeThrough b n)) =
    let no = o_seeThrough .~ b $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (CanSee v fn)) =
    doWorld (castVisibilityRay cv v) >>=
        runObjectMonadForPlayer (cv, o) . fn . and . fmap snd
runObjectMonadForPlayer (cv, o) (Free (ChangeSymbol s n)) =
    let no = o_symbol .~ s $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (ChangeMat m n)) =
    let no = o_material .~ m $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (Message s n)) = -- TODO this means we can delete set status from the world code! :-D
    doWorld (setStatus s) *>
        runObjectMonadForPlayer (cv, o) n
runObjectMonadForPlayer (cv, o) (Free (ScanRange r f fn)) = do
    points ← doWorld (interestingObjects cv r f)
    values ← doWorld (foldr onlyJust [] <$> traverse (fmap lastValue . cellAt) points)
    runObjectMonadForPlayer (cv, o) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l
runObjectMonadForPlayer (cv, o) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → runObjectMonadForPlayer (cv, o) (fn (V2 0 0))
        LineOfSight → runObjectMonadForPlayer (cv, o) (fn (V2 1 1))
runObjectMonadForPlayer _ (Pure x) =
    pure x

