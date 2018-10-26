{-# LANGUAGE UnicodeSyntax, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Dreamnet.Dreamnet
--( defaultDesignData
--, launchDreamnet
--)
where


import Prelude            hiding (head, (!!))
import Safe                      (succSafe, predSafe, at, atMay, fromJustNote)
import Control.Lens              (view, views, (%~), (^.), set, _Just, preview,
                                  previews, use)
import Control.Monad             (void, join)
import Control.Monad.Free        (Free(..))
import Control.Monad.Trans       (lift)
import Control.Monad.State       (execState, modify, get)
import Control.Monad.Random      (MonadRandom)
import Control.Monad.IO.Class    (MonadIO)
import Data.Functor              (($>))
import Data.Bifunctor            (bimap, first)
import Data.Semigroup            ((<>))
import Data.Bool                 (bool)
import Data.List                 (genericLength, elemIndex)
import Data.List.NonEmpty        (NonEmpty(..), toList, (!!))
import Data.Foldable             (traverse_)
import Data.Maybe                (fromMaybe, fromJust)
import Data.Singletons           (withSomeSing)
import Linear                    (V2(V2), _x, _y)

import qualified Data.Vector as V
import qualified Data.Map    as M
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.Engine.Conversation
import qualified Dreamnet.Engine.Visibility as Visibility (height)
import Dreamnet.Engine.Character
import Dreamnet.Engine.Object
import qualified Dreamnet.Engine.Input as Input
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration

import Dreamnet.Rendering.Renderer
import Dreamnet.Game
import Dreamnet.ComputerModel

import Design.ObjectPrograms
import Design.GameCharacters
import Design.Items

--------------------------------------------------------------------------------

{-
data GameFlowEvent = GFEWorldEvent Input.WorldEvent
                   | GFEUIEvent Input.UIEvent
                   | GFETargetEvent Input.TargetEvent

$(genSingletons [ ''GameFlowEvent ])

class GameStateFlow (gsi ∷ GameStateEnum) (ev ∷ GameFlowEvent) where
    type FlowOutState gsi ev ∷ *
    gameStateFlow ∷ GameState gsi → Sing ev → FlowOutState gsi ev 


instance GameStateFlow 'Normal ('GFEWorldEvent ('Input.Move k)) where
    type FlowOutState 'Normal ('GFEWorldEvent ('Input.Move k)) = GameState 'Normal
    gameStateFlow (StNormal w) (SGFEWorldEvent (Input.SMove d)) =
        updateVisible $ StNormal $ flip execWorld w $ do
            movePlayer (dirToVec' d)
            increaseTurn
-}


choiceChs ∷ String
choiceChs = "fdsahjkltrewyuiopvcxzbnmFDSAHJKLTREWYUIOPVCXZBNM"

--------------------------------------------------------------------------------

defaultDesignData ∷ (MonadIO r, MonadRandom r) ⇒ r DesignData
defaultDesignData =
    pure $
        DesignData {
          _dd_characters  = characterDictionary characters
        --, _dd_dev_startingMap = "./res/job"
        , _dd_dev_startingMap = "./res/bar"
        --, _dd_dev_startingMap = "./res/apartmentblock"
        --, _dd_dev_startingMap = "./res/apartment0"
        }


launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

type DreamnetMonad = RendererM C.Curses

--------------------------------------------------------------------------------

dreamnet ∷ DesignData → IO ()
dreamnet dd = C.runCurses $ do
    -- Init curses
    C.setRaw     True
    C.setEcho    False
    C.defaultWindow >>= (`C.setKeypad` True)
    void $ C.setCursorMode C.CursorInvisible
    r ← newRenderEnvironment
    g ← newGame dd
    loopTheLoop g r
    where
        loopTheLoop ∷ GameState gs → RendererEnvironment → C.Curses ()
        loopTheLoop g r = do
            x ← runRenderer (innerLoop g) r
            case fst x of
                (Just (SomeGS gs)) → loopTheLoop gs (snd x)
                Nothing            → pure ()

        innerLoop ∷ GameState gs→ DreamnetMonad (Maybe SomeGameState)
        innerLoop = \case
            gs@(StNormal w) → do
                renderNormal w
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    --ev@(Input.SMove _)         → pure (Just (SomeGS (gameStateFlow gs (SGFEWorldEvent ev))))
                    ev@(Input.SMove _)         → pure (Just (SomeGS (processNormal gs ev)))
                    ev@(Input.SMoveCamera _)   → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SExamine          → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SOperate          → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SExamineHeld      → pure (Just (either SomeGS id (processNormal gs ev)))
                    ev@Input.SOperateHeld      → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SOperateHeldOn    → pure (Just (either SomeGS SomeGS (processNormal gs ev)))
                    ev@Input.STalk             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SGet              → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SWear             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SStoreIn          → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SPullFrom         → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SWait             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@(Input.SSetStance _)    → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SInventorySheet   → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SCharacterSheet   → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SSwitchToTactical → pure (Just (either SomeGS SomeGS (processNormal gs ev)))
                    ev@Input.SSwitchToHud      → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SBackToMainMenu   → pure Nothing

            gs@(StExamination w _) → do
                renderExamination
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))

            gs@(StConversation w _ (Free cn')) → do
                renderConversation cn'
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SSelectChoice        → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            (StConversation w _ (Pure _)) → do
                renderNormal w
                flush
                pure (Just $ SomeGS (StNormal w))

            gs@(StComputerOperation w (v,ix) cd) → do
                renderComputerOperation cd
                flush
                lift (Input.nextEvent gs ()) >>= \case
                    Input.PassThroughBack    → pure (Just (SomeGS (StNormal (execWorld saveWorldData w))))
                    (Input.PassThrough '\n') → pure (Just (SomeGS (StComputerOperation w  (v,ix) (snd $ runComputer commitInput cd))))
                    (Input.PassThrough '\b') → pure (Just (SomeGS (StComputerOperation w (v,ix) (snd $ runComputer backspace cd))))
                    (Input.PassThrough c)    → pure (Just (SomeGS (StComputerOperation w (v,ix) (snd $ runComputer (typeIn c) cd))))
                    --ev                       → Just . SomeGS <$> processComputerOperation gs ev
                where
                    saveWorldData = modifyObjectAt (v, ix) (pure . set o_state (Computer cd))

            gs@(StHudTeam w i) → do
                renderHudTeam w i
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SWest)  → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SEast)  → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SNext)         → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious)     → Just . SomeGS <$> processUI gs ev
                    ev@Input.SSelectChoice        → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
                        
            gs@(StHudMessages w) → do
                renderHudMessages w
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SNext)         → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious)     → Just . SomeGS <$> processUI gs ev
                    ev@Input.SSelectChoice        → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            gs@(StHudWatch w _ _) → do
                renderHudWatch w
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SWest)  → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SEast)  → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SNext)         → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious)     → Just . SomeGS <$> processUI gs ev
                    ev@Input.SSelectChoice        → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            gs@(StInventoryUI w) → do
                renderInventoryUI
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack              → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            gs@(StSkillsUI w ch) → do
                renderSkillsUI ch
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.STab SNext)     → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack          → pure (Just (SomeGS (StNormal w)))
                    _                         → pure (Just (SomeGS gs))
            gs@(StEquipmentUI w ch) → do
                renderEquipmentUI ch
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.STab SNext)     → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SUiBack          → pure (Just (SomeGS (StNormal w)))
                    _                         → pure (Just (SomeGS gs))

            gs@(StTargetSelectionAdjactened w tp _) → do
                renderTargetSelectionAdjactened w tp
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveReticule _) → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SMoveTarget _)   → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SSmartTarget _)  → pure (Just (SomeGS (processTarget gs ev)))
                    ev@Input.SConfirmTarget    → pure (Just (processTarget gs ev))
                    ev@Input.STargetBack       → pure (Just (SomeGS (StNormal w)))

            gs@(StTargetSelectionDistant w tp _) → do
                renderTargetSelectionDistant w tp
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveReticule _) → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SMoveTarget _)   → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SSmartTarget _)  → pure (Just (SomeGS (processTarget gs ev)))
                    ev@Input.SConfirmTarget    → pure (Just (processTarget gs ev))
                    ev@Input.STargetBack       → pure (Just (SomeGS (StNormal w)))

            gs@(StChoiceSelection w chs i f) → do
                renderChoiceSelection chs i
                flush
                lift (Input.nextEvent gs (fst $ unzip chs)) >>= \case
                    Input.ChoiceBack → pure (Just (SomeGS (StNormal w)))
                    (Input.ChoiceCharacter c) → if c == (fst $ chs `at` i)
                        then pure (Just (runWithChoice f i))
                        else pure (Just (SomeGS (StChoiceSelection w chs (fromJust . elemIndex c . fst . unzip $ chs) f)))
        
--------------------------------------------------------------------------------

newGame ∷ DesignData → C.Curses (GameState 'Normal)
newGame dd = do
    sm  ← loadTileMap (view dd_dev_startingMap dd)
    pure $ StNormal $ newWorld
                    (either error id $ fromTileMap sm objectFromTile)
                    (playerPerson carla)
                    --(playerPerson ("Carla" `M.lookup` view dd_characters dd))
    where
        -- 1) This *could* all be just a single thing. Object type really does not matter here.
        -- 2) Actually, it does, because Object carries a specific state, later used by object programs
        objectFromTile ∷ Tile → Either String (Object States)
        objectFromTile t@(ttype → "Base") = Object sy "concrete" <$> t `readPassable` 1 <*> t `readSeeThrough` 2 <*> (bool 4 0 <$> t `readPassable` 1) <*> pure st
            where
                sy = Symbol $ view t_char t
                st = (Prop "Floor" "A floor")

        objectFromTile t@(ttype → "Prop") = Object sy <$> m <*> t `readPassable` 2 <*> t `readSeeThrough` 3 <*> h <*> st
            where
                sy = Symbol $ view t_char t
                m  = maybeToEither "Material property missing on 'Prop' tile, ix: 4" (4 `readStringProperty` t)
                h  = maybeToEither "Height property missing on 'Prop' tile, ix: 5" (5 `readIntProperty` t)
                st = Prop
                        <$> maybeToEither "Name property missing on 'Prop' tile, ix: 1" (1 `readStringProperty` t)
                        <*> maybeToEither "Description property missing on 'Prop' tile, ix: 6" (6 `readStringProperty` t)

        objectFromTile t@(ttype → "Person") = Object sy m p s h <$> st
            where
                sy = Symbol '@'
                m  = "blue"
                p  = False
                s  = True
                h  = 3
                st = pure . Person
                        =<< (\n → maybeToEither ("No character with name '" <> n <> "' defined.") (n `M.lookup` view dd_characters dd))
                        =<< maybeToEither "Name property missing for 'Person' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile (ttype → "Spawn") = -- TODO shitty hardcoding, spawns should probably be generalized somehow!)
            objectFromTile (Tile '.' (V.fromList [ "Base", "True", "True" ]))

        objectFromTile t@(ttype → "Camera") = Object sy m p s h <$> st
            where
                sy = Symbol $ view t_char t
                m  = "green light"
                p  = True
                s  = True
                h  = 1
                st = (`Camera` 0) . Faction
                        <$> maybeToEither "Faction property missing for 'Camera' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t@(ttype → "Computer") = pure $ Object sy m p s h st
            where
                sy = Symbol $ view t_char t
                m  = "metal"
                p  = False
                s  = True
                h  = 1
                st = Computer (ComputerData "" []) -- TODO define some data!

        objectFromTile t@(ttype → "Clothes") = Object sy m p s h <$> st
            where
                sy  = Symbol $ view t_char t
                m   = "cloth"
                p   = True
                s   = True
                h   = 0
                st  = pure . Clothes
                        =<< (\cid → maybeToEither ("WearableItem " <> cid <> " isn't defined!") (cid `M.lookup` clothesDict))
                        =<< maybeToEither "Clothes name property missing for 'Clothes' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t@(ttype → "Weapon") = Object sy m p s h <$> st
            where
                sy  = Symbol $ view t_char t
                m   = "metal"
                p   = True
                s   = True
                h   = 0
                st  = pure . Weapon
                        =<< (\wid → maybeToEither ("WeaponItem " <> wid <> " isn't defined!") (M.lookup wid weaponsDict))
                        =<< maybeToEither "Weapon name property missing for 'Weapon' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t@(ttype → "Ammo") = Object sy m p s h <$> st
            where
                sy  = Symbol $ view t_char t
                m   = "metal"
                p   = True
                s   = True
                h   = 0
                st  = pure . Ammo
                        =<< (\aid → maybeToEither ("AmmoItem " <> aid <> " isn't defined!") (M.lookup aid ammoDict))
                        =<< maybeToEither "Ammo name property missing for 'Ammo' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t@(ttype → "Throwable") = Object sy m p s h <$> st
            where
                sy  = Symbol $ view t_char t
                m   = "metal"
                p   = True
                s   = True
                h   = 0
                st  = pure . Throwable
                        =<< (\tid → maybeToEither ("ThrowableItem " <> tid <> " isn't defined!") (tid `M.lookup` throwableDict))
                        =<< maybeToEither "Throwable name property missing for 'Throwable' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t@(ttype → "Consumable") = Object sy m p s h <$> st
            where
                sy  = Symbol $ view t_char t
                m   = "red"
                p   = True
                s   = True
                h   = 0
                st  = pure . Consumable 
                        =<< (\cid → maybeToEither ("ConsumableItem " <> cid <> " isn't defined!") $ cid `M.lookup` consumableDict)
                        =<< maybeToEither "Consumable name property missing for 'Consumable' tile, ix: 1" (1 `readStringProperty` t)

        objectFromTile t =
            Left $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object (Symbol '@') "metal" False True 3 . Person

        maybeToEither ∷ a → Maybe b → Either a b
        maybeToEither d Nothing  = Left d
        maybeToEither _ (Just x) = Right x

        readPassable ∷ Tile → Int → Either String Bool
        readPassable t i = maybeToEither ("Passable property missing on tile '" <> ttype t <> "', ix: " <> show i) (i `readBoolProperty` t)

        readSeeThrough ∷ Tile → Int → Either String Bool
        readSeeThrough t i = maybeToEither ("SeeThrough property missing on tile '" <> ttype t <> "', ix: " <> show i) (i `readBoolProperty` t)

--------------------------------------------------------------------------------

class ProcessNormal (gsi ∷ GameStateEnum) (ev ∷ Input.WorldEvent) where
    type GameStateOut gsi ev ∷ *
    processNormal ∷ GameState gsi → Input.SWorldEvent ev → GameStateOut gsi ev


class ProcessUI (gsi ∷ GameStateEnum) (ev ∷ Input.UIEvent) where
    type UIGameStateOut gsi ev ∷ *
    processUI ∷ (RenderAPI r, Monad r) ⇒ GameState gsi → Input.SUIEvent ev → r (UIGameStateOut gsi ev)


class ProcessTarget (gsi ∷ GameStateEnum) (ev ∷ Input.TargetEvent) where
    type TgGameStateOut gsi ev ∷ *
    processTarget ∷ GameState gsi → Input.STargetEvent ev → TgGameStateOut gsi ev

--------------------------------------------------------------------------------

cellObject ∷ WorldPosition → World → Maybe (Object States)
cellObject p = evalWorld (objectAt p)


instance ProcessNormal 'Normal ('Input.Move k) where
    type GameStateOut 'Normal ('Input.Move k) = GameState 'Normal
    processNormal (StNormal w) (Input.SMove d) =
        updateVisible $ StNormal $ flip execWorld w $ do
            movePlayer (dirToVec' d)
            increaseTurn


instance ProcessNormal 'Normal ('Input.MoveCamera k) where
    type GameStateOut 'Normal ('Input.MoveCamera k) = GameState 'Normal
    processNormal (StNormal w) (Input.SMoveCamera _) = StNormal w
        --moveCamera v


instance ProcessNormal 'Normal 'Input.Examine where
    type GameStateOut 'Normal 'Input.Examine = GameState 'TargetSelectionDistant
    processNormal (StNormal w) _ = withTargetDistant w $ \(v, i) →
        case cellObject (v, i) w of
            Nothing → SomeGS $ StNormal (execWorld increaseTurn w)
                --doWorld (setStatus "There's nothing here." *> increaseTurn)
            Just o → bool (SomeGS describeWorld) (runExamineObject v i o) (notOnPlayer v)
        where
            notOnPlayer v  = views w_player ((/=v) . fst) w
            program o ch   = programForState ch (view o_state o) Examine
            --describeWorld  = do
            --    let d = evalWorld desc w
            --    setScroll (newScrollData (V2 2 1) (V2 60 20) Nothing d)
            --    pure (StExamination w d)
            describeWorld  = StExamination w (evalWorld desc w)
            runExamineObject ∷ V2 Int → Int → Object States → SomeGameState
            runExamineObject v i o =
                case preview (o_state._Person) (evalWorld (playerObject w) w) of
                    Nothing → SomeGS (StNormal w)
                    Just ch → runProgramAsPlayer w (v, i) (program o ch)
                    --Just ch → case runProgramAsPlayer w v i (program o ch) of
                    --    gs@(SomeGS (StConversation _ ps (Free cn))) → conversationUpdateUi (view ch_nickName <$> ps) cn *> pure gs
                    --    gs                                          → pure gs


instance ProcessNormal 'Normal 'Input.Operate where
    type GameStateOut 'Normal 'Input.Operate = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        case cellObject t w of
            Nothing →  SomeGS (StNormal w)
            Just o → case preview (o_state._Person) (evalWorld (playerObject w) w) of
                Nothing → SomeGS (StNormal w)
                Just ch → runProgramAsPlayer w t (program o ch)
        where
            program o ch = programForState ch (view o_state o) Operate


instance ProcessNormal 'Normal 'Input.ExamineHeld where
    type GameStateOut 'Normal 'Input.ExamineHeld = Either (GameState 'Normal) SomeGameState
    processNormal (StNormal w) _ =
        let mres = do
                ho  ← join $ previews (o_state._Person) (slotWrapperItem . primaryHandSlot) (evalWorld (playerObject w) w)
                pch ← preview (o_state._Person) (evalWorld (playerObject w) w)
                pure $ runProgramAsPlayer w (view w_player w) (programForState pch ho Examine)
        in  case mres of
                Just x  → Right x
                Nothing → Left (StNormal w)


instance ProcessNormal 'Normal 'Input.OperateHeld where
    type GameStateOut 'Normal 'Input.OperateHeld = GameState 'Normal
    processNormal (StNormal w) _ =
        let mres = do
                ho  ← join $ previews (o_state._Person) (slotWrapperItem . primaryHandSlot) (evalWorld (playerObject w) w)
                pch ← preview (o_state._Person) (evalWorld (playerObject w) w)
                pure $ runProgramAsPlayer w (view w_player w) (programForState pch ho Operate)
        in  case mres of
                Just x  → case x of
                    -- TODO BLATANTLY WRONG BUT FIXING COMPILATION NOW
                    (SomeGS gs@(StNormal _)) → gs
                    _                        → StNormal w
                Nothing → StNormal $ flip execWorld w $ do
                        -- TODO as much as the device wants!
                        increaseTurn
                        --setStatus "You aren't carrying anything in your hands."


instance ProcessNormal 'Normal 'Input.OperateHeldOn where
    type GameStateOut 'Normal 'Input.OperateHeldOn = Either (GameState 'Normal) (GameState 'TargetSelectionDistant)
    processNormal (StNormal w) _ =
        case join $ previews (o_state._Person) (slotWrapperItem . primaryHandSlot) (evalWorld (playerObject w) w) of
            Nothing → Left (StNormal w)
                --doWorld $ setStatus "You aren't carrying anything in your hands."
            Just ho →
                -- TODO obtain target should happen inside Object Program, and then interpreter
                --      can either show UI for the player, or use "brain"/Simulation to select
                --      one for the NPC's
                -- Also, the range should be item's range
                Right $ withTargetDistant w $ \t →
                    case cellObject t w of
                        Nothing → SomeGS (StNormal w)
                            --doWorld (setStatus "Nothing there.")
                        Just o  →
                            case preview (o_state._Person) (evalWorld (playerObject w) w) of
                                Nothing → SomeGS (StNormal w)
                                Just ch →
                                    -- TODO which of the game states should take precedence?
                                    let so          = view o_state o
                                    in  case runProgramAsPlayer w (view w_player w) (programForState ch ho (OperateOn so)) of
                                            (SomeGS gs) → runProgramAsPlayer (dreamnetWorld gs) t (programForState ch so (OperateWith ho))


instance ProcessNormal 'Normal 'Input.Talk where
    type GameStateOut 'Normal 'Input.Talk = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        case cellObject t w of
            Nothing → SomeGS (StNormal (execWorld increaseTurn w))
                --setStatus "Trying to talk to someone, but there's no one there."
            Just o → case preview (o_state._Person) (evalWorld (playerObject w) w) of
                Nothing → SomeGS (StNormal w)
                Just ch → runProgramAsPlayer w t (program o ch)
                --Just ch → case runProgramAsPlayer w (v, i) (program o ch) of
                --    gs@(SomeGS (StConversation _ ps (Free cn))) → conversationUpdateUi (view ch_name <$> ps) cn *> pure gs
                --    gs                                          → pure gs
                        
        where
            program o ch = programForState ch (view o_state o) Talk


instance ProcessNormal 'Normal 'Input.Get where
    type GameStateOut 'Normal 'Input.Get = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        case cellObject t w of
            Nothing → SomeGS (StNormal w)
                --setStatus "There's nothing here."
            Just o  → SomeGS $ StNormal $ flip execWorld w $ do
                -- Fugly
                -- One way around:
                -- o_state contains some kind of a 'pointer'
                -- into the structure that contains the type and correct state
                -- of the actual object.
                -- Second way:
                -- withState type of function that does something as long as the State
                -- is of the actual correct type, and does nothing if it isn't
                changePlayer (o_state._Person %~ pickUp (view o_state o))
                modifyCell (fst t) (deleteFromCell o)
                increaseTurn
        


instance ProcessNormal 'Normal 'Input.Wear where
    type GameStateOut 'Normal 'Input.Wear = GameState 'ChoiceSelection
    -- TODO equipping stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    processNormal (StNormal w) _ = withChoice w xs $ \i →
        let (side, slot) = vs `at` i
        in  SomeGS $ StNormal $ flip execWorld w $ do
                changePlayer (o_state._Person %~ (tryWear side slot <*> slotWrapperItem . primaryHandSlot))
                increaseTurn
        where
            tryWear side slot ch (Just i) = flip execState ch $ do
                modify (modifySlotContent (Just RightSide) Hand (const Nothing))
                modify (modifySlotContent side slot (const (Just i)))
            tryWear _ _ ch _  = ch
            xs = [ ('h', "Head")
                 , ('t', "Torso")
                 , ('T', "Back")
                 , ('b', "Belt")
                 , ('a', "Left arm")
                 , ('A', "Right arm")
                 , ('h', "Left thigh")
                 , ('H', "Right thigh")
                 , ('s', "Left shin")
                 , ('S', "Right shin")
                 , ('f', "Left foot")
                 , ('F', "Right foot")
                 ]
            vs = [ (Nothing, Head)
                 , (Nothing, Torso)
                 , (Nothing, Back)
                 , (Nothing, Belt)
                 , (Just LeftSide,  Arm)
                 , (Just RightSide, Arm)
                 , (Just LeftSide,  Thigh)
                 , (Just RightSide, Thigh)
                 , (Just LeftSide,  Shin)
                 , (Just RightSide, Shin)
                 , (Just LeftSide,  Foot)
                 , (Just RightSide, Foot)
                 ]


instance ProcessNormal 'Normal 'Input.StoreIn where
    type GameStateOut 'Normal 'Input.StoreIn = GameState 'ChoiceSelection
    processNormal (StNormal w) _ =
        -- TODO storing stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
        let containerList = fromMaybe [] $ previews (o_state._Person) equippedContainers (evalWorld (playerObject w) w)
            xs            = zip choiceChs $ fromJust . preview (_Just._Clothes.wi_name) . slotWrapperItem <$> containerList
        in  withChoice w xs $ \i → 
                SomeGS $ StNormal $ flip execWorld w $ do
                    changePlayer $ o_state._Person %~ (tryStore (containerList `at` i) <*> slotWrapperItem . primaryHandSlot)
                    increaseTurn
        where
            appendToContainer ∷ States → Maybe States → Maybe States
            appendToContainer i (Just (Clothes wi)) = Just $ Clothes (wi_storedItems %~ (++[i]) $ wi)
            appendToContainer _ x                   = x

            tryStore sw ch (Just i) = flip execState ch $ do
                modify (modifySlotContent (Just RightSide) Hand (const Nothing))
                modify (modifySlotContent (slotWrapperOrientation sw) (slotWrapperType sw) (appendToContainer i))
            tryStore _ ch _ = ch


instance ProcessNormal 'Normal 'Input.PullFrom where
    type GameStateOut 'Normal 'Input.PullFrom = GameState 'ChoiceSelection
    processNormal (StNormal w) _ =
        -- TODO make this single-step choice (show containers and items as tree)
        let containerList = fromMaybe [] $ previews (o_state._Person) equippedContainers (evalWorld (playerObject w) w)
            xs            = zip choiceChs $ fromJust . preview (_Just._Clothes.wi_name) . slotWrapperItem <$> containerList
        in  withChoice w xs $ \i → 
            let sw       = containerList `at` i
                itemList = view (_Just._Clothes.wi_storedItems) (slotWrapperItem sw)
                xs2      = zip choiceChs (show <$> itemList)
            in  SomeGS $ withChoice w xs2 $ \i2 →
                    let item = itemList `at` i2
                    in  SomeGS $ StNormal $ flip execWorld w $ do
                            changePlayer $ o_state._Person %~ execState (pullFrom sw item)
                            -- TODO pulling stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
                            increaseTurn
        where
            pullFrom sw item = do
                ch ← get
                modify $ modifySlotContent
                             (slotWrapperOrientation (primaryHandSlot ch))
                             (slotWrapperType (primaryHandSlot ch))
                             (const (Just item))
                modify $ modifySlotContent
                             (slotWrapperOrientation sw)
                             (slotWrapperType sw)
                             (\(Just (Clothes wi)) → Just $ Clothes $ wi_storedItems %~ filter (item /=) $ wi)


instance ProcessNormal 'Normal 'Input.Wait where
    type GameStateOut 'Normal 'Input.Wait = GameState 'Normal
    processNormal (StNormal w) _ = updateVisible (StNormal (execWorld increaseTurn w))


instance ProcessNormal 'Normal ('Input.SetStance i) where
    type GameStateOut 'Normal ('Input.SetStance i) = GameState 'Normal
    processNormal (StNormal w) (Input.SSetStance i) = case i of
        SNext     → updateFunc succSafe
        SPrevious → updateFunc predSafe
        where
            updateFunc f   = updateVisible (StNormal (execWorld (updateStance f) w))
            updateStance f = changePlayer (o_state._Person.ch_stance %~ f)


instance ProcessNormal 'Normal 'Input.InventorySheet where
    type GameStateOut 'Normal 'Input.InventorySheet = GameState 'InventoryUI
    processNormal (StNormal w) _ =
        let itemList = maybe [] listOfItemsFromContainers $
                            preview (o_state._Person) (evalWorld (playerObject w) w)
        --setScroll (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
        in  StInventoryUI w


instance ProcessNormal 'Normal 'Input.CharacterSheet where
    type GameStateOut 'Normal 'Input.CharacterSheet = GameState 'SkillsUI
    processNormal (StNormal w) _ = StSkillsUI w carla  -- TODO totally not correct


instance ProcessNormal 'Normal 'Input.SwitchToTactical where
    type GameStateOut 'Normal 'Input.SwitchToTactical = Either (GameState 'Normal) (GameState 'ChoiceSelection)
    processNormal (StNormal w) _ =
        let teamChars = evalWorld (fmap (fromJust . preview (o_state._Person)) <$> teamObjects w) w
        in  if not (null teamChars)
                then
                    let xs = zip choiceChs (view ch_name <$> teamChars)
                    in  Right $ withChoice w xs $ \_ →
                            -- TODO upgrade to data
                            let xs2 = zip choiceChs ["Move", "Operate"]
                            -- TODO fix this
                            in  SomeGS $ withChoice w xs2 $ \_ → SomeGS (StNormal w)
                else
                    Left (StNormal w)


instance ProcessNormal 'Normal 'Input.SwitchToHud where
    type GameStateOut 'Normal 'Input.SwitchToHud = GameState 'HudTeam
    processNormal (StNormal w) _ = StHudTeam w 0


programForState ∷ (DreamnetObjectAPI States o) ⇒ DreamnetCharacter → States → InteractionType States → o ()
programForState _  (Prop "Door" _)      it = genericDoor it
programForState ch (Prop "Mirror" _)    it = mirror ch it
programForState ch (Prop "Newspaper" _) it = mirror ch it
programForState _  (Prop n d)           it = genericProp n d it
programForState _  (Camera f l)         it = genericCamera f l it
programForState _  (Person ch)          it = genericPerson ch it
programForState _  (Computer cd)        it = genericComputer cd it
programForState _  (Clothes wi)         it = genericClothes wi it
programForState _  (Weapon wpi)         it = genericWeapon wpi it
programForState _  (Ammo ami)           it = genericAmmo ami it
programForState _  (Throwable twi)      it = genericThrowable twi it
programForState ch (Consumable ci)      it = genericConsumable ci ch it

--------------------------------------------------------------------------------

instance ProcessUI 'Examination ('Input.MoveCursor 'North) where
    type UIGameStateOut 'Examination ('Input.MoveCursor 'North) = GameState 'Examination
    processUI gs _ = doScroll scrollUp $> gs


instance ProcessUI 'Examination ('Input.MoveCursor 'South) where
    type UIGameStateOut 'Examination ('Input.MoveCursor 'South) = GameState 'Examination
    processUI gs _ = doScroll scrollDown $> gs

--------------------------------------------------------------------------------

instance ProcessUI 'Conversation ('Input.MoveCursor 'North) where
    type UIGameStateOut 'Conversation ('Input.MoveCursor 'North) = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view ch_name $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps cn@(Free CChoice{})) _ =
        doChoice selectPrevious $> StConversation w ps cn
    processUI gs _ =
        doScroll scrollUp $> gs


instance ProcessUI 'Conversation ('Input.MoveCursor 'South) where
    type UIGameStateOut 'Conversation ('Input.MoveCursor 'South) = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view ch_name $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps cn@(Free CChoice{})) _ =
        doChoice selectNext $> StConversation w ps cn
    processUI gs _ =
        doScroll scrollDown $> gs


instance ProcessUI 'Conversation 'Input.SelectChoice where
    type UIGameStateOut 'Conversation 'Input.SelectChoice = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view ch_name $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view ch_lastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps (Free (CTalk _ _ n))) _ = do
        case n of
            (Free cn) → conversationUpdateUi (view ch_nickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI (StConversation w ps (Free (CDescribe _ n))) _ = do
        case n of
            (Free cn) → conversationUpdateUi (view ch_nickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI (StConversation w ps (Free (CReceiveItem _ o n))) _ = do
        -- TODO incorrect, only player now receives items!
        let w' = execWorld (changePlayer (o_state._Person %~ pickUp o)) w
        case n of
            (Free cn) → conversationUpdateUi (view ch_nickName <$> ps) cn
            _         → pure ()
        pure (StConversation w' ps n)
    processUI (StConversation w ps (Free (CChoice _ fn))) _ = do
        n ← fn <$> currentChoice
        case n of
            (Free cn) → conversationUpdateUi (view ch_nickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI gs _ =
        pure gs


conversationUpdateUi ∷ (RenderAPI g, Monad g, Show o) ⇒ NonEmpty String → ConversationF o a → g ()
conversationUpdateUi _   (CChoice os _)         = conversationUpdateChoice os
conversationUpdateUi nms (CTalk i t _)          = conversationUpdateTalk nms i t
conversationUpdateUi _   (CDescribe t _)        = conversationUpdateDescribe t
conversationUpdateUi nms (CReceiveItem i o _)   = conversationUpdateReceiveItem nms i o
conversationUpdateUi _   _                      = pure ()


conversationUpdateChoice ∷ (RenderAPI g, Monad g) ⇒ [String] → g ()
conversationUpdateChoice os = do
    (p, s) ← (,) <$> positionFor 0 <*> conversationSize
    setChoice (newChoiceData p s os)


conversationUpdateTalk ∷ (RenderAPI g, Monad g) ⇒ NonEmpty String → Int → String → g ()
conversationUpdateTalk nms i t = do
    (p, s) ← (,) <$> positionFor i <*> conversationSize
    setScroll (newScrollData p s (toList nms `atMay` i) t)


conversationUpdateDescribe ∷ (RenderAPI g, Monad g) ⇒ String → g ()
conversationUpdateDescribe t = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    setScroll (newScrollData p s Nothing t)


conversationUpdateReceiveItem ∷ (RenderAPI g, Monad g, Show o) ⇒ NonEmpty String → Int → o → g ()
conversationUpdateReceiveItem nms i o = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    let t = nms !! i <> " received " <> show o
    setScroll (newScrollData p s Nothing t)

--------------------------------------------------------------------------------

positionFor ∷ (RenderAPI r, Functor r) ⇒ Int → r (V2 Integer)
positionFor i = (\s → fromMaybe (positions s `at` 0) $ (`atMay` i) $ positions s) <$> mainSize
    where
        positions ∷ (Integer, Integer) → [V2 Integer]
        positions (bimap (`div` 3) (`div` 3) → (w, h)) =
            [ V2 0       (h * 2)
            , V2 (w * 2) 0
            , V2 0       0
            , V2 (w * 2) (h * 2)

            , V2 (w * 2) h
            , V2 0       h
            , V2 w       (h * 2)
            , V2 w       0

            , V2 w       h
            ]

conversationSize ∷ (RenderAPI r, Functor r) ⇒ r (V2 Integer)
conversationSize = fmap (`div` 3) . uncurry V2 <$> mainSize

--------------------------------------------------------------------------------

-- TODO this should somehow be a part of computer ObjectAPI code, not here!
-- Note: if I make ability to set the flow function, rather than just gamestate
-- (setting gamestate should probably be an specialization of setting the flow function)
-- at Dreamnet:245 from ObjectAPI, this'll be it.
--------------------------------------------------------------------------------

type family IterateOverHudElements (gs ∷ GameStateEnum) (i ∷ Iteration) ∷ GameStateEnum where
    IterateOverHudElements 'HudTeam 'Next     = 'HudMessages
    IterateOverHudElements 'HudTeam 'Previous = 'HudWatch

    IterateOverHudElements 'HudMessages 'Next     = 'HudWatch
    IterateOverHudElements 'HudMessages 'Previous = 'HudTeam

    IterateOverHudElements 'HudWatch 'Next     = 'HudTeam
    IterateOverHudElements 'HudWatch 'Previous = 'HudMessages

--------------------------------------------------------------------------------

instance ProcessUI 'HudTeam ('Input.Tab i) where
    type UIGameStateOut 'HudTeam ('Input.Tab i) = GameState (IterateOverHudElements 'HudTeam i)
    processUI (StHudTeam w _) (Input.STab SNext)     = pure (StHudMessages w)
    processUI (StHudTeam w _) (Input.STab SPrevious) = pure (StHudWatch w 0 0)


instance ProcessUI 'HudTeam ('Input.MoveCursor 'West) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'West) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SWest) = pure (StHudTeam w (max 0 (i - 1)))


instance ProcessUI 'HudTeam ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'South) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SSouth) = pure (StHudTeam w tp)
        where
            tp = evalWorld (min (i + 3) . genericLength . fmap memberPosition <$> use w_team) w


instance ProcessUI 'HudTeam ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'North) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SNorth) = pure (StHudTeam w (max 0 (i - 3)))


instance ProcessUI 'HudTeam ('Input.MoveCursor 'East) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'East) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SEast) = pure (StHudTeam w tp)
        where
            tp = evalWorld (min (i + 1) . genericLength . fmap memberPosition <$> use w_team) w


instance ProcessUI 'HudTeam 'Input.SelectChoice where
    type UIGameStateOut 'HudTeam 'Input.SelectChoice = GameState 'SkillsUI
    processUI (StHudTeam w i) _ = pure (StSkillsUI w (completeTeam w `at` i))

--------------------------------------------------------------------------------

instance ProcessUI 'HudMessages ('Input.Tab i) where
    type UIGameStateOut 'HudMessages ('Input.Tab i) = GameState (IterateOverHudElements 'HudMessages i)
    processUI (StHudMessages w) (Input.STab SNext)     = pure (StHudWatch w 0 0)
    processUI (StHudMessages w) (Input.STab SPrevious) = pure (StHudTeam w 0)


instance ProcessUI 'HudMessages ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudMessages ('Input.MoveCursor 'North) = GameState 'HudMessages
    -- TODO scroll
    processUI (StHudMessages w) _ = pure (StHudMessages w)


instance ProcessUI 'HudMessages ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudMessages ('Input.MoveCursor 'South) = GameState 'HudMessages
    -- TODO scroll
    processUI (StHudMessages w) _ = pure (StHudMessages w)


instance ProcessUI 'HudMessages 'Input.SelectChoice where
    type UIGameStateOut 'HudMessages 'Input.SelectChoice = GameState 'HudMessages
    -- TODO use scroll window to show log
    processUI (StHudMessages w) _ = pure (StHudMessages w)

--------------------------------------------------------------------------------

instance ProcessUI 'HudWatch ('Input.Tab i) where
    type UIGameStateOut 'HudWatch ('Input.Tab i) = GameState (IterateOverHudElements 'HudWatch i)
    processUI (StHudWatch w _ _) (Input.STab SNext)     = pure (StHudTeam w 0)
    processUI (StHudWatch w _ _) (Input.STab SPrevious) = pure (StHudMessages w)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'West) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'West) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SWest)  = pure (StHudWatch w t ((b - 1) `mod` 3))


instance ProcessUI 'HudWatch ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'South) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SSouth) = pure (StHudWatch w (t - 1) b)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'North) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SNorth) = pure (StHudWatch w (t + 1) b)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'East) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'East) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SEast)  = pure (StHudWatch w t ((b + 1) `mod` 3))


instance ProcessUI 'HudWatch 'Input.SelectChoice where
    type UIGameStateOut 'HudWatch 'Input.SelectChoice = GameState 'HudWatch
    processUI gs _ = pure gs


--------------------------------------------------------------------------------

instance ProcessUI 'InventoryUI ('Input.MoveCursor 'North) where
    type UIGameStateOut 'InventoryUI ('Input.MoveCursor 'North) = GameState 'InventoryUI
    processUI gs (Input.SMoveCursor SNorth) = doScroll scrollUp $> gs


instance ProcessUI 'InventoryUI ('Input.MoveCursor 'South) where
    type UIGameStateOut 'InventoryUI ('Input.MoveCursor 'South) = GameState 'InventoryUI
    processUI gs (Input.SMoveCursor SSouth) = doScroll scrollDown $> gs

--------------------------------------------------------------------------------

instance ProcessUI 'SkillsUI ('Input.Tab i) where
    type UIGameStateOut 'SkillsUI ('Input.Tab i) = GameState 'EquipmentUI
    processUI (StSkillsUI w ch) _ = pure (StEquipmentUI w ch)

--------------------------------------------------------------------------------

instance ProcessUI 'EquipmentUI ('Input.Tab i) where
    type UIGameStateOut 'EquipmentUI ('Input.Tab i) = GameState 'SkillsUI
    processUI (StEquipmentUI w ch) _ = pure (StSkillsUI w ch)

--------------------------------------------------------------------------------

instance ProcessTarget 'TargetSelectionAdjactened ('Input.MoveReticule d) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.MoveReticule d) = GameState 'TargetSelectionAdjactened
    processTarget (StTargetSelectionAdjactened w (_, i) f) (Input.SMoveReticule d) = StTargetSelectionAdjactened w (dirToVec' d, i) f


instance ProcessTarget 'TargetSelectionAdjactened ('Input.MoveTarget i) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.MoveTarget i) = GameState 'TargetSelectionAdjactened
    processTarget (StTargetSelectionAdjactened w (tp, i) f) (Input.SMoveTarget SNext)     = StTargetSelectionAdjactened w (tp, (max 0 (i - 1))) f
    processTarget (StTargetSelectionAdjactened w (tp, i) f) (Input.SMoveTarget SPrevious) = StTargetSelectionAdjactened w (tp, (min (i + 1) maxi)) f
        where
            maxi =  evalWorld (maxCellIndex . (tp +) . fst =<< use w_player) w


instance ProcessTarget 'TargetSelectionAdjactened ('Input.SmartTarget i) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.SmartTarget i) = GameState 'TargetSelectionAdjactened
    processTarget gs _ = gs


instance ProcessTarget 'TargetSelectionAdjactened 'Input.ConfirmTarget where
    type TgGameStateOut 'TargetSelectionAdjactened 'Input.ConfirmTarget = SomeGameState
    processTarget (StTargetSelectionAdjactened w tp f) _ = runWithTarget f ((+ppos) `first`  tp)
        where
            ppos = evalWorld (fst <$> use w_player) w

--------------------------------------------------------------------------------

instance ProcessTarget 'TargetSelectionDistant ('Input.MoveReticule d) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.MoveReticule d) = GameState 'TargetSelectionDistant
    processTarget (StTargetSelectionDistant w (tp, i) f) (Input.SMoveReticule d) = StTargetSelectionDistant w (tp + dirToVec' d, i) f


instance ProcessTarget 'TargetSelectionDistant ('Input.MoveTarget i) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.MoveTarget i) = GameState 'TargetSelectionDistant
    processTarget (StTargetSelectionDistant w (tp, i) f) (Input.SMoveTarget SNext) = StTargetSelectionDistant w (tp, lowerTarget) f
        where
            lowerTarget = max 0 (i - 1)
    processTarget (StTargetSelectionDistant w (tp, i) f) (Input.SMoveTarget SPrevious) = StTargetSelectionDistant w (tp, min (i + 1) maxi) f
        where
            maxi = evalWorld (maxCellIndex tp) w


instance ProcessTarget 'TargetSelectionDistant ('Input.SmartTarget i) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.SmartTarget i) = GameState 'TargetSelectionDistant
    processTarget gs _ = gs


instance ProcessTarget 'TargetSelectionDistant 'Input.ConfirmTarget where
    type TgGameStateOut 'TargetSelectionDistant 'Input.ConfirmTarget = SomeGameState
    processTarget (StTargetSelectionDistant _ tp f) _ = runWithTarget f tp

--------------------------------------------------------------------------------

equippedContainers ∷ (ItemTraits i) ⇒ Character i c f → [SlotWrapper i]
equippedContainers = filter containers . equippedSlots
    where
        containers (SlotWrapper s) = views s_item (maybe False isContainer) s


listOfItemsFromContainers ∷ Character States c f → [String]
listOfItemsFromContainers ch = concat $ makeItemList <$> equippedContainers ch
    where
        -- TODO again, annoying. We don't know its "Clothes" inside Slot.
        --      why is my typing logic so bad here???
        makeItemList ∷ SlotWrapper States → [String]
        makeItemList (SlotWrapper (Slot (Just (Clothes wi)))) = _wi_name wi : (("- "<>) . show <$> _wi_storedItems wi)
        makeItemList _                                        = []



-- TODO reuse code for aiming weapons
--switchAim ∷ Maybe (Object → Bool) → StateT Game C.Curses ()
--switchAim (Just nof) = do
--    pp ← use  (g_world.w_player.e_position)
--    os ← uses (g_world.w_map) (interestingObjects pp 2 nof)
--    ca ← use  g_aim
--    case ca of
--        Just a → g_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
--        _      → g_aim .= headMay os
--switchAim Nothing = g_aim .= Nothing


-- TODO reuse code for aiming weapons
--allButTheBase ∷ Object → Bool
--allButTheBase o
--    | view o_symbol o == '.' = False
--    | otherwise              = True

--------------------------------------------------------------------------------

--render ∷ (RenderAPI r, Monad r) ⇒ GameState g e → r ()

renderNormal ∷ (RenderAPI r, Monad r) ⇒ World → r ()
renderNormal w = do
    renderWorld w
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    --updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch False (view w_turn w)

renderExamination ∷ (RenderAPI r, Monad r) ⇒ r ()
renderExamination = do
    updateUi clear
    updateUi =<< drawInformation

renderConversation ∷ (RenderAPI r, Monad r) ⇒ ConversationF a b → r ()
renderConversation CChoice{} = do
    updateUi clear
    updateUi =<< drawChoice
renderConversation CTalk{} = do
    updateUi clear
    updateUi =<< drawInformation
renderConversation CDescribe{} = do
    updateUi clear
    updateUi =<< drawInformation
renderConversation CReceiveItem{} = do
    updateUi clear
    updateUi =<< drawInformation
renderConversation _ = do
    pure ()

renderComputerOperation ∷ (RenderAPI r, Monad r) ⇒ ComputerData → r ()
renderComputerOperation cd =
    updateUi $ do
        clear
        drawComputer cd

renderHudTeam ∷ (RenderAPI r, Monad r) ⇒ World → Int → r ()
renderHudTeam w i = do
    updateHud =<< drawTeamHud (completeTeam w) (Just i)
    --updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch False (view w_turn w)

renderHudMessages ∷ (RenderAPI r, Monad r) ⇒ World → r ()
renderHudMessages w = do
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    --updateHud =<< drawStatus True (evalWorld status w)
    updateHud =<< drawWatch False (view w_turn w)

renderHudWatch ∷ (RenderAPI r, Monad r) ⇒ World → r ()
renderHudWatch w = do
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    --updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch True (view w_turn w)

renderInventoryUI ∷ (RenderAPI r, Monad r) ⇒ r ()
renderInventoryUI = do
    updateUi clear
    updateUi =<< drawInformation

renderSkillsUI ∷ (RenderAPI r, Monad r) ⇒ DreamnetCharacter → r ()
renderSkillsUI ch = do
    updateUi clear
    updateUi =<< drawCharacterSheet ch

renderEquipmentUI ∷ (RenderAPI r, Monad r) ⇒ DreamnetCharacter → r ()
renderEquipmentUI ch = do
    updateUi clear
    updateUi =<< drawEquipmentDoll ch

renderTargetSelectionAdjactened ∷ (RenderAPI r, Monad r) ⇒ World → WorldPosition → r ()
renderTargetSelectionAdjactened w (tp, i) = do
    let pp = views w_player fst w
    white ← style s_colorWhite
    green ← style s_colorGreen
    renderCellContentsToStatus w (pp + tp, i)
    updateMain $ do
        RenderAction $ do
            C.setColor white
            (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) pp
                [ "yku"
                , "h.l"
                , "bjn"
                ]
        draw' (pp + tp) (charForVec tp) [C.AttributeColor green]
    where
        charForVec (V2 -1  -1) = 'y'
        charForVec (V2  0  -1) = 'k'
        charForVec (V2  1  -1) = 'u'
        charForVec (V2 -1   0) = 'h'
        charForVec (V2  1   0) = 'l'
        charForVec (V2 -1   1) = 'b'
        charForVec (V2  0   1) = 'j'
        charForVec (V2  1   1) = 'n'
        charForVec _           = '.'


renderTargetSelectionDistant ∷ (RenderAPI r, Monad r) ⇒ World → WorldPosition → r ()
renderTargetSelectionDistant w tp = do
    renderWorld w
    green ← style s_colorGreen
    renderCellContentsToStatus w tp
    updateMain $ draw' (fst tp) 'X' [C.AttributeColor green]

renderChoiceSelection ∷ (RenderAPI r, Monad r) ⇒ [(Char, String)] → Int → r ()
renderChoiceSelection xs i = do
    green ← style s_colorGreen
    white ← style s_colorWhite
    updateUi $ RenderAction $ do
        C.clear
        C.resizeWindow (genericLength xs + 4) 30 -- TODO Enough to fit all
        C.moveWindow 10 10 -- TODO Center
        C.drawBorder (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '╭' [])
                     (Just $ C.Glyph '╮' [])
                     (Just $ C.Glyph '╰' [])
                     (Just $ C.Glyph '╯' [])
        traverse_ (drawLine green white) $ zip [0..] xs
    where
        drawLine chl cnohl (l, (ch, str)) = do
            if l == i
                then C.setColor chl
                else C.setColor cnohl
            drawString (2 ∷ Int) (l + 2) (ch : " - " <> str)


renderWorld ∷ (RenderAPI r, Monad r) ⇒ World → r ()
renderWorld w =
    let m = view w_map w
        d = views wm_data (fmap (fromMaybe (error "No last value in the map Cell!") . lastValue)) m
        v = view w_vis w
    in  updateMain =<< drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) (width m) d v



renderCellContentsToStatus ∷ (RenderAPI r, Monad r) ⇒ World → WorldPosition → r ()
renderCellContentsToStatus w (v, i) = do
    white ← style s_colorWhite
    green ← style s_colorGreen

    let cellContents = evalWorld (cellContentsString <$> cellAt v) w
    updateHud (f cellContents white green)
    where
        cellContentsString = fmap (\c → show (view o_state c) <> ", " <> show (Visibility.height c)) . cellValues
        f xs h noh = clearStatus *> statusOrigin >>= \(start,padding,_) → RenderAction $
            traverse_ (\(y, s) → drawStringWithHighlight start y s (y == i + padding) h noh) $ zip [padding..] xs
        drawStringWithHighlight x y s h colh colnoh = do
            C.moveCursor (fromIntegral y) (fromIntegral x)
            if h
                then C.setColor colnoh
                else C.setColor colh
            C.drawString s




completeTeam ∷ World → [DreamnetCharacter]
completeTeam w =
    let p = flip evalWorld w $ use w_player >>= \t → 
                               fromJustNote "complTeam" . valueAt (snd t) <$> cellAt (fst t)
    in  [(\(Person chp) → chp) (p ^. o_state)]
    {-
    let t = flip evalWorld w $ team >>= 
                               traverse (fmap fromJustNote . teamMemberPosition) >>=
                               traverse (fmap fromJustNote . uncurry valueAt . unwrapWorldCoord)
        p = flip evalWorld w $ playerPosition >>=
                               fmap fromJustNote . uncurry valueAt . unwrapWorldCoord
    in  (\(Person chp) → chp) (p ^. o_state) : ((\(Person tm) → tm) . view o_state <$> t)
    -}

--------------------------------------------------------------------------------

