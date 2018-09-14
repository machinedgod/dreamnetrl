{-# LANGUAGE UnicodeSyntax, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Dreamnet.Dreamnet
( defaultDesignData
, launchDreamnet
) where


import Prelude            hiding (head, (!!))
import Safe                      (succSafe, predSafe, at, atMay, fromJustNote)
import Control.Lens              (view, views, (%~), (^.), set, _Just, preview)
import Control.Monad             (void, (>=>))
import Control.Monad.Free        (Free(Free))
import Control.Monad.Trans       (lift)
import Control.Monad.State       (execState, modify, get)
import Control.Monad.Random      (MonadRandom)
import Control.Monad.IO.Class    (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Bifunctor            (bimap)
import Data.Semigroup            ((<>))
import Data.Bool                 (bool)
import Data.List                 (genericLength, elemIndex)
import Data.List.NonEmpty        (NonEmpty(..), toList, (!!))
import Data.Foldable             (traverse_)
import Data.Maybe                (fromMaybe, fromJust)
import Linear                    (V2(V2), _x, _y)

import qualified Data.Vector as V
import qualified Data.Map    as M
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Visibility hiding (height)
import qualified Dreamnet.Engine.Visibility as Visibility (height)
import Dreamnet.Engine.Character
import Dreamnet.Engine.Object
import qualified Dreamnet.Engine.Input as Input

import Dreamnet.Rendering.Renderer
import Dreamnet.Game
import Dreamnet.ComputerModel

import Design.ObjectPrograms
import Design.GameCharacters
import Design.Items

--------------------------------------------------------------------------------

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
        loopTheLoop ∷ GameState (GameM (RendererM C.Curses)) → RendererEnvironment → C.Curses ()
        loopTheLoop g r = do
            ((s, g'), r') ← flip runRenderer r $ flip runGame g $ do
                gameState >>= render >> flush
                doWorld (setStatus "")
                gameState >>= gameStateFlow >>= changeGameState . const
            case s of
                Quit _ → pure ()
                _      → loopTheLoop g' r'

        gameStateFlow (Quit w)                             = pure (Quit w)
        gameStateFlow (Normal _)                           = processNormal dd                        =<< lift (lift Input.nextWorldEvent)
        gameStateFlow (Examination _ d)                    = processExamination d                    =<< lift (lift Input.nextUiEvent)
        gameStateFlow (HudTeam _ i)                        = processHudTeam dd i                     =<< lift (lift Input.nextUiEvent)
        gameStateFlow (HudMessages _)                      = processHudMessages                      =<< lift (lift Input.nextUiEvent)
        gameStateFlow (HudWatch _ t b)                     = processHudWatch t b                     =<< lift (lift Input.nextUiEvent)
        gameStateFlow (Conversation _ ps cn)               = processConversation ps cn               =<< lift (lift Input.nextUiEvent)
        gameStateFlow (ComputerOperation _ p cd)           = processComputerOperation p cd           =<< lift (lift Input.nextInteractionEvent)
        gameStateFlow (InventoryUI _)                      = processInventoryUI                      =<< lift (lift Input.nextUiEvent)
        gameStateFlow (SkillsUI _ ch)                      = processSkillsUI ch                      =<< lift (lift Input.nextUiEvent)
        gameStateFlow (EquipmentUI _ ch)                   = processEquipmentUI ch                   =<< lift (lift Input.nextUiEvent)
        gameStateFlow (TargetSelectionAdjactened _ tp i f) = processAdjactenedTargetSelection tp i f =<< lift (lift Input.nextTargetSelectionEvent)
        gameStateFlow (TargetSelectionDistant _ tp i f)    = processDistantTargetSelection tp i f    =<< lift (lift Input.nextTargetSelectionEvent)
        gameStateFlow (ChoiceSelection _ xs i f)           = processChoiceSelection xs i f           =<< lift (lift (Input.nextChoiceEvent (fst $ unzip xs)))




newGame ∷ DesignData → C.Curses (GameState g)
newGame dd = do
    sm  ← loadTileMap (view dd_dev_startingMap dd)
    pure $ Normal $ newWorld
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


processNormal ∷ (GameAPI g, RenderAPI g, Monad g) ⇒ DesignData → Input.WorldEvent → g (GameState g)
processNormal _ Input.Quit =
    Quit <$> world
processNormal _ (Input.Move v) = do
    doWorld $ do
        movePlayer v
        increaseTurn
    updateVisible
    Normal <$> world
processNormal _ (Input.MoveCamera v) = do
    moveCamera v
    Normal <$> world
processNormal _ Input.Wait = do
    doWorld $ do
        setStatus "Waiting..."
        increaseTurn
    --runProgramAsPlayer v (operationProgramForSymbol (view o_symbol o) $ AiTick)
    updateVisible
    Normal <$> world
processNormal _ Input.HigherStance = do
    doWorld $ changePlayer $
        o_state %~ withCharacter (ch_stance %~ predSafe)
    updateVisible
    Normal <$> world
processNormal _ Input.LowerStance = do
    doWorld $ changePlayer $
        o_state %~ withCharacter (ch_stance %~ succSafe)
    updateVisible
    Normal <$> world
processNormal _ Input.Get = do
    withTarget Adjactened $ \v i → do
        doWorld $ valueAt i <$> cellAt v >>= \case
            Nothing → do
                setStatus "There's nothing here."
            Just o  → do
                -- Fugly
                -- One way around:
                -- o_state contains some kind of a 'pointer'
                -- into the structure that contains the type and correct state
                -- of the actual object.
                -- Second way:
                -- withState type of function that does something as long as the State
                -- is of the actual correct type, and does nothing if it isn't
                changePlayer (o_state %~ withCharacter (pickUp (view o_state o)))
                modifyCell v (deleteFromCell o)
                increaseTurn
        Normal <$> world
processNormal _ Input.Wear = do
    -- TODO equipping stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    withChoice xs $ \i → do
        let (side, slot) = vs `at` i
        doWorld $ do
            changePlayer $ o_state %~ withCharacter (tryWear side slot <*> slotWrapperItem . primaryHandSlot)
            increaseTurn
        Normal <$> world
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
processNormal _ Input.StoreIn = do
    -- TODO storing stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    containerList ← doWorld $ do
        pp ← playerPosition >>= fmap . valueAt . snd <*> cellAt . fst
        pure $ whenCharacter equippedContainers [] $ view o_state (fromJustNote "storeIn" pp)
    let xs = zip choiceChs $ fromJust . preview (_Just._Clothes.wi_name) . slotWrapperItem <$> containerList
    withChoice xs $ \i →  do
        doWorld $ do
            changePlayer $ o_state %~ withCharacter (tryStore (containerList `at` i) <*> slotWrapperItem . primaryHandSlot)
            increaseTurn
        Normal <$> world
    where
        appendToContainer ∷ States → Maybe States → Maybe States
        appendToContainer i (Just (Clothes wi)) = Just $ Clothes (wi_storedItems %~ (++[i]) $ wi)
        appendToContainer _ x                   = x

        tryStore sw ch (Just i) = flip execState ch $ do
            modify (modifySlotContent (Just RightSide) Hand (const Nothing))
            modify (modifySlotContent (slotWrapperOrientation sw) (slotWrapperType sw) (appendToContainer i))
        tryStore _ ch _ = ch

processNormal _ Input.PullFrom = do
    -- TODO make this single-step choice (show containers and items as tree)
    containerList ← doWorld $ do
        pp ← playerPosition >>= fmap . valueAt . snd <*> cellAt . fst
        pure $ whenCharacter equippedContainers [] $ view o_state (fromJustNote "storeIn" pp)
    let xs = zip choiceChs $ fromJust . preview (_Just._Clothes.wi_name) . slotWrapperItem <$> containerList
    withChoice xs $ \i → 
        let sw       = containerList `at` i
            itemList = view (_Just._Clothes.wi_storedItems) (slotWrapperItem sw)
            xs2      = zip choiceChs (show <$> itemList)
        in  withChoice xs2 $ \i2 → do
            doWorld $ do
                let item = itemList `at` i2
                changePlayer $ o_state %~ withCharacter (execState (pullFrom sw item))
                -- TODO pulling stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
                increaseTurn
            Normal <$> world
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
processNormal _ Input.Examine = do
    withTarget (Distant (Range 30)) $ \v i → do
        gs ← doWorld (valueAt i <$> cellAt v) >>= \case
            Nothing →  do
                doWorld $ do
                    setStatus "There's nothing here."
                    increaseTurn
                Normal <$> world
            Just o  →  do
                onHerself ← (==v) . fst <$> doWorld playerPosition
                if onHerself
                    then Examination <$> world <*> doWorld desc
                    else doWorld (increaseTurn *> playerObject) >>=
                            whenCharacter (runProgramAsPlayer v i . program o) (Normal <$> world) . view o_state
        case gs of
            (Examination _ d) → let examineUpdateUi = setScroll . newScrollData (V2 2 1) (V2 60 20) Nothing
                                in  examineUpdateUi d
            (Conversation _ ps cn) → conversationUpdateUi (view ch_nickName <$> ps) cn
            _                      → pure ()
        pure gs
    where
        program o ch = programForState ch (view o_state o) Examine
processNormal _ Input.Operate = do
    withTarget Adjactened $ \v i →
        doWorld (valueAt i <$> cellAt v) >>= \case
            Nothing →  do
                doWorld (setStatus "There's nothing here.")
                Normal <$> world
                              -- .-- TODO as much as operation program wants!
            Just o → doWorld (increaseTurn *> playerObject) >>=
                        whenCharacter (runProgramAsPlayer v i . program o) (Normal <$> world) . view o_state
    where
        program o ch = programForState ch (view o_state o) Operate
processNormal _ Input.ExamineHeld = do
    mres ← runMaybeT $ do
        ho  ← lift (doWorld playerObject)
                    >>= MaybeT . pure . (maybeCharacter >=> slotWrapperItem . primaryHandSlot) . view o_state
        (pp, ph) ← lift $ doWorld playerPosition
        pch      ← MaybeT $ doWorld (maybeCharacter . view o_state <$> playerObject)
        lift $ runProgramAsPlayer pp ph (programForState pch ho Examine)
    maybe
        (doWorld (setStatus "You aren't carrying anything in your hands.") >> Normal <$> world)
        pure
        mres
processNormal _ Input.OperateHeld = do
    mres ← runMaybeT $ do
        ho  ← lift (doWorld playerObject)
                    >>= MaybeT . pure . (maybeCharacter >=> slotWrapperItem . primaryHandSlot) . view o_state
        (pp, ph) ← lift $ doWorld playerPosition
        pch      ← MaybeT $ doWorld (maybeCharacter . view o_state <$> playerObject)
        lift $ runProgramAsPlayer pp ph (programForState pch ho Operate)
    maybe
        -- TODO as much as the device wants!
        (doWorld (increaseTurn  *> setStatus "You aren't carrying anything in your hands.") >> Normal <$> world)
        pure
        mres
processNormal _ Input.OperateHeldOn = do
    mho ←  doWorld playerObject >>=
            whenCharacter
                (pure . slotWrapperItem . primaryHandSlot)
                (pure Nothing)
                . view o_state
    case mho of
        Nothing → do
            doWorld $ setStatus "You aren't carrying anything in your hands."
            Normal <$> world
        Just ho →
            -- TODO obtain target should happen inside Object Program, and then interpreter
            --      can either show UI for the player, or use "brain"/Simulation to select
            --      one for the NPC's
            -- Also, the range should be item's range
            withTarget (Distant (Range 30)) $ \v i → 
                doWorld (valueAt i <$> cellAt v) >>= \case
                    Nothing → do
                        doWorld (setStatus "Nothing there.")
                        Normal <$> world
                    Just o  → do
                        let so = view o_state o
                        -- TODO refactor, make cleaner
                        (pp, ph) ← doWorld playerPosition
                        doWorld playerObject >>=
                            whenCharacter (\ch → do {
                                 void $ runProgramAsPlayer pp ph (programForState ch ho (OperateOn so));
                                 runProgramAsPlayer v i (programForState ch so (OperateWith ho));
                                })
                                (Normal <$> world)
                                . view o_state
                        -- TODO which of the game states should take precedence?
processNormal _ Input.Talk = do
    gs ← withTarget Adjactened $ \v i → 
        doWorld (valueAt i <$> cellAt v) >>= \case
            Nothing → do
                doWorld $ do
                    setStatus "Trying to talk to someone, but there's no one there."
                    increaseTurn
                Normal <$> world
            Just o → doWorld (increaseTurn *> playerObject) >>=
                        whenCharacter (runProgramAsPlayer v i . program o) (Normal <$> world) . view o_state
    case gs of
        (Conversation _ ps cn) → conversationUpdateUi (view ch_name <$> ps) cn
        _ → pure ()
    pure gs
    where
        program o ch = programForState ch (view o_state o) Talk
processNormal _ Input.InventorySheet = do
    itemList ← whenCharacter listOfItemsFromContainers [] . view o_state <$> doWorld playerObject
    --itemList ← fromCharacter listOfItemsFromContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fromJustNote "invsheet" . valueAt ix <$> cellAt pp)) <$> world
    setScroll (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
    InventoryUI <$> world
processNormal _ Input.CharacterSheet =
    SkillsUI <$> world <*> pure carla
processNormal _ Input.GiveCommand = do
    teamChars ← doWorld (fmap (fromJust . maybeCharacter . view o_state) <$> teamObjects)
    if not (null teamChars)
        then do
            let xs = zip choiceChs (view ch_name <$> teamChars)
            withChoice xs $ \i →
                -- TODO upgrade to data
                let xs2 = zip choiceChs ["Move", "Operate"]
                in  withChoice xs2 $ \i2 → do
                    doWorld $
                        setStatus ("Ordering " <> view ch_name (teamChars `at` i) <> " to " <> (snd $ xs2 `at` i2))
                    Normal <$> world
        else do
            doWorld $
                setStatus "You currently have no team."
            Normal <$> world
processNormal _ Input.SwitchToHud =
    HudTeam <$> world <*> pure 0



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

processExamination ∷ (GameAPI g, RenderAPI g, Monad g) ⇒ String → Input.UIEvent → g (GameState g)
processExamination d Input.MoveUp   = doScroll scrollUp   >> Examination <$> world <*> pure d
processExamination d Input.MoveDown = doScroll scrollDown >> Examination <$> world <*> pure d
processExamination _ _              = Normal <$> world

--------------------------------------------------------------------------------

processHudTeam ∷ (GameAPI g, Monad g) ⇒ DesignData → Int → Input.UIEvent → g (GameState g)
processHudTeam _ _ Input.TabNext =
    HudMessages <$> world
processHudTeam _ _ Input.TabPrevious =
    HudWatch <$> world <*> pure 0 <*> pure 0
processHudTeam _ i Input.MoveUp =
    HudTeam <$> world <*> pure (max 0 (i - 3))
processHudTeam _ i Input.MoveDown = do
    tp ← doWorld (min (i + 3) . genericLength <$> teamPositions)
    HudTeam <$> world <*> pure tp 
processHudTeam _ i Input.MoveLeft =
    HudTeam <$> world <*> pure (max 0 (i - 1))
processHudTeam _ i Input.MoveRight = do
    tp ← doWorld (min (i + 1) . genericLength <$> teamPositions)
    HudTeam <$> world <*> pure tp
processHudTeam _ i Input.SelectChoice = do
    tm ← completeTeam <$> world
    SkillsUI <$> world <*> pure (tm `at` i)
processHudTeam _ _ Input.Back =
    Normal <$> world

--------------------------------------------------------------------------------

processHudMessages ∷ (GameAPI g, Monad g) ⇒ Input.UIEvent → g (GameState g)
processHudMessages Input.TabNext =
    HudWatch <$> world <*> pure 0 <*> pure 0
processHudMessages Input.TabPrevious =
    HudTeam <$> world <*> pure 0
processHudMessages Input.MoveUp =
    -- TODO scroll
    HudMessages <$> world
processHudMessages Input.MoveDown =
    -- TODO scroll
    HudMessages <$> world
processHudMessages Input.SelectChoice =
    -- TODO use scroll window to show log
    HudMessages <$> world
processHudMessages Input.Back =
    Normal <$> world
processHudMessages _ =
    HudMessages <$> world

--------------------------------------------------------------------------------

processHudWatch ∷ (GameAPI g, Monad g) ⇒ Int → Int → Input.UIEvent → g (GameState g)
processHudWatch _ _ Input.TabNext =
    HudTeam <$> world <*> pure 0
processHudWatch _ _ Input.TabPrevious =
    HudMessages <$> world
processHudWatch t b Input.MoveUp =
    HudWatch <$> world <*> pure (t + 1) <*> pure b
processHudWatch t b Input.MoveDown =
    HudWatch <$> world <*> pure (t - 1) <*> pure b
processHudWatch t b Input.MoveLeft =
    HudWatch <$> world <*> pure t <*> pure ((b - 1) `mod` 3)
processHudWatch t b Input.MoveRight =
    HudWatch <$> world <*> pure t <*> pure ((b + 1) `mod` 3)
processHudWatch t b Input.SelectChoice = do
    doWorld $ setStatus ("Pushing button: " <> show b)
    HudWatch <$> world <*> pure t <*> pure b
processHudWatch _ _ Input.Back =
    Normal <$> world

--------------------------------------------------------------------------------

processConversation ∷ (GameAPI g, RenderAPI g, Monad g) ⇒ NonEmpty DreamnetCharacter → Free (ConversationF States) () → Input.UIEvent → g (GameState g)
processConversation ps (Free (CName i fn)) e =
    let cname = view ch_name $ ps !! i
    in  processConversation ps (fn cname) e
processConversation ps (Free (CLastname i fn)) e =
    let clastname = view ch_lastName $ ps !! i
    in  processConversation ps (fn clastname) e
processConversation ps (Free (CNick i fn)) e =
    let cnick = view ch_lastName $ ps !! i
    in  processConversation ps (fn cnick) e

processConversation ps cn@(Free CTalk{}) Input.MoveUp = do
    doScroll scrollUp
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps cn@(Free CTalk{}) Input.MoveDown = do
    doScroll scrollDown
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps (Free (CTalk _ _ n)) Input.SelectChoice = do
    conversationUpdateUi (view ch_nickName <$> ps) n
    Conversation <$> world <*> pure ps <*> pure n
processConversation ps cn@(Free CTalk{}) _ =
    Conversation <$> world <*> pure ps <*> pure cn

processConversation ps cn@(Free CDescribe{}) Input.MoveUp = do
    doScroll scrollUp 
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps cn@(Free CDescribe{}) Input.MoveDown = do
    doScroll scrollDown
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps (Free (CDescribe _ n)) Input.SelectChoice = do
    conversationUpdateUi (view ch_nickName <$> ps) n
    Conversation <$> world <*> pure ps <*> pure n
processConversation ps cn@(Free CDescribe{}) _ =
    Conversation <$> world <*> pure ps <*> pure cn

processConversation ps cn@(Free CReceiveItem{}) Input.MoveUp = do
    doScroll scrollUp
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps cn@(Free CReceiveItem{}) Input.MoveDown = do
    doScroll scrollDown
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps (Free (CReceiveItem _ o n)) Input.SelectChoice = do
    -- TODO incorrect, only player now receives items!
    doWorld $ changePlayer $ o_state %~ withCharacter (pickUp o)
    conversationUpdateUi (view ch_nickName <$> ps) n
    Conversation <$> world <*> pure ps <*> pure n
processConversation ps cn@(Free CReceiveItem{}) _ =
    Conversation <$> world <*> pure ps <*> pure cn

--processConversation participants (Free (CChoice cs fn)) e =
--    Conversation . fn <$> askChoice (zip3 choiceChs cs [0..])
processConversation ps cn@(Free CChoice{}) Input.MoveUp = do
    doChoice selectPrevious
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps cn@(Free CChoice{}) Input.MoveDown = do
    doChoice selectNext
    Conversation <$> world <*> pure ps <*> pure cn
processConversation ps (Free (CChoice _ fn)) Input.SelectChoice = do
    n ← fn <$> currentChoice
    conversationUpdateUi (view ch_nickName <$> ps) n
    Conversation <$> world <*> pure ps <*> pure n
processConversation ps cn@(Free CChoice{}) _ =
    Conversation <$> world <*> pure ps <*> pure cn

processConversation _ _ _ =
    Normal <$> world


conversationUpdateUi ∷ (GameAPI g, RenderAPI g, Monad g, Show o) ⇒ NonEmpty String → Free (ConversationF o) a → g ()
conversationUpdateUi _ (Free (CChoice os _)) = do
    (p, s) ← (,) <$> positionFor 0 <*> conversationSize
    setChoice (newChoiceData p s os)
conversationUpdateUi nms (Free (CTalk i t _)) = do
    (p, s) ← (,) <$> positionFor i <*> conversationSize
    setScroll (newScrollData p s (toList nms `atMay` i) t)
conversationUpdateUi _ (Free (CDescribe t _)) = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    setScroll (newScrollData p s Nothing t)
conversationUpdateUi nms (Free (CReceiveItem i o _)) = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    let t = nms !! i <> " received " <> show o
    setScroll (newScrollData p s Nothing t)
conversationUpdateUi _ _ =
    pure ()


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
processComputerOperation ∷ (GameAPI g, Monad g) ⇒ (V2 Int, Int) → ComputerData → Input.InteractionEvent → g (GameState g)
processComputerOperation (v,ix) cd (Input.PassThrough '\n') =
    ComputerOperation <$> world <*> pure (v,ix) <*> pure (snd $ runComputer commitInput cd)
processComputerOperation (v,ix) cd (Input.PassThrough '\b') =
    ComputerOperation <$> world <*> pure (v,ix) <*> pure (snd $ runComputer backspace cd)
processComputerOperation (v,ix) cd (Input.PassThrough c) =
    ComputerOperation <$> world <*> pure (v,ix) <*> pure (snd $ runComputer (typeIn c) cd)
processComputerOperation (v,ix) cd Input.BackOut = do
    doWorld $
        modifyObjectAt v ix (pure . set o_state (Computer cd))
    Normal <$> world

--------------------------------------------------------------------------------

processInventoryUI ∷ (GameAPI g, RenderAPI g, Monad g) ⇒ Input.UIEvent → g (GameState g)
processInventoryUI Input.MoveUp = do
    doScroll scrollUp
    InventoryUI <$> world
processInventoryUI Input.MoveDown = do
    doScroll scrollDown
    InventoryUI <$> world
processInventoryUI _ =
    Normal <$> world

--------------------------------------------------------------------------------

processSkillsUI ∷ (GameAPI g, Monad g) ⇒ DreamnetCharacter → Input.UIEvent → g (GameState g)
processSkillsUI ch Input.TabNext =
    EquipmentUI <$> world <*> pure ch
processSkillsUI ch Input.TabPrevious =
    EquipmentUI <$> world <*> pure ch
processSkillsUI _ Input.Back =
    Normal <$> world
processSkillsUI ch _ =
    SkillsUI <$> world <*> pure ch

--------------------------------------------------------------------------------

processEquipmentUI ∷ (GameAPI g, Monad g) ⇒ DreamnetCharacter → Input.UIEvent → g (GameState g)
processEquipmentUI ch Input.TabNext =
    SkillsUI <$> world <*> pure ch
processEquipmentUI ch Input.TabPrevious =
    SkillsUI <$> world <*> pure ch
processEquipmentUI _ Input.Back =
    Normal <$> world
processEquipmentUI ch _ =
    EquipmentUI <$> world <*> pure ch

--------------------------------------------------------------------------------

processAdjactenedTargetSelection ∷ (GameAPI g, Monad g) ⇒ V2 Int → Int → TargetActivationF g → Input.TargetEvent → g (GameState g)
processAdjactenedTargetSelection _  i f (Input.MoveReticule v) = TargetSelectionAdjactened <$> world <*> pure v <*> pure i <*> pure f
processAdjactenedTargetSelection tp i f Input.LowerTarget      = TargetSelectionAdjactened <$> world <*> pure tp <*> pure (max 0 (i - 1)) <*> pure f
processAdjactenedTargetSelection tp i f Input.HigherTarget     = TargetSelectionAdjactened <$> world <*> pure tp <*> (min (i + 1) <$> (maxCellIndex . (tp+) . fst =<< doWorld playerPosition)) <*> pure f
processAdjactenedTargetSelection tp i f Input.NextTarget       = TargetSelectionAdjactened <$> world <*> pure tp <*> pure i <*> pure f
processAdjactenedTargetSelection tp i f Input.PreviousTarget   = TargetSelectionAdjactened <$> world <*> pure tp <*> pure i <*> pure f
processAdjactenedTargetSelection tp i f Input.ConfirmTarget    = doWorld (fst <$> playerPosition) >>= \v → runWithTarget f (tp + v) i
processAdjactenedTargetSelection _  _ _ Input.CancelTargeting  = Normal <$> world

--------------------------------------------------------------------------------

processDistantTargetSelection ∷ (GameAPI g, Monad g) ⇒ V2 Int → Int → TargetActivationF g → Input.TargetEvent → g (GameState g)
processDistantTargetSelection tp i f (Input.MoveReticule v) = TargetSelectionDistant <$> world <*> pure (tp + v) <*> pure i <*> pure f
processDistantTargetSelection tp i f Input.LowerTarget      = TargetSelectionDistant <$> world <*> pure tp <*> pure (max 0 (i - 1)) <*> pure f
processDistantTargetSelection tp i f Input.HigherTarget     = TargetSelectionDistant <$> world <*> pure tp <*> (min (i + 1) <$> maxCellIndex tp) <*> pure f
processDistantTargetSelection tp i f Input.NextTarget       = TargetSelectionDistant <$> world <*> pure tp <*> pure i <*> pure f
processDistantTargetSelection tp i f Input.PreviousTarget   = TargetSelectionDistant <$> world <*> pure tp <*> pure i <*> pure f
processDistantTargetSelection tp i f Input.ConfirmTarget    = runWithTarget f tp i
processDistantTargetSelection _  _ _ Input.CancelTargeting  = Normal <$> world

--------------------------------------------------------------------------------

processChoiceSelection ∷ (GameAPI g, Monad g) ⇒ [(Char, String)] → Int → ChoiceActivationF g → Input.ChoiceEvent → g (GameState g)
processChoiceSelection _ _ _ Input.CancelChoice = Normal <$> world
processChoiceSelection xs i f (Input.ChoiceCharacter c)
    | c == (fst $ xs `at` i) = runWithChoice f i
    | otherwise              = ChoiceSelection <$> world <*> pure xs <*> pure (fromJust . elemIndex c . fst . unzip $ xs) <*> pure f
        
--------------------------------------------------------------------------------

maxCellIndex ∷ (GameAPI g) ⇒ V2 Int → g Int
maxCellIndex v = doWorld (subtract 1 . length . cellValues <$> cellAt v)


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


--pickAChoice ∷ GameState → Input.UIEvent → StateT Game C.Curses Word
--pickAChoice gs Input.MoveUp = do
--    g_choiceWindow %= selectPrevious
--    use g_choiceWindow >>= lift . drawChoiceWindow
--    lift (Input.nextEvent gs) >>= pickAChoice gs
--pickAChoice gs Input.MoveDown = do
--    g_choiceWindow %= selectNext
--    use g_choiceWindow >>= lift . drawChoiceWindow
--    lift (Input.nextEvent gs) >>= pickAChoice gs
--pickAChoice _ Input.SelectChoice =
--    uses g_choiceWindow commit
--pickAChoice gs _ = do
--    lift (Input.nextEvent gs) >>= pickAChoice gs


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

render ∷ (RenderAPI r, Monad r) ⇒ GameState g → r ()
render (Quit _) =
    pure ()
render (Normal w) = do
    renderWorld w
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch False (evalWorld currentTurn w)
render (Examination _ _) = do
    updateUi clear
    updateUi =<< drawInformation

render (HudTeam w i) = do
    updateHud =<< drawTeamHud (completeTeam w) (Just i)
    updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch False (evalWorld currentTurn w)
render (HudMessages w) = do
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    updateHud =<< drawStatus True (evalWorld status w)
    updateHud =<< drawWatch False (evalWorld currentTurn w)
render (HudWatch w _ _) = do
    updateHud =<< drawTeamHud (completeTeam w) Nothing
    updateHud =<< drawStatus False (evalWorld status w)
    updateHud =<< drawWatch True (evalWorld currentTurn w)

render (Conversation _ _ (Free CChoice{})) = do
    updateUi clear
    updateUi =<< drawChoice
render (Conversation _ _ (Free CTalk{})) = do
    updateUi clear
    updateUi =<< drawInformation
render (Conversation _ _ (Free CDescribe{})) = do
    updateUi clear
    updateUi =<< drawInformation
render (Conversation _ _ (Free CReceiveItem{})) = do
    updateUi clear
    updateUi =<< drawInformation
render Conversation{} =
    pure ()

render (ComputerOperation _ _ cd) =
    updateUi $ do
        clear
        drawComputer cd

render (InventoryUI _) = do
    updateUi clear
    updateUi =<< drawInformation
render (SkillsUI _ ch) = do
    updateUi clear
    updateUi =<< drawCharacterSheet ch
render (EquipmentUI _ ch) = do
    updateUi clear
    updateUi =<< drawEquipmentDoll ch

render (TargetSelectionAdjactened w tp i _) = do
    let pp = evalWorld playerPosition w
    white ← style s_colorWhite
    green ← style s_colorGreen
    renderCellContentsToStatus w (fst pp + tp) i
    updateMain $ do
        RenderAction $ do
            C.setColor white
            (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) (fst pp)
                [ "yku"
                , "h.l"
                , "bjn"
                ]
        draw' (fst pp + tp) (charForVec tp) [C.AttributeColor green]
    where
        charForVec (V2 -1 -1) = 'y'
        charForVec (V2  0 -1) = 'k'
        charForVec (V2  1 -1) = 'u'
        charForVec (V2 -1  0) = 'h'
        charForVec (V2  1  0) = 'l'
        charForVec (V2 -1  1) = 'b'
        charForVec (V2  0  1) = 'j'
        charForVec (V2  1  1) = 'n'
        charForVec _          = '.'
render (TargetSelectionDistant w tp i _) = do
    renderWorld w
    green ← style s_colorGreen
    renderCellContentsToStatus w tp i
    updateMain $ draw' tp 'X' [C.AttributeColor green]
render (ChoiceSelection _ xs i _) = do
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


renderWorld ∷ (RenderAPI r, Monad r) ⇒ DreamnetWorld → r ()
renderWorld w =
    let m = evalWorld currentMap w
        d = views wm_data (fmap (fromMaybe (error "No last value in the map Cell!") . lastValue)) m
        v = evalWorld visibility w
    in  updateMain =<< drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) (width m) d v



renderCellContentsToStatus ∷ (RenderAPI r, Monad r) ⇒ DreamnetWorld → V2 Int → Int → r ()
renderCellContentsToStatus w v i = do
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




completeTeam ∷ World States Visibility → [DreamnetCharacter]
completeTeam w =
    let p = flip evalWorld w $ playerPosition >>= \t → 
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

