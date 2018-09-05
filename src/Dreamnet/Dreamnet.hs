{-# LANGUAGE UnicodeSyntax, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dreamnet.Dreamnet
( defaultDesignData
, launchDreamnet
) where


import Prelude            hiding (head, (!!))
import Safe                      (succSafe, predSafe, at, atMay, fromJustNote)
import Control.Lens              (view, views, (%~), (^.), set)
import Control.Monad             (void, (>=>))
import Control.Monad.Free        (Free(Free))
import Control.Monad.Trans       (lift)
import Control.Monad.State       (execState, modify, get)
import Control.Monad.Random      (MonadRandom)
import Control.Monad.IO.Class    (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Bifunctor            (bimap)
import Data.Semigroup            ((<>))
import Data.List                 (genericLength)
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
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.Character
import qualified Dreamnet.Engine.Input as Input

import Dreamnet.Rendering.Renderer
import Dreamnet.Game
import Dreamnet.ComputerModel

import Design.ObjectPrograms
import Design.GameCharacters
import Design.Items

--------------------------------------------------------------------------------

choiceChs ∷ String
choiceChs = "fdsahjkltrewyuiopvcxzbnmFDSAHJKLTREWYUIOPVCXZBNM" -- q is not added to be able to back out

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
        gameStateFlow (Normal _)                           = processNormal dd                        =<< nextEvent Input.nextWorldEvent
        gameStateFlow (Examination _ d)                    = processExamination d                    =<< nextEvent Input.nextUiEvent
        gameStateFlow (HudTeam _ i)                        = processHudTeam dd i                     =<< nextEvent Input.nextUiEvent
        gameStateFlow (HudMessages _)                      = processHudMessages                      =<< nextEvent Input.nextUiEvent
        gameStateFlow (HudWatch _ t b)                     = processHudWatch t b                     =<< nextEvent Input.nextUiEvent
        gameStateFlow (Conversation _ ps cn)               = processConversation ps cn               =<< nextEvent Input.nextUiEvent
        gameStateFlow (ComputerOperation _ p cd)           = processComputerOperation p cd           =<< nextEvent Input.nextInteractionEvent
        gameStateFlow (InventoryUI _)                      = processInventoryUI                      =<< nextEvent Input.nextUiEvent
        gameStateFlow (SkillsUI _ ch)                      = processSkillsUI ch                      =<< nextEvent Input.nextUiEvent
        gameStateFlow (EquipmentUI _ ch)                   = processEquipmentUI ch                   =<< nextEvent Input.nextUiEvent
        gameStateFlow (TargetSelectionAdjactened _ tp i f) = processAdjactenedTargetSelection tp i f =<< nextEvent Input.nextTargetSelectionEvent
        gameStateFlow (TargetSelectionDistant _ tp i f)    = processDistantTargetSelection tp i f    =<< nextEvent Input.nextTargetSelectionEvent


newGame ∷ DesignData → C.Curses (GameState g)
newGame dd = do
    sm  ← loadTileMap (view dd_dev_startingMap dd)
    pure $ Normal $ newWorld
                       (fromTileMap sm  objectFromTile)
                       (playerPerson ("Carla" `characterForName` view dd_characters dd))
    where
        -- 1) This *could* all be just a single thing. Object type really does not matter here.
        -- 2) Actually, it does, because Object carries a specific state, later used by object programs
        objectFromTile ∷ Tile → Object States
        objectFromTile t@(ttype → "Base") =
            let m  = "concrete"
                p  = 1 `readBoolProperty` t
                s  = 2 `readBoolProperty` t
                h  = 0
                st = (Prop "Floor" "A floor")
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Prop") =
            let m  = 4 `readStringProperty` t
                p  = 2 `readBoolProperty` t
                s  = 3 `readBoolProperty` t
                h  = 5 `readWordProperty` t
                st = Prop (1 `readStringProperty` t) (6 `readStringProperty` t)
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


processNormal ∷ (GameAPI g, RenderAPI g, Monad g) ⇒ DesignData → Input.WorldEvent → g (GameState g)
processNormal _ Input.Quit =
    Quit <$> world
processNormal _ (Input.Move v) = do
    doWorld $ do
        movePlayer v
        updateVisible
        increaseTurn
    Normal <$> world
processNormal _ (Input.MoveCamera v) = do
    moveCamera v
    Normal <$> world
processNormal _ Input.Wait = do
    doWorld $ do
        setStatus "Waiting..."
        updateVisible
        increaseTurn
    --runProgramAsPlayer v (operationProgramForSymbol (view o_symbol o) $ AiTick)
    Normal <$> world
processNormal _ Input.HigherStance = do
    doWorld $ changePlayer $
        o_state %~ withCharacter (ch_stance %~ predSafe)
    Normal <$> world
processNormal _ Input.LowerStance = do
    doWorld $ changePlayer $
        o_state %~ withCharacter (ch_stance %~ succSafe)
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
    (side, slot) ← askChoice
        [ ('h', "Head",        (Nothing,  Head))
        , ('t', "Torso",       (Nothing,  Torso))
        , ('T', "Back",        (Nothing,  Back))
        , ('b', "Belt",        (Nothing,  Belt))
        , ('a', "Left arm",    (Just LeftSide,  Arm))
        , ('A', "Right arm",   (Just RightSide, Arm))
        , ('h', "Left thigh",  (Just LeftSide,  Thigh))
        , ('H', "Right thigh", (Just RightSide, Thigh))
        , ('s', "Left shin",   (Just LeftSide,  Shin))
        , ('S', "Right shin",  (Just RightSide, Shin))
        , ('f', "Left foot",   (Just LeftSide,  Foot))
        , ('F', "Right foot",  (Just RightSide, Foot))
        ]
    doWorld $ do
        changePlayer $ o_state %~ withCharacter (tryWear side slot <*> slotWrapperItem . primaryHandSlot)
        increaseTurn
    Normal <$> world
    where
        tryWear side slot ch (Just i) = flip execState ch $ do
            modify (modifySlotContent (Just RightSide) Hand (const Nothing))
            modify (modifySlotContent side slot (const (Just i)))
        tryWear _ _ ch _  = ch
processNormal _ Input.StoreIn = do
    -- TODO storing stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    containerList ← doWorld $ do
        p ← playerPosition >>= \(pp, ix) → valueAt ix <$> cellAt pp
        pure $ whenCharacter equippedContainers [] $ view o_state (fromJustNote "storeIn" p)
    sw ← askChoice (zip3 choiceChs ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)
    doWorld $ do
        changePlayer $ o_state %~ withCharacter (tryStore sw <*> slotWrapperItem . primaryHandSlot)
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
        p ← playerPosition >>= \(pp, ix) → valueAt ix <$> cellAt pp
        pure $ whenCharacter equippedContainers [] $ view o_state (fromJustNote "storeIn" p)
    sw ← askChoice (zip3 choiceChs ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)

    let itemList = (\(Just (Clothes wi)) → view wi_storedItems wi) (slotWrapperItem sw)
    item ← askChoice (zip3 choiceChs (show <$> itemList) itemList)
    doWorld $ do
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
                            whenCharacter (runProgramAsPlayer v . program o) (Normal <$> world)
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
                        whenCharacter (runProgramAsPlayer v . program o) (Normal <$> world)
    where
        program o ch = programForState ch (view o_state o) Operate
processNormal _ Input.ExamineHeld = do
    mres ← runMaybeT $ do
        ho  ← lift (doWorld playerObject)
                    >>= MaybeT . pure . (maybeCharacter >=> slotWrapperItem . primaryHandSlot)
        pp  ← lift $ doWorld (fst <$> playerPosition)
        pch ← MaybeT $ doWorld (maybeCharacter <$> playerObject)
        lift $ runProgramAsPlayer pp (programForState pch ho Examine)
    maybe
        (doWorld (setStatus "You aren't carrying anything in your hands.") >> Normal <$> world)
        pure
        mres
processNormal _ Input.OperateHeld = do
    mres ← runMaybeT $ do
        ho  ← lift (doWorld playerObject)
                    >>= MaybeT . pure . (maybeCharacter >=> slotWrapperItem . primaryHandSlot)
        pp  ← lift $ doWorld (fst <$> playerPosition)
        pch ← MaybeT $ doWorld (maybeCharacter <$> playerObject)
        lift $ runProgramAsPlayer pp (programForState pch ho Operate)
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
                        pp ← doWorld (fst <$> playerPosition)
                        doWorld playerObject >>=
                            whenCharacter (\ch → do {
                                 void $ runProgramAsPlayer pp (programForState ch ho (OperateOn so));
                                 runProgramAsPlayer v  (programForState ch so (OperateWith ho));
                                })
                                (Normal <$> world)
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
                        whenCharacter (runProgramAsPlayer v . program o) (Normal <$> world)
    case gs of
        (Conversation _ ps cn) → conversationUpdateUi (view ch_name <$> ps) cn
        _ → pure ()
    pure gs
    where
        program o ch = programForState ch (view o_state o) Talk
processNormal _ Input.InventorySheet = do
    itemList ← whenCharacter listOfItemsFromContainers [] <$> doWorld playerObject
    --itemList ← fromCharacter listOfItemsFromContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fromJustNote "invsheet" . valueAt ix <$> cellAt pp)) <$> world
    setScroll (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
    InventoryUI <$> world
processNormal dd Input.CharacterSheet =
    SkillsUI <$> world <*> pure (characterForName "Carla" (view dd_characters dd))
processNormal _ Input.GiveCommand = do
    teamChars ← doWorld (fmap (fromJust . maybeCharacter) <$> teamObjects)
    if not (null teamChars)
        then do
            ch ← askChoice (zip3 choiceChs (view ch_name <$> teamChars) teamChars)
            -- TODO upgrade to data
            let actionList = [ "Move"
                             , "Operate"
                             ]
            ac ← askChoice (zip3 choiceChs (show <$> actionList) actionList)
            doWorld $
                setStatus ("Ordering " <> view ch_name ch <> " to " <> ac)
        else
            doWorld $
                setStatus "You currently have no team."
    Normal <$> world
processNormal _ Input.SwitchToHud =
    HudTeam <$> world <*> pure 0



programForState ∷ (ObjectAPI States o, Monad o) ⇒ DreamnetCharacter → States → InteractionType States → o ()
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
    let name = view ch_name $ ps !! i
    in  processConversation ps (fn name) e
processConversation ps (Free (CLastname i fn)) e =
    let lastname = view ch_lastName $ ps !! i
    in  processConversation ps (fn lastname) e
processConversation ps (Free (CNick i fn)) e =
    let nick = view ch_lastName $ ps !! i
    in  processConversation ps (fn nick) e

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
        cellContentsString = fmap (show . view o_state) . cellValues
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
    let p = flip evalWorld w $ playerPosition >>= \(pp, ix) →
                               fmap (fromJustNote "complTeam" . valueAt ix) (cellAt pp)
    in  [(\(Person chp) → chp) (p ^. o_state)]
    {-
    let t = flip evalWorld w $ team >>= 
                               traverse (fmap fromJustNote . teamMemberPosition) >>=
                               traverse (fmap fromJustNote . uncurry valueAt . unwrapWorldCoord)
        p = flip evalWorld w $ playerPosition >>=
                               fmap fromJustNote . uncurry valueAt . unwrapWorldCoord
    in  (\(Person chp) → chp) (p ^. o_state) : ((\(Person tm) → tm) . view o_state <$> t)
    -}

