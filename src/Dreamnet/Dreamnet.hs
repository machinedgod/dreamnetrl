{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dreamnet.Dreamnet
where

import Safe                      (succSafe, predSafe, at, fromJustNote)
import Control.Lens              (makeLenses, use, uses, view, views, (.=),
                                  assign, (+=), (%~), (^.), set)
import Control.Monad             (void)
import Control.Monad.Free        (Free)
import Control.Monad.Reader      (MonadReader)
import Control.Monad.State       (MonadState, StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.List                 (genericLength)
import Data.Foldable             (traverse_, find)
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2), _x, _y)

import qualified UI.NCurses  as C
import qualified Data.Vector as V    (fromList)
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import qualified Dreamnet.Input as Input
import Dreamnet.ScrollData
import Dreamnet.ChoiceData
import Dreamnet.World
import Dreamnet.Conversation
import Dreamnet.TileMap
import Dreamnet.TileData
import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.Visibility
import Dreamnet.Character
import Dreamnet.ObjectMonad

import Design.DesignAPI
import Design.ObjectPrograms
import Design.GameCharacters

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
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = newRenderEnvironment >>= \rdf →
    pure Game {
        _g_turn  = 0
      , _g_world = newWorld
                       (fromTileMap (view dd_startingMap dd) objectFromTile)
                       (playerPerson ( "Carla" `characterForName` view dd_characters dd))
      , _g_gameState    = Normal 
      , _g_rendererData = rdf
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
        objectFromTile t@(ttype → "Door") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = 1 `readBoolProperty` t
                h  = 5
                st = Door
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t@(ttype → "Stairs") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = True
                h  = 1
                st = Prop "Stairs"
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
        objectFromTile t@(ttype → "Item") = 
            let m  = "blue plastic"
                p  = True
                s  = True
                h  = 0
                st = Prop (1 `readStringProperty` t)
            in  Object (Symbol $ view t_char t) m p s h st
        objectFromTile t =
            error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object (Symbol '@') "metal" False True 3 . Person

--------------------------------------------------------------------------------

choiceChs ∷ [Char]
choiceChs = "fdsahjkltrewyuiopvcxzbnmFDSAHJKLTREWYUIOPVCXZBNM" -- q is not added to be able to back out


class GameAPI g where
    currentTurn     ∷ g Word
    increaseTurn    ∷ g ()
    nextEvent       ∷ C.Curses a → g a -- TODO type leak
    gameState       ∷ g GameState
    changeGameState ∷ (GameState → g GameState) → g GameState
    world           ∷ g (World States Visibility)
    changeWorld     ∷ WorldM States Visibility a → g a
    -- TODO change to withTarget to make more functional
    obtainTarget    ∷ g (Maybe (V2 Int, Object States))
    -- TODO offer abort!
    askChoice       ∷ [(Char, String, a)] → g a
    runProgram      ∷ DesignData → V2 Int → Free ObjectF () → g GameState
    doRender        ∷ RendererF () → g ()
    queryRenderer   ∷ RendererF a → g a


--------------------------------------------------------------------------------

newtype GameM a = GameM { runGameM ∷ StateT Game C.Curses a }
                deriving (Functor, Applicative, Monad, MonadState Game)
            

instance GameAPI GameM where
    currentTurn = use g_turn

    increaseTurn = g_turn += 1

    nextEvent = GameM . lift

    gameState = use g_gameState

    changeGameState f = use g_gameState >>= f >>= \g → assign g_gameState g >> pure g

    world = use g_world

    changeWorld m =
        uses g_world (runWorld m) >>= \(x, w') →
            g_world .= w' >>
                pure x

    obtainTarget = do
        ap ← uses g_world (evalWorld (horizontalCoord <$> playerPosition))
        doRender $ updateMain $ RenderAction $
            (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) ap $
                [ "yku"
                , "h.l"
                , "bjn"
                ]
        t ← GameM (lift Input.nextTargetSelectionEvent)
        uses g_world (evalWorld (valuesAt (ap + t))) >>=
            \case
                [] → error "Obtaining target on non-existent tile :-O"
                l  → pure (Just (ap + t, last l))

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

    runProgram dd v prg = do
        ms ← queryRenderer mainSize
        o  ← uses g_world (evalWorld (last <$> valuesAt v))
        changeWorld $ do
           (_, gs) ← runObjectMonadWorld dd ms prg v o
           updateVisible
           pure gs

    doRender r = use g_rendererData >>= \rd → GameM $ lift $ do
        rd `runRenderer` r
        C.render

    queryRenderer q = use g_rendererData >>= \rd → GameM $ lift $ rd `runRenderer` q
        
--------------------------------------------------------------------------------

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
    g ← newGame dd
    void $ flip execStateT g $ runGameM $ do
        changeWorld updateVisible
        gs ← gameState
        w  ← world
        t  ← currentTurn
        doRender (render gs w t)
        loopTheLoop dd

--------------------------------------------------------------------------------

loopTheLoop ∷ ∀ g. (GameAPI g, Monad g) ⇒ DesignData → g ()
loopTheLoop dd = do
    changeWorld (setStatus "")
    gs ← changeGameState gameStateFlow
    case gs of
        Quit → pure ()
        _    → do
            w  ← world
            t  ← currentTurn
            doRender (render gs w t)
            loopTheLoop dd
    where
        gameStateFlow ∷ GameState → g GameState
        gameStateFlow Quit                        = pure Quit
        gameStateFlow Normal                      = nextEvent Input.nextWorldEvent       >>= processNormal dd
        gameStateFlow (Examination et)            = nextEvent Input.nextUiEvent          >>= processExamination et
        gameStateFlow (HudTeam i)                 = nextEvent Input.nextUiEvent          >>= processHudTeam dd i
        gameStateFlow HudMessages                 = nextEvent Input.nextUiEvent          >>= processHudMessages
        gameStateFlow (HudWatch t b)              = nextEvent Input.nextUiEvent          >>= processHudWatch t b
        gameStateFlow (ConversationFlow cn sd)    = nextEvent Input.nextUiEvent          >>= processConversationFlow cn sd
        gameStateFlow (ConversationChoice cn cd)  = nextEvent Input.nextUiEvent          >>= processConversationChoice cn cd
        gameStateFlow (InventoryUI sd)            = nextEvent Input.nextUiEvent          >>= processInventoryUI sd
        gameStateFlow (SkillsUI  ch)              = nextEvent Input.nextUiEvent          >>= processSkillsUI ch
        gameStateFlow (EquipmentUI ch)            = nextEvent Input.nextUiEvent          >>= processEquipmentUI ch
        gameStateFlow (ComputerOperation v ix cd) = nextEvent Input.nextInteractionEvent >>= processComputerOperation v ix cd


fromCharacter ∷ (DreamnetCharacter → a) → a → Object States → a
fromCharacter f d = stateQuery . view o_state 
    where
        stateQuery (Person ch) = f ch
        stateQuery _           = d


withCharacter ∷ (DreamnetCharacter → DreamnetCharacter) → Object States → Object States
withCharacter f = o_state %~ stateMod
    where
        stateMod ∷ States → States
        stateMod (Person ch) = Person (f ch)
        stateMod x           = x


processNormal ∷ (GameAPI g, Monad g) ⇒ DesignData → Input.WorldEvent → g GameState
processNormal _ Input.Quit = do
    pure Quit
processNormal _ Input.SwitchToHud = do
    pure $ HudTeam 0
processNormal _ (Input.Move v) = do
    changeWorld $ do
        movePlayer v
        updateVisible
    increaseTurn
    pure Normal
processNormal _ Input.Wait = do
    changeWorld $ do
        setStatus "Waiting..."
        updateVisible
    --runProgram v (operationProgramForSymbol (view o_symbol o) $ AiTick)
    increaseTurn
    pure Normal
processNormal _ Input.HigherStance = do
    changeWorld $
        changePlayer (withCharacter (ch_stance %~ predSafe))
    pure Normal
processNormal _ Input.LowerStance = do
    changeWorld $
        changePlayer (withCharacter (ch_stance %~ succSafe))
    pure Normal
processNormal _ Input.Get = do
    increaseTurn
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "There's nothing here."
        Just (v, o) →
            changeWorld $ do
                -- Fugly
                -- One way around:
                -- o_state contains some kind of a 'pointer'
                -- into the structure that contains the type and correct state
                -- of the actual object.
                -- Second way:
                -- withState type of function that does something as long as the State
                -- is of the actual correct type, and does nothing if it isn't
                changePlayer (withCharacter (pickUp (view o_state o)))
                deleteFromCell v o
    pure Normal
processNormal dd Input.UseHeld = do
    increaseTurn -- TODO as much as the device wants!
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "Nothing there."
        Just (v, o) → do
            -- TODO this should be much easier
            mho ← fromCharacter (slotWrapperItem . primaryHandSlot) Nothing . evalWorld (playerPosition >>= fmap (fromJustNote "useHeld") . uncurry valueAt . unwrapWorldCoord) <$> world
            case mho of
                Nothing →
                    changeWorld $ setStatus "You aren't carrying anything in your hands."
                Just ho → do
                    hv  ← evalWorld (horizontalCoord <$> playerPosition) <$> world
                    let so = view o_state o
                    _ ← runProgram dd hv (programForState ho (OperateOn so))
                    _ ← runProgram dd v  (programForState so (OperateWith ho))
                    --changeWorld $ setStatus $ "Operated " <> show ho <> " on " <> show so
                    pure ()
    pure Normal
processNormal _ Input.Wear = do
    -- TODO equipping stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    increaseTurn
    (side, slot) ← askChoice
        [ ('h', "Head",        (Nothing,  Head))
        , ('t', "Torso",       (Nothing,  Torso))
        , ('T', "Back",        (Nothing,  Back))
        , ('b', "Belt",        (Nothing,  Belt))
        , ('a', "Left arm",    ((Just LeftSide),  Arm))
        , ('A', "Right arm",   ((Just RightSide), Arm))
        , ('h', "Left thigh",  ((Just LeftSide),  Thigh))
        , ('H', "Right thigh", ((Just RightSide), Thigh))
        , ('s', "Left shin",   ((Just LeftSide),  Shin))
        , ('S', "Right shin",  ((Just RightSide), Shin))
        , ('f', "Left foot",   ((Just LeftSide),  Foot))
        , ('F', "Right foot",  ((Just RightSide), Foot))
        ]
    changeWorld $
        changePlayer $
            withCharacter $
                \ch → case slotWrapperItem (primaryHandSlot ch) of
                       Nothing → ch
                       Just i  → modifySlotContent (Just RightSide) Hand (const Nothing)
                                 . modifySlotContent side slot (const (Just i)) $ ch
    pure Normal
processNormal _ Input.StoreIn = do
    -- TODO storing stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    increaseTurn
    containerList ← fromCharacter equippedContainers [] . evalWorld (playerPosition >>= fmap (fromJustNote "storeIn") . uncurry valueAt . unwrapWorldCoord) <$> world
    sw ← askChoice (zip3 choiceChs ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)
    changeWorld $
        changePlayer $
            withCharacter $
                \ch → case slotWrapperItem (primaryHandSlot ch) of
                       Nothing → ch
                       Just i  → modifySlotContent (Just RightSide) Hand (const Nothing)
                                 . modifySlotContent (slotWrapperOrientation sw) (slotWrapperType sw) (appendToContainer i) $ ch
    pure Normal
    where
        appendToContainer ∷ States → Maybe States → Maybe States
        appendToContainer i (Just (Clothes wi)) = Just $ Clothes (wi_storedItems %~ (++[i]) $ wi)
        appendToContainer _ x                   = x
processNormal _ Input.PullFrom = do
    -- TODO pulling stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    increaseTurn

    -- TODO make this single-step choice (show containers and items as tree)
    containerList ← fromCharacter equippedContainers [] . evalWorld (playerPosition >>= fmap (fromJustNote "pullfrom") . uncurry valueAt . unwrapWorldCoord) <$> world
    sw ← askChoice (zip3 choiceChs ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)

    let (Just (Clothes wi)) = slotWrapperItem sw
        itemList            = view wi_storedItems wi

    item ← askChoice (zip3 choiceChs (show <$> itemList) itemList)

    changeWorld $
        changePlayer $
            withCharacter $
                \ch → modifySlotContent
                        (slotWrapperOrientation (primaryHandSlot ch))
                        (slotWrapperType (primaryHandSlot ch))
                        (const (Just item))
                      . modifySlotContent
                        (slotWrapperOrientation sw)
                        (slotWrapperType sw)
                        (\(Just (Clothes wi)) → Just (Clothes (wi { _wi_storedItems = filter ((/=) item) (_wi_storedItems wi) })))
                      $ ch
    pure Normal
processNormal dd Input.Examine = do
    increaseTurn
    obtainTarget >>= \case
        Just (v, o)  → do
            onHerself ← (==v) . evalWorld (horizontalCoord <$> playerPosition) <$> world
            if onHerself
                then do
                    examineText ← evalWorld desc <$> world
                    pure $ Examination (newScrollData (V2 2 1) (V2 60 20) Nothing examineText)
                else
                    runProgram dd v (programForState (view o_state o) Examine)
        Nothing → do
            changeWorld $ setStatus "There's nothing there."
            pure Normal
processNormal dd Input.Operate = do
    -- TODO as much as operation program wants!
    increaseTurn
    obtainTarget >>= \case
        Nothing → do
            changeWorld $ setStatus "There's nothing here."
            pure Normal
        Just (v, o) →
            runProgram dd v (programForState (view o_state o) Operate)
processNormal dd Input.Talk = do
    increaseTurn
    obtainTarget >>= \case
        Nothing → do
            changeWorld $ setStatus "Trying to talk to someone, but there's no one there."
            pure Normal
        Just (v, o) → do
            runProgram dd v (programForState (view o_state o) Talk)
processNormal _ Input.InventorySheet = do
    itemList ← fromCharacter listOfItemsFromContainers [] . evalWorld (playerPosition >>= fmap (fromJustNote "invsheet") . uncurry valueAt . unwrapWorldCoord) <$> world
    pure $ InventoryUI (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
processNormal dd Input.CharacterSheet =
    pure $ SkillsUI (characterForName "Carla" (view dd_characters dd))


processExamination ∷ (GameAPI g, Monad g) ⇒ ScrollData → Input.UIEvent → g GameState
processExamination sd Input.MoveUp =
    pure $ Examination (scrollUp sd)
processExamination sd Input.MoveDown =
    pure $ Examination (scrollDown sd)
processExamination _ _ =
    pure Normal


processHudTeam ∷ (GameAPI g, Monad g) ⇒ DesignData → Int → Input.UIEvent → g GameState
processHudTeam _ _ Input.TabNext =
    pure HudMessages
processHudTeam _ _ Input.TabPrevious =
    pure (HudWatch 0 0)
processHudTeam _ i Input.MoveUp =
    pure $ HudTeam (max 0 (i - 3))
processHudTeam _ i Input.MoveDown =
    HudTeam . min (i + 3) . genericLength . evalWorld team <$> world
processHudTeam _ i Input.MoveLeft =
    pure $ HudTeam (max 0 (i - 1))
processHudTeam _ i Input.MoveRight =
    HudTeam . min (i + 1) . genericLength . evalWorld team <$> world
processHudTeam _ i Input.SelectChoice = do
    tm ← completeTeam <$> world
    pure $ SkillsUI (at tm i)
processHudTeam _ _ Input.Back =
    pure Normal


processHudMessages ∷ (GameAPI g, Monad g) ⇒ Input.UIEvent → g GameState
processHudMessages Input.TabNext =
    pure (HudWatch 0 0)
processHudMessages Input.TabPrevious =
    pure (HudTeam 0)
processHudMessages Input.MoveUp =
    -- TODO scroll
    pure HudMessages
processHudMessages Input.MoveDown =
    -- TODO scroll
    pure HudMessages
processHudMessages Input.SelectChoice =
    -- TODO use scroll window to show log
    pure HudMessages
processHudMessages Input.Back =
    pure Normal
processHudMessages _ =
    pure HudMessages


processHudWatch ∷ (GameAPI g, Monad g) ⇒ Int → Int → Input.UIEvent → g GameState
processHudWatch _ _ Input.TabNext =
    pure (HudTeam 0)
processHudWatch _ _ Input.TabPrevious =
    pure HudMessages
processHudWatch t b Input.MoveUp =
    pure $ HudWatch (t + 1) b
processHudWatch t b Input.MoveDown =
    pure $ HudWatch (t - 1) b
processHudWatch t b Input.MoveLeft =
    pure $ HudWatch t ((b - 1) `mod` 3)
processHudWatch t b Input.MoveRight =
    pure $ HudWatch t ((b + 1) `mod` 3)
processHudWatch t b Input.SelectChoice = do
    changeWorld $ setStatus ("Pushing button: " <> show b)
    pure $ HudWatch t b
processHudWatch _ _ Input.Back =
    pure Normal


processConversationFlow ∷ (GameAPI g, Monad g) ⇒ ConversationNode → ScrollData → Input.UIEvent → g GameState
processConversationFlow cn sd Input.MoveUp =
    pure $ ConversationFlow cn (scrollUp sd)
processConversationFlow cn sd Input.MoveDown =
    pure $ ConversationFlow cn (scrollDown sd)
processConversationFlow cn _ Input.SelectChoice =
    queryRenderer mainSize >>= \ms →
        pure $ createConversationState ms (advance cn)
processConversationFlow cn sd _ =
    pure $ ConversationFlow cn sd


processConversationChoice ∷ (GameAPI g, Monad g) ⇒ ConversationNode → ChoiceData → Input.UIEvent → g GameState
processConversationChoice cn cd Input.MoveUp =
    pure $ ConversationChoice cn (selectPrevious cd)
processConversationChoice cn cd Input.MoveDown =
    pure $ ConversationChoice cn (selectNext cd)
processConversationChoice cn cd Input.SelectChoice = do
    queryRenderer mainSize >>= \ms →
        pure $ createConversationState ms (pick cn $ fromIntegral $ view cd_currentSelection cd)
processConversationChoice cn cd _ =
    pure $ ConversationChoice cn cd


processInventoryUI ∷ (GameAPI g, Monad g) ⇒ ScrollData → Input.UIEvent → g GameState
processInventoryUI sd Input.MoveUp =
    pure $ InventoryUI (scrollUp sd)
processInventoryUI sd Input.MoveDown =
    pure $ InventoryUI (scrollDown sd)
processInventoryUI _ _ =
    pure Normal


processSkillsUI ∷ (GameAPI g, Monad g) ⇒ DreamnetCharacter → Input.UIEvent → g GameState
processSkillsUI ch Input.TabNext =
    pure $ EquipmentUI ch
processSkillsUI ch Input.TabPrevious =
    pure $ EquipmentUI ch
processSkillsUI _ Input.Back =
    pure Normal
processSkillsUI ch _ =
    pure $ SkillsUI ch


processEquipmentUI ∷ (GameAPI g, Monad g) ⇒ DreamnetCharacter → Input.UIEvent → g GameState
processEquipmentUI ch Input.TabNext =
    pure $ SkillsUI ch
processEquipmentUI ch Input.TabPrevious =
    pure $ SkillsUI ch
processEquipmentUI _ Input.Back =
    pure Normal
processEquipmentUI ch _ =
    pure $ EquipmentUI ch


-- TODO this should somehow be a part of computer ObjectAPI code, not here!
-- Note: if I make ability to set the flow function, rather than just gamestate
-- (setting gamestate should probably be an specialization of setting the flow function)
-- at Dreamnet:245 from ObjectAPI, this'll be it.
processComputerOperation ∷ (GameAPI g, Monad g) ⇒ V2 Int → Int → ComputerData → Input.InteractionEvent → g GameState
processComputerOperation v ix cd (Input.PassThrough '\n') = do
    pure $ ComputerOperation v ix (snd $ runComputer commitInput cd)
processComputerOperation v ix cd (Input.PassThrough '\b') = do
    pure $ ComputerOperation v ix (snd $ runComputer backspace cd)
processComputerOperation v ix cd (Input.PassThrough c) = do
    pure $ ComputerOperation v ix (snd $ runComputer (typeIn c) cd)
processComputerOperation v ix cd Input.BackOut = do
    changeWorld $
        modifyObjectAt (WorldCoord (v, ix)) (pure . set o_state (Computer cd))
    pure Normal


--------------------------------------------------------------------------------

equippedContainers ∷ (ItemTraits i) ⇒ Character i c f → [SlotWrapper i]
equippedContainers = filter containers . equippedSlots
    where
        containers (SlotWrapper s) = views s_item (fromMaybe False . fmap isContainer) s


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

--withAimOrElse ∷ (V2 Int → [Object] → StateT Game C.Curses a) → StateT Game C.Curses a → StateT Game C.Curses a
--withAimOrElse f e = fromMaybe e <=< runMaybeT $ do
--    v  ← MaybeT (use g_aim)
--    os ← uses (g_world.w_map) (valuesAt v)
--    pure (f v os)
--
--
--withAim ∷ (V2 Int → [Object] → StateT Game C.Curses ()) → StateT Game C.Curses ()
--withAim f = withAimOrElse f (pure ())

--------------------------------------------------------------------------------

render ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ GameState → World States Visibility → Word → r ()
render Normal                     w t = renderWorld w *> renderHud Normal (completeTeam w) t *> drawStatus Normal (evalWorld status w) >>= updateHud
render (Examination sd)           _ _ = updateUi $ drawInformation sd
render (ComputerOperation _ _ cd) _ _ = updateUi $ drawComputer cd
render gs@(HudTeam _)             w t = renderHud gs (completeTeam w) t
render HudMessages                w t = renderHud HudMessages (completeTeam w) t
render gs@(HudWatch _ _)          w t = renderHud gs (completeTeam w) t
render (ConversationFlow _ sd)    _ _ = updateUi $ drawInformation sd
render (ConversationChoice _ cd)  _ _ = updateUi $ drawChoice cd
render (InventoryUI  sd)          _ _ = updateUi $ drawInformation sd
render (SkillsUI  ch)             _ _ = updateUi clear >> drawCharacterSheet ch >>= updateUi
render (EquipmentUI  ch)          _ _ = updateUi clear >> drawEquipmentDoll ch >>= updateUi
render Quit                       _ _ = pure ()


completeTeam ∷ World States Visibility → [DreamnetCharacter]
completeTeam w = 
    let p = flip evalWorld w $ playerPosition >>=
                               fmap (fromJustNote "complTeam") . uncurry valueAt . unwrapWorldCoord
    in  [(\(Person chp) → chp) (p ^. o_state)]
    {-
    let t = flip evalWorld w $ team >>= 
                               traverse (fmap fromJustNote . teamMemberPosition) >>=
                               traverse (fmap fromJustNote . uncurry valueAt . unwrapWorldCoord)
        p = flip evalWorld w $ playerPosition >>=
                               fmap fromJustNote . uncurry valueAt . unwrapWorldCoord
    in  (\(Person chp) → chp) (p ^. o_state) : ((\(Person tm) → tm) . view o_state <$> t)
    -}


renderWorld ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ World States Visibility → r ()
renderWorld w =
    let m = evalWorld currentMap w
        d = views wm_data (fmap last) m
        v = evalWorld visibility w
    in  drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) (width m) d v >>= updateMain


renderHud ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ GameState → [DreamnetCharacter] → Word → r ()
renderHud gs tm t = drawHud gs tm t >>= updateHud

