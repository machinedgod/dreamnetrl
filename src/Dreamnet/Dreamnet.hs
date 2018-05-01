{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dreamnet.Dreamnet
( defaultDesignData
, launchDreamnet
) where

import Safe                      (succSafe, predSafe, at, atMay, fromJustNote)
import Control.Lens              (view, views, (%~), (^.), set)
import Control.Monad             (void)
import Control.Monad.Reader      (MonadReader)
import Control.Monad.Random      (MonadRandom)
import Control.Monad.IO.Class    (MonadIO)
import Data.Bifunctor            (bimap)
import Data.Semigroup            ((<>))
import Data.List                 (genericLength)
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2))

import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.Game

import Dreamnet.Engine.Conversation
import Dreamnet.ComputerModel
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.Character
import Dreamnet.Engine.Rendering.Renderer hiding (moveCamera)

import qualified Dreamnet.Engine.Input as Input

import Design.DesignAPI
import Design.ObjectPrograms
import Design.GameCharacters

--------------------------------------------------------------------------------

choiceChs ∷ [Char]
choiceChs = "fdsahjkltrewyuiopvcxzbnmFDSAHJKLTREWYUIOPVCXZBNM" -- q is not added to be able to back out

--------------------------------------------------------------------------------

defaultDesignData ∷ (MonadIO r, MonadRandom r) ⇒ r DesignData
defaultDesignData = do
    pure $
        DesignData {
          _dd_characters  = characterDictionary characters
        , _dd_dev_startingMap = "./res/bar"
        --, _dd_dev_startingMap = "./res/apartmentblock"
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
    g ← newGame dd
    void $ flip execGame g $ do
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
        gameStateFlow (Examination s)             = nextEvent Input.nextUiEvent          >>= processExamination s
        gameStateFlow (HudTeam i)                 = nextEvent Input.nextUiEvent          >>= processHudTeam dd i
        gameStateFlow HudMessages                 = nextEvent Input.nextUiEvent          >>= processHudMessages
        gameStateFlow (HudWatch t b)              = nextEvent Input.nextUiEvent          >>= processHudWatch t b
        gameStateFlow (Conversation cn)           = nextEvent Input.nextUiEvent          >>= processConversation cn
        gameStateFlow (InventoryUI is)            = nextEvent Input.nextUiEvent          >>= processInventoryUI is
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
processNormal _ (Input.MoveCamera v) = do
    moveCamera v
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
                modifyCell v (deleteFromCell o)
    pure Normal
processNormal dd Input.UseHeld = do
    increaseTurn -- TODO as much as the device wants!
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "Nothing there."
        Just (v, o) → do
            -- TODO this should be much easier
            mho ← fromCharacter (slotWrapperItem . primaryHandSlot) Nothing . evalWorld (playerPosition >>= (\(pp, ix) → fmap (fromJustNote "useHeld" . valueAt ix) $ cellAt pp)) <$> world
            case mho of
                Nothing →
                    changeWorld $ setStatus "You aren't carrying anything in your hands."
                Just ho → do
                    hv  ← evalWorld (fst <$> playerPosition) <$> world
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
    containerList ← fromCharacter equippedContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fmap (fromJustNote "storeIn" . valueAt ix) $ cellAt pp)) <$> world
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
    containerList ← fromCharacter equippedContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fmap (fromJustNote "pullfrom" . valueAt ix) $ cellAt pp)) <$> world
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
            onHerself ← (==v) . evalWorld (fst <$> playerPosition) <$> world
            if onHerself
                then do
                    examineText ← evalWorld desc <$> world
                    pure $ Examination examineText
                    --pure $ Examination (newScrollData (V2 2 1) (V2 60 20) Nothing examineText)
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
    itemList ← fromCharacter listOfItemsFromContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fmap (fromJustNote "invsheet" . valueAt ix) $ cellAt pp)) <$> world
    pure $ InventoryUI itemList
    --pure $ InventoryUI (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
processNormal dd Input.CharacterSheet =
    pure $ SkillsUI (characterForName "Carla" (view dd_characters dd))


processExamination ∷ (GameAPI g, Monad g) ⇒ String → Input.UIEvent → g GameState
processExamination s Input.MoveUp = do
    doRenderData (doScroll scrollUp)
    pure $ Examination s
processExamination s Input.MoveDown = do
    doRenderData (doScroll scrollDown)
    pure $ Examination s
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


processConversation ∷ (GameAPI g, Monad g) ⇒ ConversationNode → Input.UIEvent → g GameState
processConversation cn@(TalkNode _ _ _ _) e =
    processConversationFlow cn e
processConversation cn@(ChoiceNode _ _) e =
    processConversationChoice cn e
processConversation cn@(DescriptionNode _ _) e = 
    processConversationFlow cn e
processConversation End _ =
    pure Normal


processConversationFlow ∷ (GameAPI g, Monad g) ⇒ ConversationNode → Input.UIEvent → g GameState
processConversationFlow cn Input.MoveUp = do
    doRenderData (doScroll scrollUp)
    pure $ Conversation cn
processConversationFlow cn Input.MoveDown = do
    doRenderData (doScroll scrollDown)
    pure $ Conversation cn
processConversationFlow cn Input.SelectChoice = do
    let cn' = advance cn
    conversationUpdateUi cn'
    pure $ Conversation cn'
processConversationFlow cn _ =
    pure $ Conversation cn


processConversationChoice ∷ (GameAPI g, Monad g) ⇒ ConversationNode → Input.UIEvent → g GameState
processConversationChoice cn Input.MoveUp = do
    doRenderData (doChoice selectPrevious)
    pure $ Conversation cn
processConversationChoice cn Input.MoveDown = do
    doRenderData (doChoice selectNext)
    pure $ Conversation cn
processConversationChoice cn Input.SelectChoice = do
    cs ← fmap (view (rd_choiceData.cd_currentSelection)) queryRenderData 
    let cn' = pick cn (fromIntegral cs)
    conversationUpdateUi cn'
    pure $ Conversation cn'
processConversationChoice cn _ =
    pure $ Conversation cn


conversationUpdateUi ∷ (GameAPI g, Monad g) ⇒ ConversationNode → g ()
conversationUpdateUi (ChoiceNode os _) = do
    (p, s) ← (,) <$> positionFor 0 <*> conversationSize
    doRenderData (setChoice (newChoiceData p s os))
conversationUpdateUi (TalkNode t i nms _) = do
    (p, s) ← (,) <$> positionFor i <*> conversationSize
    doRenderData (setScroll (newScrollData p s (nms `atMay` fromIntegral i) t))
conversationUpdateUi (DescriptionNode txt _) = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    doRenderData (setScroll (newScrollData p s Nothing txt))
conversationUpdateUi _ =
    pure ()


positionFor ∷ (GameAPI g, Functor g) ⇒ Word → g (V2 Integer)
positionFor (fromIntegral → i) = doRender $
    (\s → fromMaybe (positions s `at` 0) $ (`atMay` i) $ positions s) <$> mainSize
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

conversationSize ∷ (GameAPI g, Functor g) ⇒ g (V2 Integer)
conversationSize = doRender (fmap (`div` 3) . uncurry V2 <$> mainSize)


processInventoryUI ∷ (GameAPI g, Monad g) ⇒ [String] → Input.UIEvent → g GameState
processInventoryUI lst Input.MoveUp = do
    doRenderData (doScroll scrollUp)
    pure $ InventoryUI lst
processInventoryUI lst Input.MoveDown = do
    doRenderData (doScroll scrollDown)
    pure $ InventoryUI lst
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
        modifyObjectAt v ix (pure . set o_state (Computer cd))
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
--    os ← uses (g_world.w_map) (cellAt v)
--    pure (f v os)
--
--
--withAim ∷ (V2 Int → [Object] → StateT Game C.Curses ()) → StateT Game C.Curses ()
--withAim f = withAimOrElse f (pure ())

--------------------------------------------------------------------------------

render ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ GameState → World States Visibility → Word → r ()
render Normal                          w t = renderWorld w *> renderHud Normal (completeTeam w) t *> drawStatus Normal (evalWorld status w) >>= updateHud
render (Examination _)                 _ _ = updateUi clear >> drawInformation >>= updateUi
render (ComputerOperation _ _ cd)      _ _ = updateUi (clear >> drawComputer cd)
render gs@(HudTeam _)                  w t = renderHud gs (completeTeam w) t
render HudMessages                     w t = renderHud HudMessages (completeTeam w) t
render gs@(HudWatch _ _)               w t = renderHud gs (completeTeam w) t
render (Conversation (ChoiceNode _ _)) _ _ = updateUi clear >> drawChoice >>= updateUi
render (Conversation _)                _ _ = updateUi clear >> drawInformation >>= updateUi
render (InventoryUI _)                 _ _ = updateUi clear >> drawInformation >>= updateUi
render (SkillsUI  ch)                  _ _ = updateUi clear >> drawCharacterSheet ch >>= updateUi
render (EquipmentUI  ch)               _ _ = updateUi clear >> drawEquipmentDoll ch >>= updateUi
render Quit                            _ _ = pure ()


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


renderWorld ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ World States Visibility → r ()
renderWorld w =
    let m = evalWorld currentMap w
        d = views wm_data (fmap (fromMaybe (error "No last value in the map Cell!") . lastValue)) m
        v = evalWorld visibility w
    in  drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) (width m) d v >>= updateMain


renderHud ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ GameState → [DreamnetCharacter] → Word → r ()
renderHud gs tm t = drawHud gs tm t >>= updateHud

