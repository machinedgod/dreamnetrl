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
import Control.Monad.State       (execState, modify)
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

import Dreamnet.Engine.ConversationMonad
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.Character
import qualified Dreamnet.Engine.Input as Input

import Dreamnet.Rendering.Renderer hiding (moveCamera)

import Design.DesignAPI

import Design.ObjectPrograms
import Design.ComputerModel
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
        --, _dd_dev_startingMap = "./res/bar"
        , _dd_dev_startingMap = "./res/apartmentblock"
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
        doWorld updateVisible
        gs ← gameState
        w  ← world
        t  ← currentTurn
        doRender (render gs w t)
        loopTheLoop dd

--------------------------------------------------------------------------------

loopTheLoop ∷ ∀ g. (GameAPI g, Monad g) ⇒ DesignData → g ()
loopTheLoop dd = do
    doWorld (setStatus "")
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
        gameStateFlow Examination                 = nextEvent Input.nextUiEvent          >>= processExamination
        gameStateFlow (HudTeam i)                 = nextEvent Input.nextUiEvent          >>= processHudTeam dd i
        gameStateFlow HudMessages                 = nextEvent Input.nextUiEvent          >>= processHudMessages
        gameStateFlow (HudWatch t b)              = nextEvent Input.nextUiEvent          >>= processHudWatch t b
        gameStateFlow (Conversation cn)           = nextEvent Input.nextUiEvent          >>= processConversation cn
        gameStateFlow InventoryUI                 = nextEvent Input.nextUiEvent          >>= processInventoryUI
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


whenCharacter ∷ (DreamnetCharacter → g a) → g a → States → g a
whenCharacter f _ (Person ch) = f ch
whenCharacter _ d _           = d


processNormal ∷ (GameAPI g, Monad g) ⇒ DesignData → Input.WorldEvent → g GameState
processNormal _ Input.Quit = do
    pure Quit
processNormal _ Input.SwitchToHud = do
    pure $ HudTeam 0
processNormal _ (Input.Move v) = do
    doWorld $ do
        movePlayer v
        updateVisible
    increaseTurn
    pure Normal
processNormal _ (Input.MoveCamera v) = do
    moveCamera v
    pure Normal
processNormal _ Input.Wait = do
    doWorld $ do
        setStatus "Waiting..."
        updateVisible
    --runProgram v (operationProgramForSymbol (view o_symbol o) $ AiTick)
    increaseTurn
    pure Normal
processNormal _ Input.HigherStance = do
    doWorld $
        changePlayer (withCharacter (ch_stance %~ predSafe))
    pure Normal
processNormal _ Input.LowerStance = do
    doWorld $
        changePlayer (withCharacter (ch_stance %~ succSafe))
    pure Normal
processNormal _ Input.Get = do
    increaseTurn
    obtainTarget >>= \case
        Nothing →
            doWorld $ setStatus "There's nothing here."
        Just (v, o) →
            doWorld $ do
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
processNormal _ Input.UseHeld = do
    increaseTurn -- TODO as much as the device wants!
    mho ← doWorld playerObject >>=
            whenCharacter
                (pure . slotWrapperItem . primaryHandSlot)
                (pure Nothing)
    case mho of
        Nothing → do
            doWorld $ setStatus "You aren't carrying anything in your hands."
            pure Normal
        Just ho →
            -- TODO obtain target should happen inside Object Program, and then interpreter
            --      can either show UI for the player, or use "brain"/Simulation to select
            --      one for the NPC's
            obtainTarget >>= \case
                Nothing → do
                    doWorld $ setStatus "Nothing there."
                    pure Normal
                Just (v, o) → do
                    let so = view o_state o
                    pp ← doWorld (fst <$> playerPosition)
                    doWorld playerObject >>=
                        whenCharacter (\ch → do {
                             void $ runProgram pp (programForState ch ho (OperateOn so));
                             void $ runProgram v  (programForState ch so (OperateWith ho));
                            })
                            (pure ())
                    -- TODO which of the game states should take precedence?
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
    doWorld $
        changePlayer $
            withCharacter $
                \ch → case slotWrapperItem (primaryHandSlot ch) of
                       Nothing → ch
                       Just i  → flip execState ch $ do
                            modify (modifySlotContent (Just RightSide) Hand (const Nothing))
                            modify (modifySlotContent side slot (const (Just i)))
    pure Normal
processNormal _ Input.StoreIn = do
    -- TODO storing stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    increaseTurn
    containerList ← doWorld $ do
        (pp, ix) ← playerPosition
        cellAt pp >>= pure . fromCharacter equippedContainers [] . fromJustNote "storeIn" . valueAt ix
    sw ← askChoice (zip3 choiceChs ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)
    doWorld $
        changePlayer $
            withCharacter $
                \ch → case slotWrapperItem (primaryHandSlot ch) of
                       Nothing → ch
                       Just i  → flip execState ch $ do
                            modify (modifySlotContent (Just RightSide) Hand (const Nothing))
                            modify (modifySlotContent (slotWrapperOrientation sw) (slotWrapperType sw) (appendToContainer i))
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

    let itemList = (\(Just (Clothes wi)) → view wi_storedItems wi) (slotWrapperItem sw)
    item ← askChoice (zip3 choiceChs (show <$> itemList) itemList)

    doWorld $
        changePlayer $
            withCharacter $
                \ch → flip execState ch $ do
                    modify (modifySlotContent
                            (slotWrapperOrientation (primaryHandSlot ch))
                            (slotWrapperType (primaryHandSlot ch))
                            (const (Just item)))
                    modify (modifySlotContent
                            (slotWrapperOrientation sw)
                            (slotWrapperType sw)
                            (\(Just (Clothes wi)) → Just $ Clothes $ wi_storedItems %~ filter ((/=) item) $ wi))
    pure Normal
processNormal _ Input.Examine = do
    increaseTurn
    obtainTarget >>= \case
        Just (v, o)  → do
            onHerself ← (==v) . evalWorld (fst <$> playerPosition) <$> world
            if onHerself
                then
                    doWorld desc >>= \d →
                        doRenderData (setScroll (newScrollData (V2 2 1) (V2 60 20) Nothing d)) >>
                            pure Examination
                else
                    doWorld playerObject >>=
                        \(Person ch) → runProgram v (programForState ch (view o_state o) Examine)
        Nothing → do
            doWorld $ setStatus "There's nothing there."
            pure Normal
processNormal _ Input.Operate = do
    -- TODO as much as operation program wants!
    increaseTurn
    obtainTarget >>= \case
        Nothing → do
            doWorld $ setStatus "There's nothing here."
            pure Normal
        Just (v, o) →
            doWorld playerObject >>=
                \(Person ch) → runProgram v (programForState ch (view o_state o) Operate)
processNormal _ Input.Talk = do
    increaseTurn
    obtainTarget >>= \case
        Nothing → do
            doWorld $ setStatus "Trying to talk to someone, but there's no one there."
            pure Normal
        Just (v, o) → do
            doWorld playerObject >>=
                whenCharacter
                    (\ch → void $ runProgram v (programForState ch (view o_state o) Talk))
                    (pure ())
            -- Can't use whenCharacter because I need to work for prop mirror too
            case view o_state o of
                (Person ch) →
                    startConversation ch
                (Prop "Mirror") →
                    doWorld playerObject >>= whenCharacter startConversation (pure Normal)
                _ → do
                    doWorld $ setStatus "I can't talk to that."
                    pure Normal
    where
        startConversation ch = do
            pcName ← doWorld playerObject >>= whenCharacter
                    (pure . view ch_name)
                    (pure "Player character is not a Person! :-O")
            pure $ Conversation (runConversationF_temp
                    [ pcName , view ch_name ch ]
                    (view ch_conversation ch))
processNormal _ Input.InventorySheet = do
    itemList ← fromCharacter listOfItemsFromContainers [] . evalWorld (playerPosition >>= (\(pp, ix) → fmap (fromJustNote "invsheet" . valueAt ix) $ cellAt pp)) <$> world
    doRenderData (setScroll (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList))
    pure InventoryUI
processNormal dd Input.CharacterSheet =
    pure $ SkillsUI (characterForName "Carla" (view dd_characters dd))


processExamination ∷ (GameAPI g, Monad g) ⇒ Input.UIEvent → g GameState
processExamination Input.MoveUp = do
    doRenderData (doScroll scrollUp)
    pure Examination
processExamination Input.MoveDown = do
    doRenderData (doScroll scrollDown)
    pure Examination
processExamination _ =
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
    doWorld $ setStatus ("Pushing button: " <> show b)
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


programForState ∷ (ObjectAPI States o, Monad o) ⇒ DreamnetCharacter → States → InteractionType States → o ()
programForState _  (Prop "Door")   it = door it
programForState ch (Prop "Mirror") it = mirror ch it
programForState _  (Prop n)        it = genericProp n it
programForState _  (Camera f l)    it = camera f l it
programForState _  (Person ch)     it = person ch it
programForState _  (Computer cd)   it = computer cd it
programForState _  (Clothes wi)    it = genericClothes wi it
programForState _  (Weapon wpi)    it = genericWeapon wpi it
programForState _  (Ammo ami)      it = genericAmmo ami it
programForState _  (Throwable twi) it = genericThrowable twi it
programForState _  Empty           _  = pure ()


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


processInventoryUI ∷ (GameAPI g, Monad g) ⇒ Input.UIEvent → g GameState
processInventoryUI Input.MoveUp = do
    doRenderData (doScroll scrollUp)
    pure InventoryUI
processInventoryUI Input.MoveDown = do
    doRenderData (doScroll scrollDown)
    pure InventoryUI
processInventoryUI _ =
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
    doWorld $
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
render Normal w t = do
    renderWorld w
    drawTeamHud (completeTeam w) Nothing >>= updateHud
    drawStatus False (evalWorld status w) >>= updateHud
    drawWatch False t >>= updateHud
    where
        renderWorld ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ World States Visibility → r ()
        renderWorld w =
            let m = evalWorld currentMap w
                d = views wm_data (fmap (fromMaybe (error "No last value in the map Cell!") . lastValue)) m
                v = evalWorld visibility w
            in  drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) (width m) d v >>= updateMain
render Examination _ _ = do
    updateUi clear 
    drawInformation >>= updateUi
render (ComputerOperation _ _ cd) _ _ =
    updateUi $ do
        clear
        drawComputer cd
render (HudTeam i) w t = do
    drawTeamHud (completeTeam w) (Just i) >>= updateHud
    drawStatus False (evalWorld status w) >>= updateHud
    drawWatch False t >>= updateHud
render HudMessages w t = do
    drawTeamHud (completeTeam w) Nothing >>= updateHud
    drawStatus True (evalWorld status w) >>= updateHud
    drawWatch False t >>= updateHud
render (HudWatch _ _) w t = do
    drawTeamHud (completeTeam w) Nothing >>= updateHud
    drawStatus False (evalWorld status w) >>= updateHud
    drawWatch True t >>= updateHud
render (Conversation (ChoiceNode _ _)) _ _ = do
    updateUi clear
    drawChoice >>= updateUi
render (Conversation _) _ _ = do
    updateUi clear
    drawInformation >>= updateUi
render InventoryUI _ _ = do
    updateUi clear 
    drawInformation >>= updateUi
render (SkillsUI ch) _ _ = do
    updateUi clear 
    drawCharacterSheet ch >>= updateUi
render (EquipmentUI ch) _ _ = do
    updateUi clear 
    drawEquipmentDoll ch >>= updateUi
render Quit _ _ =
    pure ()


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


