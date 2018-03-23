{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dreamnet.Dreamnet
where

import Safe                      (succSafe, predSafe, at)
import Control.Lens              (makeLenses, use, uses, view, views, (.=),
                                  assign, (+=), (%~), set)
import Control.Monad             (void)
import Control.Monad.Free        (Free)
import Control.Monad.Reader      (MonadReader)
import Control.Monad.State       (MonadState, StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.List                 (genericLength)
import Data.Foldable             (traverse_, find)
import Data.Maybe                (fromJust)
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
import Dreamnet.Entity
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
data Game = Game {
      _g_turn         ∷ Word
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
                       (playerPerson . (`characterForName` view dd_characters dd) <$>
                            [ "Carla"
                            , "Devin"
                            , "Hideo"
                            , "Phillipe"
                            , "Qaayenaat"
                            , "Annabelle"
                            ])
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
        ap ← use (g_world.w_active.e_position)
        doRender $ updateMain $ RenderAction $
            (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) ap $
                [ "yku"
                , "h.l"
                , "bjn"
                ]
        t ← GameM (lift Input.nextTargetSelectionEvent)
        uses (g_world.w_map) (valuesAt (ap + t)) >>=
            \case
                []  → error "Obtaining target on non-existent tile :-O"
                l   → pure (Just (ap + t, last l))

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
            traverse_ (\(i, (ch, str, _)) → drawString 2 (i + 2) (ch : " - " <> str)) $ zip [0..] lst
        t ← GameM (lift $ Input.nextAllowedCharEvent  (fst3 $ unzip3 lst))
        pure $ trd3 $ fromJust $ find ((== t) . fst3) lst 
        where
            fst3 (x, _, _) = x
            trd3 (_, _, x) = x

    runProgram dd v prg = do
        ms ← queryRenderer mainSize
        o  ← uses (g_world.w_map) (last . valuesAt v)
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
        moveActive v
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
        changeActive (withCharacter (ch_stance %~ predSafe))
    pure Normal
processNormal _ Input.LowerStance = do
    changeWorld $
        changeActive (withCharacter (ch_stance %~ succSafe))
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
                changeActive (withCharacter (pickUp (view o_state o)))
                deleteObject v o
    pure Normal
processNormal dd Input.UseHeld = do
    increaseTurn -- TODO as much as the device wants!
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "Nothing there."
        Just (v, o) → do
            -- TODO this should be much easier
            hv  ← view (w_active.e_position) <$> world
            mho ← fromCharacter (\ch → slottedItem $ view (ch_primaryHand ch) ch) Nothing . view (w_active.e_object) <$> world
            case mho of
                Nothing →
                    changeWorld $ setStatus "You aren't carrying anything in your hands."
                Just ho → do
                    let so = view o_state o
                    _ ← runProgram dd hv (programForState ho (OperateOn so))
                    _ ← runProgram dd v  (programForState so (OperateWith ho))
                    changeWorld $ setStatus $ "Operated " <> show ho <> " on " <> show so
                    pure ()
    pure Normal
processNormal _ Input.Wear = do
    -- TODO equipping stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    increaseTurn
    (side, slot) ← askChoice
        [ ('h', "Head",        (LeftSide,  Head))
        , ('t', "Torso",       (LeftSide,  Torso))
        , ('T', "Back",        (LeftSide,  Back))
        , ('b', "Belt",        (LeftSide,  Belt))
        , ('a', "Left arm",    (LeftSide,  Arm))
        , ('A', "Right arm",   (RightSide, Arm))
        , ('h', "Left thigh",  (LeftSide,  Thigh))
        , ('H', "Right thigh", (RightSide, Thigh))
        , ('s', "Left shin",   (LeftSide,  Shin))
        , ('S', "Right shin",  (RightSide, Shin))
        , ('f', "Left foot",   (LeftSide,  Foot))
        , ('F', "Right foot",  (RightSide, Foot))
        ]
    changeWorld $
        changeActive $
            withCharacter $
                \ch → case views (ch_primaryHand ch) slottedItem ch of
                       Nothing → ch
                       Just i  → equip RightSide Hand Nothing . equip side slot (Just i) $ ch
    pure Normal
processNormal _ Input.StoreIn = do
    increaseTurn

    containerList ← fromCharacter equippedContainers [] . view (w_active.e_object) <$> world
    let characters = "fdsahjklrewqyuiovcxzbnmtgp"
    container ← askChoice (zip3 characters ((\(SlotWrapper (Slot (Just (Clothes wi)))) → view wi_name wi) <$> containerList) containerList)

    changeWorld $
        changeActive $
            withCharacter $
                \ch → case views (ch_primaryHand ch) slottedItem ch of
                       Nothing → ch
                       Just i  → equip RightSide Hand Nothing . modEquipped eq_back (\(Slot (Just (Clothes wi))) → Slot (Just (Clothes (wi_storedItems %~ (++[i]) $ wi)))) $ ch
    pure Normal
processNormal dd Input.Examine = do
    increaseTurn
    obtainTarget >>= \case
        Just (v, o)  → do
            onHerself ← views (w_active.e_position) (==v) <$> world
            if onHerself
                then do
                    examineText ← views w_map desc <$> world
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
    itemList ← fromCharacter listOfItemsFromContainers [] . view (w_active.e_object) <$> world
    pure $ InventoryUI (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
    where
        listOfItemsFromContainers ∷ DreamnetCharacter → [String]
        listOfItemsFromContainers ch = concat $ makeItemList <$> equippedContainers ch
        -- TODO again, annoying. We don't know its "Clothes" inside Slot.
        --      why is my typing logic so bad here???
        makeItemList ∷ SlotWrapper States → [String]
        makeItemList (SlotWrapper (Slot (Just (Clothes wi)))) = _wi_name wi : (("- "<>) . show <$> _wi_storedItems wi)
        makeItemList (SlotWrapper (Slot Nothing))             = []
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
    HudTeam . min (i + 3) . genericLength . view w_team <$> world
processHudTeam _ i Input.MoveLeft =
    pure $ HudTeam (max 0 (i - 1))
processHudTeam _ i Input.MoveRight =
    HudTeam . min (i + 1) . genericLength . view w_team <$> world
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
        modifyObjectAt v ix (pure . set o_state (Computer cd))
    pure Normal


--------------------------------------------------------------------------------

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
--    pp ← use  (g_world.w_active.e_position)
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
render Normal                     w t = renderWorld w *> renderHud Normal (completeTeam w) t *> drawStatus Normal (view w_status w) >>= updateHud
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
    let t = view w_team w
    in  views w_active (fmap (views (e_object.o_state) (\(Person ch) → ch)) . (:t)) w


renderWorld ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ World States Visibility → r ()
renderWorld wrld =
    let w = views w_map width wrld
        d = views (w_map.wm_data) (fmap last) wrld
        v = view w_vis wrld
    in  drawMap ((\(Symbol ch) → ch) . view o_symbol) (view o_material) w d v >>= updateMain


renderHud ∷ (RenderAPI r, MonadReader RendererEnvironment r) ⇒ GameState → [DreamnetCharacter] → Word → r ()
renderHud gs tm t = drawHud gs tm t >>= updateHud

