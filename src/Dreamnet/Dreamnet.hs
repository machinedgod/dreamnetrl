{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Safe                      (succSafe, predSafe, at)
import Control.Lens              (makeLenses, use, uses, view, views, (.=),
                                  assign, (+=), (%~))
import Control.Monad             (void, when)
import Control.Monad.Free        (Free)
import Control.Monad.State       (MonadState, StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Linear                    (V2(V2))

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
    , _g_keepRunning  ∷ Bool
    , _g_rendererData ∷ RendererEnvironment
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← initRenderer
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
      , _g_keepRunning  = True
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
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Door") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = 1 `readBoolProperty` t
                h  = 5
                st = Prop "Door"
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Stairs") =
            let m  = "wood"
                p  = 1 `readBoolProperty` t
                s  = True
                h  = 1
                st = Prop "Stairs"
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Prop") =
            let m  = 4 `readStringProperty` t
                p  = 2 `readBoolProperty` t
                s  = 3 `readBoolProperty` t
                h  = 5 `readWordProperty` t
                st = Prop (1 `readStringProperty` t)
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Person") = 
            let m  = "blue"
                p  = False
                s  = True
                h  = 3
                st = Person $ characterForName (1 `readStringProperty` t) (view dd_characters dd)
            in  Object '@' m p s h st
        objectFromTile (ttype → "Spawn") = -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
            objectFromTile (Tile '.' (V.fromList [ "Base", "True", "True" ]))
        objectFromTile t@(ttype → "Camera") =
            let m  = "green light"
                p  = True
                s  = True
                h  = 1
                st = Camera (Faction $ 1 `readStringProperty` t) 0
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Computer") =
            let m  = "metal"
                p  = False
                s  = True
                h  = 1
                st = Computer (ComputerData "" [])
            in  Object (view t_char t) m p s h st
        objectFromTile t@(ttype → "Item") = 
            let m  = "blue plastic"
                p  = True
                s  = True
                h  = 0
                st = Prop (1 `readStringProperty` t)
            in  Object (view t_char t) m p s h st
        objectFromTile t =
            error $ "Can't convert Tile type into Object: " <> show t
        -- TODO Errrrrr, this should be done through the tileset???

        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object '@' "metal" False True 3 . Person

--------------------------------------------------------------------------------

class GameAPI g


changeWorld ∷ WorldM States Visibility a → StateT Game C.Curses a
changeWorld m = do
    w ← use g_world
    let (x, w') = runWorld m w
    g_world .= w'
    pure x
    
--------------------------------------------------------------------------------

newtype GameM m a = GameM { runGameM ∷ StateT Game m a }
                  deriving (Functor, Applicative, Monad, MonadState Game)
            

instance GameAPI (GameM m a)
-- TODO implement everything

--------------------------------------------------------------------------------

launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

-- TODO This'll die when renderer gets refactored a bit
doRender ∷ RendererF () → StateT Game C.Curses ()
doRender r = use g_rendererData >>=
    lift . (`runRenderer` r) >>=
    assign g_rendererData . snd
        
--------------------------------------------------------------------------------

dreamnet ∷ DesignData → IO ()
dreamnet dd = C.runCurses $ do
    -- Init curses
    C.setRaw     True
    C.setEcho    False
    C.defaultWindow >>= (`C.setKeypad` True)
    void $ C.setCursorMode C.CursorInvisible
    g ← newGame dd
    void $ flip execStateT g $ do
        changeWorld updateVisible
        use g_gameState >>= render
        loopTheLoop dd


loopTheLoop ∷ DesignData → StateT Game C.Curses ()
loopTheLoop dd = do
    r ← use g_keepRunning
    when r $ do
        g_world.w_status .= ""
        use g_gameState >>= gameStateFlow >>= \gs → assign g_gameState gs >> render gs
        flipBackbuffer
        loopTheLoop dd
    where
        gameStateFlow Normal                        = lift Input.nextWorldEvent       >>= processNormal dd
        gameStateFlow (Examination et)              = lift Input.nextUiEvent          >>= processExamination et
        gameStateFlow (HudTeam i)                   = lift Input.nextUiEvent          >>= processHudTeam dd i
        gameStateFlow HudMessages                   = lift Input.nextUiEvent          >>= processHudMessages
        gameStateFlow (HudWatch t b)                = lift Input.nextUiEvent          >>= processHudWatch t b
        gameStateFlow (ConversationFlow ch cn sd)   = lift Input.nextUiEvent          >>= processConversationFlow ch cn sd
        gameStateFlow (ConversationChoice ch cn cd) = lift Input.nextUiEvent          >>= processConversationChoice ch cn cd
        gameStateFlow (InventoryUI sd)              = lift Input.nextUiEvent          >>= processInventoryUI sd
        gameStateFlow (SkillsUI  ch)                = lift Input.nextUiEvent          >>= processSkillsUI ch
        gameStateFlow (EquipmentUI ch)              = lift Input.nextUiEvent          >>= processEquipmentUI ch
        gameStateFlow (ComputerOperation cd)        = lift Input.nextInteractionEvent >>= processComputerOperation cd


processNormal ∷ DesignData → Input.WorldEvent → StateT Game C.Curses GameState
processNormal _ Input.Quit = do
    g_keepRunning .= False
    pure Normal
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
    changeWorld $ do
        changeActive (o_state %~ (\(Person ch) → Person (ch_stance %~ predSafe $ ch)))
    pure Normal
processNormal _ Input.LowerStance = do
    changeWorld $ do
        changeActive (o_state %~ (\(Person ch) → Person (ch_stance %~ succSafe $ ch)))
    pure Normal
processNormal _ Input.Get = do
    increaseTurn
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "There's nothing here."
        Just (v, o) →
            changeWorld $ do
                changeActive (o_state %~ (\(Person ch) → Person $ pickUp o ch))
                deleteObject v o
    pure Normal
processNormal _ Input.UseHeld = do
    increaseTurn -- TODO as much as the device wants!
    obtainTarget >>= \case
        Nothing →
            changeWorld $ setStatus "Nothing there."
        Just (_, _) →
            changeWorld $ setStatus "Need to figure out how to run programs for held Objects."
    pure Normal
processNormal _ Input.WearHeld = do
    increaseTurn
    changeWorld $ do
        changeActive
            (o_state %~
                (\(Person ch) → Person $
                    case views (ch_primaryHand ch) slottedItem ch of
                        Nothing → ch
                        Just i  → equip RightSide Hand Nothing . equip LeftSide Torso (Just i) $ ch)
                     )
    pure Normal
processNormal dd Input.Examine = do
    increaseTurn
    obtainTarget >>= \case
        Just (v, o)  → do
            onHerself ← uses (g_world.w_active.e_position) (==v)
            if onHerself
                then do
                    examineText ← uses (g_world.w_map) desc
                    pure $ Examination (newScrollData (V2 2 1) (V2 60 20) Nothing examineText)
                else
                    runProgram dd v (programForObject o Examine)
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
            runProgram dd v (programForObject o Operate)
processNormal dd Input.Talk = do
    increaseTurn
    obtainTarget >>= \case
        Nothing → do
            changeWorld $ setStatus "Trying to talk to someone, but there's no one there."
            pure Normal
        Just (v, o) → do
            runProgram dd v (programForObject o Talk)
processNormal _ Input.InventorySheet =
    pure $ InventoryUI (newScrollData (V2 1 1) (V2 60 30) (Just "Inventory sheet") "")
processNormal dd Input.CharacterSheet =
    pure $ SkillsUI  (characterForName "Carla" (view dd_characters dd))


processExamination ∷ ScrollData → Input.UIEvent → StateT Game C.Curses GameState
processExamination sd Input.MoveUp =
    pure $ Examination (scrollUp sd)
processExamination sd Input.MoveDown =
    pure $ Examination (scrollDown sd)
processExamination _ _ =
    pure $ Normal


processHudTeam ∷ DesignData → Int → Input.UIEvent → StateT Game C.Curses GameState
processHudTeam _ _ Input.TabNext =
    pure HudMessages
processHudTeam _ _ Input.TabPrevious =
    pure (HudWatch 0 0)
processHudTeam _ i Input.MoveUp =
    pure $ HudTeam (max 0 (i - 3))
processHudTeam _ i Input.MoveDown =
    HudTeam . min (i + 3) . fromIntegral . length <$> use (g_world.w_team) 
processHudTeam _ i Input.MoveLeft =
    pure $ HudTeam (max 0 (i - 1))
processHudTeam _ i Input.MoveRight =
    HudTeam . min (i + 1) . fromIntegral . length <$> use (g_world.w_team)
processHudTeam _ i Input.SelectChoice = do
    tm ← use (g_world.w_active) >>= \cal → uses (g_world.w_team) (cal:)
    pure $ views (e_object.o_state) (\(Person ch) → SkillsUI ch) (at tm i)
processHudTeam _ _ Input.Back =
    pure Normal


processHudMessages ∷ Input.UIEvent → StateT Game C.Curses GameState
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


processHudWatch ∷ Int → Int → Input.UIEvent → StateT Game C.Curses GameState
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


processConversationFlow ∷ DreamnetCharacter → ConversationNode → ScrollData → Input.UIEvent → StateT Game C.Curses GameState
processConversationFlow ch cn sd Input.MoveUp =
    pure $ ConversationFlow ch cn (scrollUp sd)
processConversationFlow ch cn sd Input.MoveDown =
    pure $ ConversationFlow ch cn (scrollDown sd)
processConversationFlow ch cn _ Input.SelectChoice = do
    pos ← lift (positionFor 0)
    siz ← lift conversationSize
    pure $ createConversationState pos siz ch (advance cn)
processConversationFlow ch cn sd _ =
    pure $ ConversationFlow ch cn sd


processConversationChoice ∷ DreamnetCharacter → ConversationNode → ChoiceData → Input.UIEvent → StateT Game C.Curses GameState
processConversationChoice ch cn cd Input.MoveUp =
    pure $ ConversationChoice ch cn (selectPrevious cd)
processConversationChoice ch cn cd Input.MoveDown =
    pure $ ConversationChoice ch cn (selectNext cd)
processConversationChoice ch cn cd Input.SelectChoice = do
    pos ← lift (positionFor 0)
    siz ← lift conversationSize
    pure $ createConversationState pos siz ch (pick cn $ fromIntegral $ view cd_currentSelection cd)
processConversationChoice ch cn cd _ =
    pure $ ConversationChoice ch cn cd


processInventoryUI ∷ ScrollData → Input.UIEvent → StateT Game C.Curses GameState
processInventoryUI sd Input.MoveUp =
    pure $ InventoryUI (scrollUp sd)
processInventoryUI sd Input.MoveDown =
    pure $ InventoryUI (scrollDown sd)
processInventoryUI _ _ =
    pure Normal


processSkillsUI ∷ DreamnetCharacter → Input.UIEvent → StateT Game C.Curses GameState
processSkillsUI ch Input.TabNext =
    pure $ EquipmentUI ch
processSkillsUI ch Input.TabPrevious =
    pure $ EquipmentUI ch
processSkillsUI _ Input.Back =
    pure Normal
processSkillsUI ch _ =
    pure $ SkillsUI ch


processEquipmentUI ∷ DreamnetCharacter → Input.UIEvent → StateT Game C.Curses GameState
processEquipmentUI ch Input.TabNext =
    pure $ SkillsUI ch
processEquipmentUI ch Input.TabPrevious =
    pure $ SkillsUI ch
processEquipmentUI _ Input.Back =
    pure Normal
processEquipmentUI ch _ =
    pure $ EquipmentUI ch


processComputerOperation ∷ ComputerData → Input.InteractionEvent → StateT Game C.Curses GameState
processComputerOperation cd (Input.PassThrough '\n') = do
    pure $ ComputerOperation (snd $ runComputer commitInput cd)
processComputerOperation cd (Input.PassThrough '\b') = do
    pure $ ComputerOperation (snd $ runComputer backspace cd)
processComputerOperation cd (Input.PassThrough c) = do
    pure $ ComputerOperation (snd $ runComputer (typeIn c) cd)
processComputerOperation _ Input.BackOut =
    pure Normal


--------------------------------------------------------------------------------

increaseTurn ∷ StateT Game C.Curses ()
increaseTurn = g_turn += 1


runProgram ∷ DesignData → V2 Int → Free ObjectF () → StateT Game C.Curses GameState
runProgram dd v prg = do
    o  ← uses (g_world.w_map) (last . valuesAt v)
    changeWorld $ do
       (_, gs) ← runObjectMonadWorld dd prg v o
       updateVisible
       pure gs


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


obtainTarget ∷ StateT Game C.Curses (Maybe (V2 Int, Object States))
obtainTarget = do
    --renderMessage "Select direction (h/j/k/l/y/u/b/n/.):"
    --lift C.render

    ap ← use (g_world.w_active.e_position)
    t  ← lift Input.nextTargetSelectionEvent
    uses (g_world.w_map) (valuesAt (ap + t)) >>=
        \case
            []  → error "Obtaining target on non-existent tile :-O"
            l   → pure (Just (ap + t, last l))

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

flipBackbuffer ∷ StateT Game C.Curses ()
flipBackbuffer = lift C.render


render ∷ GameState → StateT Game C.Curses ()
render Normal                      = renderNormal
render (Examination sd)            = doRender $ updateUi $ drawInformation sd
render (ComputerOperation cd)      = doRender $ updateUi $ drawComputer cd
render (HudTeam _)                 = renderNormal
render HudMessages                 = renderNormal
render (HudWatch _ _)              = renderNormal
render (ConversationFlow _ _ sd)   = doRender $ updateUi $ drawInformation sd
render (ConversationChoice _ _ cd) = doRender $ updateUi $ drawChoice cd
render (InventoryUI  sd)           = doRender $ updateUi $ drawInformation sd
render (SkillsUI  ch)              = doRender $ updateUi clear >> drawCharacterSheet ch >>= updateUi
render (EquipmentUI  ch)           = doRender $ updateUi clear >> drawEquipmentDoll ch >>= updateUi


renderNormal ∷ StateT Game C.Curses ()
renderNormal = do
    w  ← uses (g_world.w_map) width
    d  ← uses (g_world.w_map.wm_data) (fmap last)
    v  ← use (g_world.w_vis)
    s  ← use (g_world.w_status)

    gs ← use g_gameState
    tm ← use (g_world.w_team) >>= \t → uses (g_world.w_active) (fmap (views (e_object.o_state)  (\(Person ch) → ch)) . (:t))
    t  ← use g_turn
    
    doRender $ do
        drawMap (view o_symbol) (view o_material) w d v >>= updateMain
        drawHud gs tm t >>= updateHud
        drawStatus gs s >>= updateHud

