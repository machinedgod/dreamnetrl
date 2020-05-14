{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Safe                      (at)
import Control.Lens              (view, _Just, preview)
import Control.Lens.Operators
import Control.Applicative       (Alternative(empty))
import Control.Monad             (void)
import Control.Monad.Free        (Free(..))
import Control.Monad.Trans       (lift)
import Control.Monad.Except      (MonadError(throwError))
import Control.Monad.Random      (MonadRandom)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.List                 (genericLength, elemIndex)
import Data.Foldable             (traverse_)
import Data.Maybe                (fromJust, listToMaybe)
import Data.Singletons           (withSomeSing)
import Linear                    (V2(V2), V3(V3), _x, _y, _z, _xy)

import qualified Data.Map    as M
import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre (wrapMain, defaultParams, projectName,
                                      realMain, showError)

import Dreamnet.Engine.TileMap
import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Object
import qualified Dreamnet.Engine.Visibility as Vis
import qualified Dreamnet.Engine.Input      as Input
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration
import Dreamnet.Engine.Utils (maybeToError)

import Dreamnet.Rendering.Renderer
import Dreamnet.Game
import Dreamnet.ComputerModel

import Dreamnet.ProcessNormal
import Dreamnet.ProcessUI
import Dreamnet.ProcessTarget

import Design.GameCharacters
import Design.Items

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
                Nothing            → pure () -- Quit the game if we get no game state

        innerLoop ∷ GameState gs → DreamnetMonad (Maybe SomeGameState)
        innerLoop = \case
            gs@(StNormal w) → do
                renderNormal w
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMove _)            → pure (Just (SomeGS (processNormal gs ev)))
                    ev@(Input.SMoveCamera _)      → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SExamine             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SDescribeEnvironment → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SOperate             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SExamineHeld         → pure (Just (either SomeGS id (processNormal gs ev)))
                    ev@Input.SOperateHeld         → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SOperateHeldOn       → pure (Just (either SomeGS SomeGS (processNormal gs ev)))
                    ev@Input.STalk                → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SGet                 → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SWear                → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SStoreIn             → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SPullFrom            → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SWait                → pure (Just (SomeGS (processNormal gs ev)))
                    ev@(Input.SSetStance _)       → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SInventorySheet      → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SCharacterSheet      → pure (Just (SomeGS (processNormal gs ev)))
                    ev@Input.SSwitchToTactical    → pure (Just (either SomeGS SomeGS (processNormal gs ev)))
                    ev@Input.SSwitchToHud         → pure (Just (SomeGS (processNormal gs ev)))
                    Input.SBackToMainMenu         → pure Nothing

            gs@(StExamination w _) → do
                renderExamination
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))

            gs@(StConversation w _ (Free cn')) → do
                renderConversation cn'
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    ev@Input.SSelectChoice        → Just . SomeGS <$> processUI gs ev
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            (StConversation w _ (Pure _)) → do
                renderNormal w
                flush
                pure (Just $ SomeGS (StNormal w))

            gs@(StComputerOperation w v cd) → do
                renderComputerOperation cd
                flush
                lift (Input.nextEvent gs ()) >>= \case
                    Input.PassThroughBack    → pure (Just (SomeGS (StNormal (execWorld saveWorldData w))))
                    (Input.PassThrough '\n') → pure (Just (SomeGS (StComputerOperation w v (snd $ runComputer commitInput cd))))
                    (Input.PassThrough '\b') → pure (Just (SomeGS (StComputerOperation w v (snd $ runComputer backspace cd))))
                    (Input.PassThrough c)    → pure (Just (SomeGS (StComputerOperation w v (snd $ runComputer (typeIn c) cd))))
                    --ev                       → Just . SomeGS <$> processComputerOperation gs ev
                where
                    saveWorldData = doMap (modifyObject v (o_state .~ Computer cd))

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
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
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
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
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
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            gs@(StInventoryUI w) → do
                renderInventoryUI
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveCursor SNorth) → Just . SomeGS <$> processUI gs ev
                    ev@(Input.SMoveCursor SSouth) → Just . SomeGS <$> processUI gs ev
                    Input.SUiBack                 → pure (Just (SomeGS (StNormal w)))
                    _                             → pure (Just (SomeGS gs))
            gs@(StSkillsUI w ch) → do
                renderSkillsUI ch
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.STab SNext)     → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious) → Just . SomeGS <$> processUI gs ev
                    Input.SUiBack             → pure (Just (SomeGS (StNormal w)))
                    _                         → pure (Just (SomeGS gs))
            gs@(StEquipmentUI w ch) → do
                renderEquipmentUI ch
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.STab SNext)     → Just . SomeGS <$> processUI gs ev
                    ev@(Input.STab SPrevious) → Just . SomeGS <$> processUI gs ev
                    Input.SUiBack             → pure (Just (SomeGS (StNormal w)))
                    _                         → pure (Just (SomeGS gs))

            gs@(StTargetSelectionAdjactened w d z _) → do
                renderTargetSelectionAdjactened w d z
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveReticule _) → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SMoveTarget _)   → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SSmartTarget _)  → pure (Just (SomeGS (processTarget gs ev)))
                    ev@Input.SConfirmTarget    → pure (Just (processTarget gs ev))
                    Input.STargetBack          → pure (Just (SomeGS (StNormal w)))

            gs@(StTargetSelectionDistant w tp _) → do
                renderTargetSelectionDistant w tp
                flush
                lift (Input.nextEvent gs ()) >>= \e → e `withSomeSing` \case
                    ev@(Input.SMoveReticule _) → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SMoveTarget _)   → pure (Just (SomeGS (processTarget gs ev)))
                    ev@(Input.SSmartTarget _)  → pure (Just (SomeGS (processTarget gs ev)))
                    ev@Input.SConfirmTarget    → pure (Just (processTarget gs ev))
                    Input.STargetBack          → pure (Just (SomeGS (StNormal w)))

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
    sm  ← either error id <$> liftIO (loadTileMap (view dd_dev_startingMap dd))
    pure $ StNormal $
        newWorld (either error id $ fromTileMap baseFromTile (objectFromTile dd) sm)
                 (playerPerson carla)
    where
        playerPerson ∷ DreamnetCharacter → Object States
        playerPerson = Object (Symbol '@') "metal" False True . Person



baseFromTile ∷ Tile → Either String (Base Symbol)
baseFromTile t@(ttype → "Base")   = Right $ Base (Symbol (view t_char t)) False
baseFromTile t@(ttype → "Spawn")  = Right $ Base (Symbol (view t_char t)) True
baseFromTile t@(ttype → "Stairs") = Right $ Base (Symbol (view t_char t)) True
baseFromTile _                    = Left  $ "Non-base tile in base layer"


-- 1) This *could* all be just a single thing. Object type really does not matter here.
-- 2) Actually, it does, because Object carries a specific state, later used by object programs
objectFromTile ∷ (MonadError String me) ⇒ DesignData → Tile → me (Cell (Object States))
objectFromTile _ (ttype → "Base") = pure empty
objectFromTile _ t@(ttype → "Prop") = pure <$> (Object sy <$> m <*> t `readPassable` 2 <*> t `readSeeThrough` 3 <*> st)
    where
        sy = Symbol $ view t_char t
        m  = maybeToError "Material property missing on 'Prop' tile, ix: 4" (4 `readStringProperty` t)
        --h  = maybeToError "Height property missing on 'Prop' tile, ix: 5" (5 `readIntProperty` t)
        st = Prop
                <$> maybeToError "Name property missing on 'Prop' tile, ix: 1" (1 `readStringProperty` t)
                <*> maybeToError "Description property missing on 'Prop' tile, ix: 6" (6 `readStringProperty` t)
        --readPassable ∷ Tile → Int → Either String Bool
        readPassable t i = maybeToError ("Passable property missing on tile '" <> ttype t <> "', ix: " <> show i) (i `readBoolProperty` t)
        --readSeeThrough ∷ Tile → Int → Either String Bool
        readSeeThrough t i = maybeToError ("SeeThrough property missing on tile '" <> ttype t <> "', ix: " <> show i) (i `readBoolProperty` t)

objectFromTile dd t@(ttype → "Person") = pure <$> (Object sy m p s <$> st)
    where
        sy = Symbol '@'
        m  = "blue"
        p  = False
        s  = True
        --h  = 3
        st = pure . Person
                =<< (\n → maybeToError ("No character with name '" <> n <> "' defined.") (n `M.lookup` view dd_characters dd))
                =<< maybeToError "Name property missing for 'Person' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Camera") = pure <$> (Object sy m p s <$> st)
    where
        sy = Symbol $ view t_char t
        m  = "green light"
        p  = True
        s  = True
        --h  = 1
        st = (`Camera` 0) . Faction
                <$> maybeToError "Faction property missing for 'Camera' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Computer") = pure <$> (pure $ Object sy m p s st)
    where
        sy = Symbol $ view t_char t
        m  = "metal"
        p  = False
        s  = True
        --h  = 1
        st = Computer (ComputerData "" []) -- TODO define some data!
objectFromTile _ t@(ttype → "Clothes") = pure <$> (Object sy m p s <$> st)
    where
        sy  = Symbol $ view t_char t
        m   = "cloth"
        p   = True
        s   = True
        --h   = 0
        st  = pure . Clothes
                =<< (\cid → maybeToError ("WearableItem " <> cid <> " isn't defined!") (cid `M.lookup` clothesDict))
                =<< maybeToError "Clothes name property missing for 'Clothes' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Weapon") = pure <$> (Object sy m p s <$> st)
    where
        sy  = Symbol $ view t_char t
        m   = "metal"
        p   = True
        s   = True
        --h   = 0
        st  = pure . Weapon
                =<< (\wid → maybeToError ("WeaponItem " <> wid <> " isn't defined!") (M.lookup wid weaponsDict))
                =<< maybeToError "Weapon name property missing for 'Weapon' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Ammo") = pure <$> (Object sy m p s <$> st)
    where
        sy  = Symbol $ view t_char t
        m   = "metal"
        p   = True
        s   = True
        --h   = 0
        st  = pure . Ammo
                =<< (\aid → maybeToError ("AmmoItem " <> aid <> " isn't defined!") (M.lookup aid ammoDict))
                =<< maybeToError "Ammo name property missing for 'Ammo' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Throwable") = pure <$> (Object sy m p s <$> st)
    where
        sy  = Symbol $ view t_char t
        m   = "metal"
        p   = True
        s   = True
        --h   = 0
        st  = pure . Throwable
                =<< (\tid → maybeToError ("ThrowableItem " <> tid <> " isn't defined!") (tid `M.lookup` throwableDict))
                =<< maybeToError "Throwable name property missing for 'Throwable' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t@(ttype → "Consumable") = pure <$> (Object sy m p s <$> st)
    where
        sy  = Symbol $ view t_char t
        m   = "red"
        p   = True
        s   = True
        --h   = 0
        st  = pure . Consumable
                =<< (\cid → maybeToError ("ConsumableItem " <> cid <> " isn't defined!") $ cid `M.lookup` consumableDict)
                =<< maybeToError "Consumable name property missing for 'Consumable' tile, ix: 1" (1 `readStringProperty` t)
objectFromTile _ t =
    throwError $ "Can't convert Tile type into Object: " <> show t
-- TODO Errrrrr, this should be done through the tileset???

--------------------------------------------------------------------------------

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

renderTargetSelectionAdjactened ∷ (RenderAPI r, Monad r) ⇒ World → Maybe Direction → Int → r ()
renderTargetSelectionAdjactened w md z =
    let pp = view w_player w
    in  case md of
            Nothing → renderCellColumnToStatus w pp
            Just d  → let d3 = V3 (dirToVec d ^. _x) (dirToVec d ^. _y) z
                          tp = clipToBounds (w ^. w_map) $ d3 + unpack pp
                      in  do
                          white ← style s_colorWhite
                          green ← style s_colorGreen
                          renderCellColumnToStatus w tp
                          updateMain $ do
                              RenderAction $ do
                                  C.setColor white
                                  (drawList <$> subtract 1 . view _x <*> subtract 1 . view _y) (unpack pp)
                                      [ "yku"
                                      , "h.l"
                                      , "bjn"
                                      ]
                              draw' (tp ^. unpacked._xy) (charForVec (dirToVec d)) [C.AttributeColor green]
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


renderTargetSelectionDistant ∷ (RenderAPI r, Monad r) ⇒ World → Safe (V3 Int) → r ()
renderTargetSelectionDistant w tp = do
    renderWorld w
    green ← style s_colorGreen
    renderCellColumnToStatus w tp
    updateMain $ draw' (unpack tp ^. _xy) 'X' [C.AttributeColor green]


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

--------------------------------------------------------------------------------

-- TODO render blinking when player is underneath something
renderWorld ∷ (RenderAPI r, Monad r) ⇒ World → r ()
renderWorld w =
    let wm = view w_map w
    in  updateMain =<<
            drawMap (charF wm) (matF wm) (const Vis.Visible) (width w) (height w)
    where
        topNonEmptyCoord wm = listToMaybe . fmap fst . filter (not . snd) . fmap ((,) <$> cellAt wm <*> isEmptyAt wm)
        --topNonEmptyCoord wm = listToMaybe . filter (not . (==Empty)) . fmap (cellAt wm)

        fromBase wm v = v ^. baseAtL wm.wb_symbol.s_char

        charF wm i =
            let coord = indexToCoord wm (clipToBounds' wm i)
            in  case topNonEmptyCoord wm (reverse (column wm coord)) of
                    Nothing → fromBase wm coord
                    Just wc → fromJust $ preview (followLinksL wm._Just.o_symbol.s_char) wc

        matF wm i =
            let coord = indexToCoord wm (clipToBounds' wm i)
            in  case topNonEmptyCoord wm (reverse (column wm coord)) of
                    Nothing → "default"
                    Just wc → fromJust $ preview (followLinksL wm._Just.o_material) wc
                    --Just wc → fromJust $ preview (o_material) wc


renderCellColumnToStatus ∷ (RenderAPI r, Monad r) ⇒ World → Safe (V3 Int) → r ()
renderCellColumnToStatus w v = do
    white ← style s_colorWhite
    green ← style s_colorGreen

    let objectStatePrism = let wm = w ^. w_map
                           in  cellAtL wm.followLinksL wm._Just.o_state
        columnContents   ∷ [String]
        columnContents   = show . preview objectStatePrism <$> column (w ^. w_map) v
    updateHud (f columnContents white green)
    where
        f xs hilight nohilight = do
            clearStatus
            statusOrigin >>= \(start,padding,_) → RenderAction $
                traverse_ (drawCellContent hilight nohilight start padding) (zip [padding..] xs)

        drawCellContent ∷ C.ColorID → C.ColorID → Int → Int → (Int, String) → C.Update ()
        drawCellContent hilight nohilight start padding (y, s) = do
            C.moveCursor (fromIntegral y) (fromIntegral  start)
            if y == (unpack v ^. _z) + padding
                then C.setColor nohilight
                else C.setColor hilight
            C.drawString s

