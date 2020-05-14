{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.ProcessNormal
where


import Prelude            hiding (head, (!!))
import Safe                      (succSafe, predSafe, at)
import Control.Lens              (view, views, _Just, preview, previews)
import Control.Lens.Operators
import Control.Monad             (join)
import Control.Monad.State       (execState, modify, get)
import Data.Maybe                (fromMaybe, fromJust)
import Data.Singletons           (fromSing)

import Dreamnet.Engine.Character
import Dreamnet.Engine.Object
import qualified Dreamnet.Engine.Input      as Input
import Dreamnet.Engine.Iteration
import Dreamnet.Engine.Utils (maybeToError)

import Dreamnet.Game

import Design.ObjectPrograms
import Design.GameCharacters
import Design.Items

--------------------------------------------------------------------------------

choiceChs ∷ String
choiceChs = "fdsahjkltrewyuiopvcxzbnmFDSAHJKLTREWYUIOPVCXZBNM"

--------------------------------------------------------------------------------

class ProcessNormal (gsi ∷ GameStateEnum) (ev ∷ Input.WorldEvent) where
    type GameStateOut gsi ev ∷ *
    processNormal ∷ GameState gsi → Input.SWorldEvent ev → GameStateOut gsi ev

--------------------------------------------------------------------------------

instance ProcessNormal 'Normal ('Input.Move k) where
    type GameStateOut 'Normal ('Input.Move k) = GameState 'Normal
    processNormal (StNormal w) (Input.SMove d) =
        updateVisible $ StNormal $ flip execWorld w $ do
            --let v2 = dirToVec' d
            --    v3 = V3 (v2 ^. _x) (v2 ^. _y) 0
            movePlayer (fromSing d)
            --movePlayer =<< uses wMap (`clipToBounds` v3)
            increaseTurn


instance ProcessNormal 'Normal ('Input.MoveCamera k) where
    type GameStateOut 'Normal ('Input.MoveCamera k) = GameState 'Normal
    processNormal (StNormal w) (Input.SMoveCamera _) = StNormal w
        --moveCamera v


instance ProcessNormal 'Normal 'Input.Examine where
    type GameStateOut 'Normal 'Input.Examine = GameState 'TargetSelectionDistant
    processNormal (StNormal w) _ = withTargetDistant w $ \v →
        maybe nextTurn (runProgramAsPlayer w v) $
            program <$> v ^? targetedObject
                    <*> playerObject w ^? oState._Person
        where
            nextTurn       = SomeGS $ StNormal (execWorld increaseTurn w)
            program o ch   = programForState ch (view oState o) Examine
            targetedObject = let wm = w ^. wMap
                             in  cellAtL wm.followLinksL wm._Just


instance ProcessNormal 'Normal 'Input.DescribeEnvironment where
    type GameStateOut 'Normal 'Input.DescribeEnvironment = GameState 'Examination
    processNormal (StNormal w) _ = StExamination w (w ^. wMap.wmDesc)
        --where
        --    describeWorld  = do
        --        let d = evalWorld desc w
        --        setScroll (newScrollData (V2 2 1) (V2 60 20) Nothing d)
        --        pure (StExamination w d)


instance ProcessNormal 'Normal 'Input.Operate where
    type GameStateOut 'Normal 'Input.Operate = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        fromMaybe (SomeGS (StNormal w)) $ do
            o  ← preview (let wm = w ^. wMap
                          in  cellAtL wm.followLinksL wm._Just) t
            ch ← preview (oState._Person) (playerObject w)
            pure (runProgramAsPlayer w t (program o ch))
        where
            program o ch = programForState ch (view oState o) Operate


instance ProcessNormal 'Normal 'Input.ExamineHeld where
    type GameStateOut 'Normal 'Input.ExamineHeld = Either (GameState 'Normal) SomeGameState
    processNormal (StNormal w) _ = maybeToError (StNormal w) $ do
        ho  ← join $ previews (oState._Person) (slotWrapperItem . primaryHandSlot) (playerObject w)
        pch ← preview (oState._Person) (playerObject w)
        pure $ runProgramAsPlayer w (view wPlayer w) (programForState pch ho Examine)


instance ProcessNormal 'Normal 'Input.OperateHeld where
    type GameStateOut 'Normal 'Input.OperateHeld = GameState 'Normal
    processNormal (StNormal w) _ = fromMaybe (StNormal (execWorld increaseTurn w)) $ do
        ho    ← join $ previews (oState._Person) (slotWrapperItem . primaryHandSlot) (playerObject w)
        pch   ← preview (oState._Person) (playerObject w)
        -- Have to pattern match on x to uncover SomeGS
        pure $
            case runProgramAsPlayer w (view wPlayer w) (programForState pch ho Operate) of
                -- TODO BLATANTLY WRONG BUT FIXING COMPILATION NOW
                (SomeGS gs@(StNormal _)) → gs
                _                        → StNormal w


-- TODO obtain target should happen inside Object Program, and then interpreter
--      can either show UI for the player, or use "brain"/Simulation to select
--      one for the NPC's
-- Also, the range should be item's range
instance ProcessNormal 'Normal 'Input.OperateHeldOn where
    type GameStateOut 'Normal 'Input.OperateHeldOn = Either (GameState 'Normal) (GameState 'TargetSelectionDistant)
    processNormal (StNormal w) _ = maybeToError (StNormal w) $ do
        ho ← join $ previews (oState._Person) (slotWrapperItem . primaryHandSlot) (playerObject w)
        pure $ withTargetDistant w $ \t → fromMaybe (SomeGS (StNormal w)) $ do
            so ← preview (let wm = w ^. wMap
                          in  cellAtL wm.followLinksL wm._Just.oState) t
            ch ← preview (oState._Person) (playerObject w)
            -- TODO which of the game states should take precedence?
            pure $
                case runProgramAsPlayer w (view wPlayer w) (programForState ch ho (OperateOn so)) of
                    (SomeGS gs) → runProgramAsPlayer (dreamnetWorld gs) t (programForState ch so (OperateWith ho))


instance ProcessNormal 'Normal 'Input.Talk where
    type GameStateOut 'Normal 'Input.Talk = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        fromMaybe (SomeGS (StNormal w)) $ do
            o  ← preview (let wm = w ^. wMap
                          in  cellAtL wm.followLinksL wm._Just) t
            ch ← preview (oState._Person) (playerObject w)
            pure $ runProgramAsPlayer w t (program o ch)
                --Just ch → case runProgramAsPlayer w (v, i) (program o ch) of
                --    gs@(SomeGS (StConversation _ ps (Free cn))) → conversationUpdateUi (view chName <$> ps) cn *> pure gs
                --    gs                                          → pure gs
        where
            program o ch = programForState ch (view oState o) Talk


instance ProcessNormal 'Normal 'Input.Get where
    type GameStateOut 'Normal 'Input.Get = GameState 'TargetSelectionAdjactened
    processNormal (StNormal w) _ = withTargetAdjactened w $ \t →
        SomeGS $ StNormal $ fromMaybe w $ do
            o ← preview (let wm = w ^. wMap
                         in  cellAtL wm.followLinksL wm._Just.oState) t
            pure $ flip execWorld w $ do
                changePlayer (pickUp o)
                doMap (deleteObject t)
                increaseTurn



instance ProcessNormal 'Normal 'Input.Wear where
    type GameStateOut 'Normal 'Input.Wear = GameState 'ChoiceSelection
    -- TODO equipping stuff might take more than one turn! We need support for multi-turn actions (with a tiny progress bar :-))
    processNormal (StNormal w) _ = withChoice w xs $ \i →
        let (side, slot) = vs `at` i
        in  SomeGS $ StNormal $ flip execWorld w $ do
                changePlayer (tryWear side slot <*> slotWrapperItem . primaryHandSlot)
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
        let containerList = fromMaybe [] $ previews (oState._Person) equippedContainers (playerObject w)
            xs            = zip choiceChs $ fromJust . preview (_Just._Clothes.wiName) . slotWrapperItem <$> containerList
        in  withChoice w xs $ \i →
                SomeGS $ StNormal $ flip execWorld w $ do
                    changePlayer (tryStore (containerList `at` i) <*> slotWrapperItem . primaryHandSlot)
                    increaseTurn
        where
            appendToContainer ∷ States → Maybe States → Maybe States
            appendToContainer i (Just (Clothes wi)) = Just $ Clothes (wiStoredItems %~ (++[i]) $ wi)
            appendToContainer _ x                   = x

            tryStore sw ch (Just i) = flip execState ch $ do
                modify (modifySlotContent (Just RightSide) Hand (const Nothing))
                modify (modifySlotContent (slotWrapperOrientation sw) (slotWrapperType sw) (appendToContainer i))
            tryStore _ ch _ = ch


instance ProcessNormal 'Normal 'Input.PullFrom where
    type GameStateOut 'Normal 'Input.PullFrom = GameState 'ChoiceSelection
    processNormal (StNormal w) _ =
        -- TODO make this single-step choice (show containers and items as tree)
        let containerList = fromMaybe [] $ previews (oState._Person) equippedContainers (playerObject w)
            xs            = zip choiceChs $ fromJust . preview (_Just._Clothes.wiName) . slotWrapperItem <$> containerList
        in  withChoice w xs $ \i →
            let sw       = containerList `at` i
                itemList = view (_Just._Clothes.wiStoredItems) (slotWrapperItem sw)
                xs2      = zip choiceChs (show <$> itemList)
            in  SomeGS $ withChoice w xs2 $ \i2 →
                    let item = itemList `at` i2
                    in  SomeGS $ StNormal $ flip execWorld w $ do
                            changePlayer (execState (pullFrom sw item))
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
                             (_Just._Clothes.wiStoredItems %~ filter (item /=))


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
            updateStance f = changePlayer (chStance %~ f)


instance ProcessNormal 'Normal 'Input.InventorySheet where
    type GameStateOut 'Normal 'Input.InventorySheet = GameState 'InventoryUI
    processNormal (StNormal w) _ = StInventoryUI w
        --let itemList = maybe [] listOfItemsFromContainers $
        --                    preview (oState._Person) (playerObject w)
        ----setScroll (newScrollData' (V2 1 1) (V2 60 30) (Just "Inventory sheet") itemList)
        --in  StInventoryUI w


instance ProcessNormal 'Normal 'Input.CharacterSheet where
    type GameStateOut 'Normal 'Input.CharacterSheet = GameState 'SkillsUI
    processNormal (StNormal w) _ = StSkillsUI w carla  -- TODO totally not correct


instance ProcessNormal 'Normal 'Input.SwitchToTactical where
    type GameStateOut 'Normal 'Input.SwitchToTactical = Either (GameState 'Normal) (GameState 'ChoiceSelection)
    processNormal (StNormal w) _ =
        let teamChars = fromJust . preview (oState._Person) <$> teamObjects w
        in  if not (null teamChars)
                then
                    let xs = zip choiceChs (view chName <$> teamChars)
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


equippedContainers ∷ (ItemTraits i) ⇒ Character i c f → [SlotWrapper i]
equippedContainers = filter containers . equippedSlots
    where
        containers (SlotWrapper s) = views sItem (maybe False isContainer) s


listOfItemsFromContainers ∷ Character States c f → [String]
listOfItemsFromContainers ch = concat $ makeItemList <$> equippedContainers ch
    where
        -- TODO again, annoying. We don't know its "Clothes" inside Slot.
        --      why is my typing logic so bad here???
        makeItemList ∷ SlotWrapper States → [String]
        makeItemList (SlotWrapper (Slot (Just (Clothes wi)))) = _wiName wi : (("- "<>) . show <$> _wiStoredItems wi)
        makeItemList _                                        = []



-- TODO reuse code for aiming weapons
--switchAim ∷ Maybe (Object → Bool) → StateT Game C.Curses ()
--switchAim (Just nof) = do
--    pp ← use  (gWorld.wPlayer.ePosition)
--    os ← uses (gWorld.wMap) (interestingObjects pp 2 nof)
--    ca ← use  gAim
--    case ca of
--        Just a → gAim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
--        _      → gAim .= headMay os
--switchAim Nothing = gAim .= Nothing


-- TODO reuse code for aiming weapons
--allButTheBase ∷ Object → Bool
--allButTheBase o
--    | view oSymbol o == '.' = False
--    | otherwise              = True
