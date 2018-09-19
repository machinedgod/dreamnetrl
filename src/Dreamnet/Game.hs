{-# LANGUAGE UnicodeSyntax, LambdaCase, TupleSections, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Game
( module Dreamnet.Engine.World

, ItemTraits(..)

, Material(..)

, WearableItem(..), wi_name, wi_equippedAt, wi_volume, wi_weight, wi_material,
  wi_coverage, wi_containerVolume, wi_storedItems

, AmmoType(..)

, WeaponItem(..), wpi_name, wpi_description, wpi_settings, wpi_ammoType

, AmmoItem(..), ami_name, ami_type, ami_currentLoad, ami_maxLoad

, ThrownWeaponItem(..), twi_name

, ConsumableItem(..), ci_name

, Faction(..)
, States(..), _Prop, _Camera, _Person, _Computer, _Clothes, _Weapon, _Ammo,
  _Throwable, _Consumable

, DreamnetCharacter
, DreamnetWorld
, TargetSelectionType(..)
, DistantTargetSelection(..)
, lineOfSight

, TargetActivationF(runWithTarget)
, ChoiceActivationF(runWithChoice)
, GameStateEnum(..), GameState(..), SomeGameState(..), dreamnetWorld, modifyWorld

, DesignData(..), dd_characters, dd_dev_startingMap

, runProgramAsPlayer, withTargetAdjactened, withTargetDistant, withChoice,
  updateVisible
) where

import Safe                       (fromJustNote)
import Control.Lens               (makeLenses, makePrisms, view, preview, (.~), _Just)
import Control.Monad.Free         (Free(Free, Pure))
import Control.Monad.Reader       (MonadReader, runReader, ask, asks) 
import Data.Bool                  (bool)
import Data.Maybe                 (isJust, fromMaybe)
import Data.List                  (elemIndex)
import Data.List.NonEmpty         (NonEmpty((:|)))
import Linear                     (V2(V2))

import qualified Data.Map    as M (Map)
import qualified Data.Set    as S (fromList)

import Dreamnet.Engine.World
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.Character
import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Utils
import Dreamnet.Engine.Object

import Dreamnet.ComputerModel

--------------------------------------------------------------------------------

class ItemTraits i where
    isContainer ∷ i → Bool
    -- TODO container of what type?

data Material = Kevlar
              | Polyester
              | Cotton
              deriving (Eq, Show)


-- TODO I'm just like making shit up here, to define what wearable item is
data WearableItem i = WearableItem {
      _wi_name       ∷ String
    , _wi_equippedAt ∷ SlotType
    -- Volume of the item
    , _wi_volume     ∷ Word
    -- Weight of the item
    , _wi_weight     ∷ Float
    -- Material (TODO make a list?)
    , _wi_material   ∷ Material
    -- When worn as clothes, what is the coverage percent of the body part?
    , _wi_coverage   ∷ Float

    -- If Nothing, its not a container. If Just x, then what is the container's volume?
    -- Note, does not necessarily have to be less than _wi_volume, eg. belts and clip carriers
    , _wi_containerVolume ∷ Maybe Word
    , _wi_storedItems ∷ [i] -- TODO probably just use a slot, or slots! :-O?
    }
    deriving (Eq, Functor)
makeLenses ''WearableItem


instance Show (WearableItem i) where
    show = _wi_name

instance ItemTraits (WearableItem i) where
    isContainer = isJust . _wi_containerVolume


data AmmoType = LaserjetBattery
              deriving (Eq)


-- TODO ammo type as phantom type?
data WeaponItem = WeaponItem {
      _wpi_name        ∷ String
    , _wpi_description ∷ String
    , _wpi_settings    ∷ M.Map String String
    , _wpi_ammoType    ∷ AmmoType
    }
    deriving (Eq)
makeLenses ''WeaponItem


data AmmoItem = AmmoItem {
      _ami_name        ∷ String
    , _ami_type        ∷ AmmoType
    , _ami_currentLoad ∷ Word
    , _ami_maxLoad     ∷ Word
    }
    deriving (Eq)
makeLenses ''AmmoItem


newtype ThrownWeaponItem = ThrownWeaponItem {
      _twi_name ∷ String
    }
    deriving (Eq)
makeLenses ''ThrownWeaponItem


newtype ConsumableItem = ConsumableItem {
      _ci_name ∷ String
    }
    deriving(Eq)
makeLenses ''ConsumableItem

--------------------------------------------------------------------------------

newtype Faction = Faction String
                deriving (Eq, Show)


type DreamnetCharacter = Character States (Free (ConversationF States) ()) Faction
type DreamnetWorld     = World States Visibility


-- TODO Not happy with this development!
-- NOTE this *could* be a record of lists instead, eg.
-- cameras :: [(Faction, Word)], props :: [String], etc
-- Also, if I use a table of stats, then I could encode few different types
-- of objects together?
-- It could be that states support few general programs, that are then
-- configurable via properties and switches. Then, when making a map, object Type
-- would determine the program to run, and tile properties would be switches and
-- additional parameters
--
-- So for example, Camera could utilize some generic 'perception' program that
-- somehow signals some other object, that's set up with switches?
data States = Prop        String String
            | Camera      Faction Word
            | Person      DreamnetCharacter
            | Computer    ComputerData
            | Clothes     (WearableItem States)
            | Weapon      WeaponItem
            | Ammo        AmmoItem
            | Throwable   ThrownWeaponItem
            | Consumable  ConsumableItem
            deriving(Eq)
makePrisms ''States


instance Show States where
    show (Prop s _)      = s
    show (Camera _ _)    = "camera"
    show (Person ch)     = view ch_name ch
    show (Computer _)    = "computer"
    show (Clothes wi)    = view wi_name wi
    show (Weapon wpi)    = view wpi_name wpi
    show (Ammo ami)      = view ami_name ami
    show (Throwable twi) = view twi_name twi
    show (Consumable ci) = view ci_name ci


instance ItemTraits States where
    isContainer (Clothes wi) = isContainer wi
    isContainer _            = False

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter
      -- TODO move item dictionaries here
    , _dd_dev_startingMap ∷ String
    }
makeLenses ''DesignData

--------------------------------------------------------------------------------

data TargetSelectionType = Adjactened
                         | Distant DistantTargetSelection


data DistantTargetSelection = Range       Word
                            | Filtered    (States → Bool)
                            | Composed    TargetSelectionType TargetSelectionType


lineOfSight ∷ (States → Bool) → TargetSelectionType
lineOfSight isVisibleF = Distant (Filtered isVisibleF)

--------------------------------------------------------------------------------

newtype TargetActivationF = TargetActivationF {
      runWithTarget ∷ V2 Int → Int → SomeGameState
    }

newtype ChoiceActivationF = ChoiceActivationF {
      runWithChoice ∷ Int → SomeGameState
    }

--------------------------------------------------------------------------------

data GameStateEnum = 
      Quit
    | Normal
    | Examination
    | Conversation
    | ComputerOperation
    
    | HudTeam
    | HudMessages
    | HudWatch
    | InventoryUI
    | SkillsUI
    | EquipmentUI
    
    | TargetSelectionAdjactened
    | TargetSelectionDistant
    | ChoiceSelection

-- TODO try making World Objects keep ObjectPrograms in them, rather than states
--      then, somehow, programs can keep the state by themselves. Its a monad
--      after all.

type Turn              = Int
type Button            = Int
type SelectedCharacter = Int

data GameState ∷ GameStateEnum → * where
    StNormal            ∷ DreamnetWorld                                                               → GameState 'Normal
    StExamination       ∷ DreamnetWorld → String                                                      → GameState 'Examination
    StConversation      ∷ DreamnetWorld → NonEmpty DreamnetCharacter → Free (ConversationF States) () → GameState 'Conversation
    StComputerOperation ∷ DreamnetWorld → (V2 Int, Int) → ComputerData                                → GameState 'ComputerOperation

    StHudTeam     ∷ DreamnetWorld → SelectedCharacter → GameState 'HudTeam
    StHudMessages ∷ DreamnetWorld                     → GameState 'HudMessages
    StHudWatch    ∷ DreamnetWorld → Turn → Button     → GameState 'HudWatch
    StInventoryUI ∷ DreamnetWorld                     → GameState 'InventoryUI
    StSkillsUI    ∷ DreamnetWorld → DreamnetCharacter → GameState 'SkillsUI
    StEquipmentUI ∷ DreamnetWorld → DreamnetCharacter → GameState 'EquipmentUI

    StTargetSelectionAdjactened ∷ DreamnetWorld → V2 Int           → Int → TargetActivationF → GameState 'TargetSelectionAdjactened
    StTargetSelectionDistant    ∷ DreamnetWorld → V2 Int           → Int → TargetActivationF → GameState 'TargetSelectionDistant
    StChoiceSelection           ∷ DreamnetWorld → [(Char, String)] → Int → ChoiceActivationF → GameState 'ChoiceSelection


dreamnetWorld ∷ GameState gse → DreamnetWorld
dreamnetWorld (StNormal w)                          = w
dreamnetWorld (StExamination w _)                   = w
dreamnetWorld (StConversation w _ _)                = w
dreamnetWorld (StComputerOperation w _ _)           = w
dreamnetWorld (StHudTeam w _)                       = w
dreamnetWorld (StHudMessages w)                     = w
dreamnetWorld (StHudWatch w _ _)                    = w
dreamnetWorld (StInventoryUI w)                     = w
dreamnetWorld (StSkillsUI w _)                      = w
dreamnetWorld (StEquipmentUI w _)                   = w
dreamnetWorld (StTargetSelectionAdjactened w _ _ _) = w
dreamnetWorld (StTargetSelectionDistant w _ _ _)    = w
dreamnetWorld (StChoiceSelection w _ _ _)           = w


modifyWorld ∷ (DreamnetWorld → DreamnetWorld) → GameState gse → GameState gse
modifyWorld wf (StNormal w)                          = StNormal (wf w)
modifyWorld wf (StExamination w s)                   = StExamination (wf w) s
modifyWorld wf (StConversation w cs c)               = StConversation (wf w) cs c
modifyWorld wf (StComputerOperation w v cd)          = StComputerOperation (wf w) v cd
modifyWorld wf (StHudTeam w i)                       = StHudTeam (wf w) i
modifyWorld wf (StHudMessages w)                     = StHudMessages (wf w)
modifyWorld wf (StHudWatch w hs mm)                  = StHudWatch (wf w) hs mm
modifyWorld wf (StInventoryUI w)                     = StInventoryUI (wf w)
modifyWorld wf (StSkillsUI w ch)                     = StSkillsUI (wf w) ch
modifyWorld wf (StEquipmentUI w ch)                  = StEquipmentUI (wf w) ch
modifyWorld wf (StTargetSelectionAdjactened w t i f) = StTargetSelectionAdjactened (wf w) t i f
modifyWorld wf (StTargetSelectionDistant w t i f)    = StTargetSelectionDistant (wf w) t i f
modifyWorld wf (StChoiceSelection w xs i f)          = StChoiceSelection (wf w) xs i f

--------------------------------------------------------------------------------

data SomeGameState = ∀ gse. SomeGS (GameState gse)

--------------------------------------------------------------------------------

runProgramAsPlayer ∷ DreamnetWorld → (V2 Int, Int) → Free (ObjectF States) () → SomeGameState
runProgramAsPlayer w (v, i) prg = case evalWorld (valueAt i <$> cellAt v) w of
    Nothing → SomeGS (StNormal w)
    Just o  → let (v', i', o', sgs) = runObjectMonadForPlayer (v, i, o, StNormal w) prg `runReader` w
              in  case sgs of
                    (SomeGS gs) → SomeGS (updateVisible (modifyWorld (saveChanges o (v', i', o')) gs))
        where
            saveChanges o (v', i', o') w' = flip execWorld w' $
                replaceObject v o o'
                --moveObject v o v'
                


withTargetAdjactened ∷ DreamnetWorld → (V2 Int → Int → SomeGameState) → GameState 'TargetSelectionAdjactened
withTargetAdjactened w f = StTargetSelectionAdjactened w (pp w) 0 (TargetActivationF f)
    where
        pp = evalWorld (fst <$> playerPosition)


withTargetDistant ∷ DreamnetWorld → (V2 Int → Int → SomeGameState) → GameState 'TargetSelectionDistant
withTargetDistant w f = StTargetSelectionDistant w (pp w) 0 (TargetActivationF f)
    where
        pp = evalWorld (fst <$> playerPosition)


withChoice ∷ DreamnetWorld → [(Char, String)] → (Int → SomeGameState) → GameState 'ChoiceSelection
withChoice w lst f = StChoiceSelection w lst 0 (ChoiceActivationF f)


updateVisible ∷ GameState gse → GameState gse
updateVisible gs =
    let w          = dreamnetWorld gs
        pp         = evalWorld playerPosition w `addStanceHeight` w
        tps        = (`addStanceHeight` w) <$> evalWorld teamPositions w
        raysForAll = (`pointsForOne` w) <$> (pp : tps)
        nw         = execWorld (setVisibility (S.fromList $ mconcat raysForAll)) w
    in  modifyWorld (const nw) gs
    
    -- NOTE resolving 'x' causes lag
    where
        pointsForOne ∷ (V2 Int, Int) → DreamnetWorld → [V2 Int]
        pointsForOne (p, h) w =
            let points = circle 20 p
                rays   ∷ [[(V2 Int, Bool)]]
                rays   = (\t → evalWorld (castRay p h t 3) w) <$> points
            in  mconcat $ fmap fst . visibleAndOneExtra <$> rays

        visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
        visibleAndOneExtra l =
            let front = takeWhile ((==True) . snd) l
                remv  = dropWhile ((==True) . snd) l
            in  bool (head remv : front) front (null remv)

        -- TODO subtract 1 because standing on the floor counts as if we're in the second 'height box'
        --      floor has to count as having no height, and height calculations must use heights of all
        --      objects in the same cell, not the index itself!
        addStanceHeight ∷ (V2 Int, Int) → DreamnetWorld → (V2 Int, Int)
        addStanceHeight (v, i) = (v,) . max 1 . min 4 . subtract 1 . (+i) . stanceToHeight . valueAt i . evalWorld (cellAt v)

        stanceToHeight = go . fromJustNote "updateVisible!" . preview (_Just.o_state._Person.ch_stance) 
            where
                go Upright = 3
                go Crouch  = 2
                go Prone   = 1


runObjectMonadForPlayer ∷ (MonadReader DreamnetWorld m) ⇒ (V2 Int, Int, Object States, GameState gs) → Free (ObjectF States) () → m (V2 Int, Int, Object States, SomeGameState)
runObjectMonadForPlayer (cv, h, o, gs) (Free (Position fn)) =
    runObjectMonadForPlayer (cv, h, o, gs) (fn (cv, h))
runObjectMonadForPlayer (_, h, o, gs) (Free (Move v n)) =
    runObjectMonadForPlayer (v, h, o, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (Passable fn)) =
    runObjectMonadForPlayer (cv, h, o, gs) (fn $ view o_passable o)
runObjectMonadForPlayer (cv, h, o, gs) (Free (SetPassable b n)) =
    let no = o_passable .~ b $ o
    in  runObjectMonadForPlayer (cv, h, no, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (SeeThrough fn)) =
    runObjectMonadForPlayer (cv, h, o, gs) (fn $ view o_seeThrough o)
runObjectMonadForPlayer (cv, h, o, gs) (Free (SetSeeThrough b n)) =
    let no = o_seeThrough .~ b $ o
    in  runObjectMonadForPlayer (cv, h, no, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (CanSee v fn)) =
    asks (evalWorld (castRay cv h v 0)) >>= -- TODO add object height!
        runObjectMonadForPlayer (cv, h, o, gs) . fn . and . fmap snd
runObjectMonadForPlayer (cv, h, o, gs) (Free (ChangeSymbol s n)) =
    let no = o_symbol .~ s $ o
    in  runObjectMonadForPlayer (cv, h, no, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (ChangeMat m n)) =
    let no = o_material .~ m $ o
    in  runObjectMonadForPlayer (cv, h, no, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (Message _ n)) =
    -- TODO repair set status!
    runObjectMonadForPlayer (cv, h, o, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (DoTalk c n)) = do
    w ← ask
    fromMaybe 
        (runObjectMonadForPlayer (cv, h, o, gs) n)
        (do
            pc ← evalWorld (preview (o_state._Person) <$> playerObject) w
            ch ← preview (o_state._Person) o
            pure $ runObjectMonadForPlayer (cv, h, o, StConversation w (pc :| [ch]) c) n
        )
runObjectMonadForPlayer (cv, h, o, gs) (Free (OperateComputer n)) = do
    w ← ask
    maybe
        (runObjectMonadForPlayer (cv, h, o, gs) n)
        (\cd → runObjectMonadForPlayer (cv, h, o, StComputerOperation w (cv, 1) cd) n)
        (preview (o_state._Computer) o)
runObjectMonadForPlayer (cv, h, o, gs) (Free (ScanRange r f fn)) = do
    points ← asks (evalWorld (interestingObjects cv r (f . view o_state)))
    values ← asks (evalWorld (foldr onlyJust [] <$> traverse (fmap (fmap (view o_state) . lastValue) . cellAt) points))
    runObjectMonadForPlayer (cv, h, o, gs) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l
runObjectMonadForPlayer (cv, h, o, gs) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → runObjectMonadForPlayer (cv, h, o, gs) (fn (V2 0 0))
        LineOfSight → runObjectMonadForPlayer (cv, h, o, gs) (fn (V2 1 1))
runObjectMonadForPlayer (cv, h, o, gs) (Free (SpawnNewObject _ _ n)) =
    --doWorld $
    --    modifyCell v (addToCell (Object (Symbol '?') "metal" True True 1 s))
    -- TODO fix SpawnNewObject
    runObjectMonadForPlayer (cv, h, o, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (RemoveObject _ _ n)) =
    --doWorld $ do
    --    x ← fromJustNote "RemoveObject runObjectMonadForAI" . valueAt i <$> cellAt v
    --    modifyCell v (deleteFromCell x)
    -- TODO fix RemoveObject
    runObjectMonadForPlayer (cv, h, o, gs) n
runObjectMonadForPlayer (cv, h, o, gs) (Free (FindObject s fn)) = do
    xs ← asks $ evalWorld $ do
        pp ← fst <$> playerPosition
        interestingObjects pp 60 ((s==) . view o_state)
    if null xs
        then runObjectMonadForPlayer (cv, h, o, gs) (fn Nothing)
        else do
            let v = head xs
            cellvs ← asks (evalWorld (cellValues <$> cellAt v))
            let mi = s `elemIndex` (view o_state <$> cellvs)
            let r = (v,) <$> mi
            runObjectMonadForPlayer (cv, h, o, gs) (fn r)
runObjectMonadForPlayer (cv, h, o, gs) (Pure ()) =
    pure (cv, h, o, SomeGS gs)
    

