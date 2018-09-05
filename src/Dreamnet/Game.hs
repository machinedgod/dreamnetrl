{-# LANGUAGE UnicodeSyntax, LambdaCase, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

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
  _Throwable, _Consumable, withCharacter, whenCharacter, whenComputer,
  whenClothes, maybeCharacter


, DreamnetCharacter
, DreamnetWorld
, TargetSelectionType(..)
, DistantTargetSelection(..)
, lineOfSight

, TargetActivationF(runWithTarget)
, ChoiceActivationF(runWithChoice)
, GameAPI(..)
, GameState(..)
, DesignData(..)
, dd_characters
, dd_dev_startingMap

, GameM
, runGame
, evalGame
, execGame
) where

import Control.Lens               (makeLenses, makePrisms, view, (.~), Prism')
import Control.Monad.Trans        (MonadTrans, lift)
import Control.Monad.Free         (Free(Free, Pure))
import Control.Monad.State        (MonadState, StateT, runStateT, evalStateT,
                                   execStateT, get, gets, put, modify)
import Data.Maybe                 (isJust)
import Data.List.NonEmpty         (NonEmpty((:|)))
import Linear                     (V2(V2))

import qualified Data.Map    as M (Map)

import Dreamnet.Engine.World
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.Character
import Dreamnet.Engine.Conversation

import qualified Dreamnet.Rendering.Renderer as R

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


withCharacter ∷ (DreamnetCharacter → DreamnetCharacter) → States → States
withCharacter f (Person ch) = Person (f ch)
withCharacter _ x           = x

--------------------------------------------------------------------------------

whenCharacter ∷ (DreamnetCharacter →  a) →  a → States →  a
whenCharacter f _ (Person ch) = f ch
whenCharacter _ d _           = d


whenComputer ∷ (ComputerData →  a) →  a → States →  a
whenComputer f _ (Computer cd) = f cd
whenComputer _ d _             = d


whenClothes ∷ (∀ b. WearableItem b → a) → a → States → a
whenClothes f _ (Clothes wi) = f wi
whenClothes _ d _            = d


_clothes ∷ Prism' States (WearableItem a)
_clothes = undefined

--------------------------------------------------------------------------------

maybeCharacter ∷ States → Maybe DreamnetCharacter
maybeCharacter (Person ch) = Just ch
maybeCharacter _           = Nothing

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

newtype TargetActivationF g = TargetActivationF {
      runWithTarget ∷ (GameAPI g) ⇒ V2 Int → Int → g (GameState g)
    }

newtype ChoiceActivationF g = ChoiceActivationF {
      runWithChoice ∷ (GameAPI g) ⇒ Int → g (GameState g)
    }


class GameAPI g where
    gameState          ∷ g (GameState g)
    changeGameState    ∷ (GameState g → GameState g) → g (GameState g)
    world              ∷ g DreamnetWorld
    doWorld            ∷ WorldM States Visibility a → g a -- TODO why not WorldApi???
    withTarget         ∷ TargetSelectionType → ((Monad g) ⇒ V2 Int → Int → g (GameState g)) → g (GameState g)
    withChoice         ∷ [(Char, String)] → ((Monad g) ⇒ Int → g (GameState g)) → g (GameState g)
    runProgramAsPlayer ∷ V2 Int → Free (ObjectF States) () → g (GameState g)

--------------------------------------------------------------------------------

-- TODO try making World Objects keep ObjectPrograms in them, rather than states
--      then, somehow, programs can keep the state by themselves. Its a monad
--      after all.
data GameState g = Quit               DreamnetWorld
                 | Normal             DreamnetWorld
                 | Examination        DreamnetWorld String
                 | Conversation       DreamnetWorld (NonEmpty DreamnetCharacter) (Free (ConversationF States) ())
                 | ComputerOperation  DreamnetWorld (V2 Int, Int) ComputerData

                 | HudTeam      DreamnetWorld Int
                 | HudMessages  DreamnetWorld
                 | HudWatch     DreamnetWorld Int Int
                 | InventoryUI  DreamnetWorld
                 | SkillsUI     DreamnetWorld DreamnetCharacter
                 | EquipmentUI  DreamnetWorld DreamnetCharacter

                 | TargetSelectionAdjactened  DreamnetWorld (V2 Int) Int (TargetActivationF g)
                 | TargetSelectionDistant     DreamnetWorld (V2 Int) Int (TargetActivationF g)
                 | ChoiceSelection            DreamnetWorld [(Char, String)] Int (ChoiceActivationF g)

--------------------------------------------------------------------------------

-- TODO Can't be monadstate of two things at once (GameState and RendererEnvironment)
newtype GameM m a = GameM { runGameM ∷ StateT (GameState (GameM m)) m a }
                  deriving (Functor, Applicative, Monad, MonadState (GameState (GameM m)))


instance MonadTrans GameM where
    lift = GameM . lift


instance (R.RenderAPI m, Monad m) ⇒ R.RenderAPI (GameM m) where
    updateMain = lift . R.updateMain

    updateHud = lift . R.updateHud

    updateUi = lift . R.updateUi

    screenSize = lift R.screenSize

    mainSize = lift R.mainSize

    hudSize = lift R.hudSize

    setScroll = lift . R.setScroll

    doScroll = lift . R.doScroll
    
    withScroll = lift . R.withScroll

    setChoice = lift . R.setChoice

    doChoice = lift . R.doChoice

    withChoiceData = lift . R.withChoiceData

    currentChoice = lift R.currentChoice

    moveCamera = lift . R.moveCamera

    camera = lift R.camera

    style s = lift (R.style s)

    flush = lift R.flush



instance (Monad m) ⇒ GameAPI (GameM m) where
    gameState = get

    changeGameState f = modify f >> get

    world = gets $ \case
        (Quit w)                  → w
        (Normal w)                → w
        (Examination w _)         → w
        (Conversation w _ _)      → w
        (ComputerOperation w _ _) → w

        (HudTeam w _)     → w
        (HudMessages w)   → w
        (HudWatch w _ _)  → w
        (InventoryUI w)   → w
        (SkillsUI w _)    → w
        (EquipmentUI w _) → w
        
        (TargetSelectionAdjactened w _ _ _) → w
        (TargetSelectionDistant w _ _ _)    → w
        (ChoiceSelection w _ _ _)           → w

    doWorld m = do
        (x, w') ← runWorld m <$> world
        modify $ \case
            (Quit _)                   → Quit w'
            (Normal _)                 → Normal w'
            (Examination _ s)          → Examination w' s
            (Conversation _ cs c)      → Conversation w' cs c
            (ComputerOperation _ v cd) → ComputerOperation w' v cd

            (HudTeam _ i)      → HudTeam w' i
            (HudMessages _)    → HudMessages w'
            (HudWatch _ hs mm) → HudWatch w' hs mm
            (InventoryUI _)    → InventoryUI w'
            (SkillsUI _ ch)    → SkillsUI w' ch
            (EquipmentUI _ ch) → EquipmentUI w' ch

            (TargetSelectionAdjactened _ t i f) → TargetSelectionAdjactened w' t i f
            (TargetSelectionDistant _ t i f)    → TargetSelectionDistant w' t i f
            (ChoiceSelection _ xs i f)          → ChoiceSelection w' xs i f
        pure x

    withTarget Adjactened f = do
        w  ← world
        pp ← doWorld (fst <$> playerPosition)
        let gs = TargetSelectionAdjactened w pp 0 (TargetActivationF f)
        put gs
        pure gs

    withTarget (Distant _) f = do
        w  ← world
        pp ← doWorld (fst <$> playerPosition)
        let gs = TargetSelectionDistant w pp 0 (TargetActivationF f)
        put gs
        pure gs

    withChoice lst f = do
        w  ← world
        let gs = ChoiceSelection w lst 0 (ChoiceActivationF f)
        put gs
        pure gs

    runProgramAsPlayer v prg =
        doWorld (lastValue <$> cellAt v) >>= \case
            Nothing → Normal <$> world
            Just o  → do
                gs ← runObjectMonadForPlayer (v, o) prg
                doWorld updateVisible
                pure gs


runGame ∷ (Monad m) ⇒ GameM m a → GameState (GameM m) → m (a, GameState (GameM m))
runGame = runStateT . runGameM


evalGame ∷ (Monad m) ⇒ GameM m a → GameState (GameM m) → m a
evalGame = evalStateT . runGameM


execGame ∷ (Monad m) ⇒ GameM m a → GameState (GameM m) → m (GameState (GameM m))
execGame = execStateT . runGameM

--------------------------------------------------------------------------------

runObjectMonadForPlayer ∷ (GameAPI g, Monad g) ⇒ (V2 Int, Object States) → Free (ObjectF States) a → g (GameState g)
runObjectMonadForPlayer t (Free (Position fn)) =
    runObjectMonadForPlayer t (fn (fst t))
runObjectMonadForPlayer (cv, o) (Free (Move v n)) =
    doWorld (moveObject cv o v) *>
        runObjectMonadForPlayer (v, o) n
runObjectMonadForPlayer t (Free (Passable fn)) =
    runObjectMonadForPlayer t (fn $ view o_passable (snd t))
runObjectMonadForPlayer (cv, o) (Free (SetPassable b n)) =
    let no = o_passable .~ b $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer t (Free (SeeThrough fn)) =
    runObjectMonadForPlayer t (fn $ view o_seeThrough (snd t))
runObjectMonadForPlayer (cv, o) (Free (SetSeeThrough b n)) =
    let no = o_seeThrough .~ b $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (CanSee v fn)) =
    doWorld (castVisibilityRay cv v) >>=
        runObjectMonadForPlayer (cv, o) . fn . and . fmap snd
runObjectMonadForPlayer (cv, o) (Free (ChangeSymbol s n)) =
    let no = o_symbol .~ s $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (ChangeMat m n)) =
    let no = o_material .~ m $ o
    in  doWorld (replaceObject cv o no) *>
            runObjectMonadForPlayer (cv, no) n
runObjectMonadForPlayer (cv, o) (Free (Message s n)) = -- TODO this means we can delete set status from the world code! :-D
    doWorld (setStatus s) *>
        runObjectMonadForPlayer (cv, o) n
runObjectMonadForPlayer (_, o) (Free (DoTalk c _)) = do
    w ← world
    (Person pc) ← doWorld playerObject
    changeGameState $ \_ → 
        whenCharacter (\ch → Conversation w (pc :| [ch]) c) (Normal w) (view o_state o)
    -- runObjectMonadForPlayer (cv, o) n
runObjectMonadForPlayer (cv, o) (Free (OperateComputer _)) = do
    w ← world
    changeGameState $ \_ →
        -- TODO actually find the position here -----.----   , and if there isn't one, don't do anything!
        whenComputer (\cd → ComputerOperation w (cv, 1) cd) (Normal w) (view o_state o)
    --runObjectMonadForPlayer (cv, o) n
runObjectMonadForPlayer (cv, o) (Free (ScanRange r f fn)) = do
    points ← doWorld (interestingObjects cv r f)
    values ← doWorld (foldr onlyJust [] <$> traverse (fmap lastValue . cellAt) points)
    runObjectMonadForPlayer (cv, o) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l
runObjectMonadForPlayer (cv, o) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → runObjectMonadForPlayer (cv, o) (fn (V2 0 0))
        LineOfSight → runObjectMonadForPlayer (cv, o) (fn (V2 1 1))
runObjectMonadForPlayer _ (Pure _) =
    Normal <$> world

