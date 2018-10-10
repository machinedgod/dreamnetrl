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
( module Dreamnet.World

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
import Control.Lens               (makeLenses, view, preview, (.~), _Just)
import Control.Monad.Free         (Free(Free, Pure))
import Control.Monad.Reader       (MonadReader, runReader, ask, asks) 
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)
import Data.List                  (elemIndex)
import Data.List.NonEmpty         (NonEmpty((:|)))
import Linear                     (V2(V2))

import qualified Data.Map    as M (Map)
import qualified Data.Set    as S (fromList)

import Dreamnet.Engine.Utils
import Dreamnet.Engine.Object

import Dreamnet.World
import Dreamnet.ObjectStates

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
      runWithTarget ∷ WorldPosition → SomeGameState
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
    StNormal            ∷ World                                                               → GameState 'Normal
    StExamination       ∷ World → String                                                      → GameState 'Examination
    StConversation      ∷ World → NonEmpty DreamnetCharacter → Free (ConversationF States) () → GameState 'Conversation
    StComputerOperation ∷ World → WorldPosition → ComputerData                                → GameState 'ComputerOperation

    StHudTeam     ∷ World → SelectedCharacter → GameState 'HudTeam
    StHudMessages ∷ World                     → GameState 'HudMessages
    StHudWatch    ∷ World → Turn → Button     → GameState 'HudWatch
    StInventoryUI ∷ World                     → GameState 'InventoryUI
    StSkillsUI    ∷ World → DreamnetCharacter → GameState 'SkillsUI
    StEquipmentUI ∷ World → DreamnetCharacter → GameState 'EquipmentUI

    StTargetSelectionAdjactened ∷ World → WorldPosition          → TargetActivationF → GameState 'TargetSelectionAdjactened
    StTargetSelectionDistant    ∷ World → WorldPosition          → TargetActivationF → GameState 'TargetSelectionDistant
    StChoiceSelection           ∷ World → [(Char, String)] → Int → ChoiceActivationF → GameState 'ChoiceSelection


dreamnetWorld ∷ GameState gse → World
dreamnetWorld (StNormal w)                        = w
dreamnetWorld (StExamination w _)                 = w
dreamnetWorld (StConversation w _ _)              = w
dreamnetWorld (StComputerOperation w _ _)         = w
dreamnetWorld (StHudTeam w _)                     = w
dreamnetWorld (StHudMessages w)                   = w
dreamnetWorld (StHudWatch w _ _)                  = w
dreamnetWorld (StInventoryUI w)                   = w
dreamnetWorld (StSkillsUI w _)                    = w
dreamnetWorld (StEquipmentUI w _)                 = w
dreamnetWorld (StTargetSelectionAdjactened w _ _) = w
dreamnetWorld (StTargetSelectionDistant w _ _)    = w
dreamnetWorld (StChoiceSelection w _ _ _)         = w


modifyWorld ∷ (World → World) → GameState gse → GameState gse
modifyWorld wf (StNormal w)                        = StNormal (wf w)
modifyWorld wf (StExamination w s)                 = StExamination (wf w) s
modifyWorld wf (StConversation w cs c)             = StConversation (wf w) cs c
modifyWorld wf (StComputerOperation w v cd)        = StComputerOperation (wf w) v cd
modifyWorld wf (StHudTeam w i)                     = StHudTeam (wf w) i
modifyWorld wf (StHudMessages w)                   = StHudMessages (wf w)
modifyWorld wf (StHudWatch w hs mm)                = StHudWatch (wf w) hs mm
modifyWorld wf (StInventoryUI w)                   = StInventoryUI (wf w)
modifyWorld wf (StSkillsUI w ch)                   = StSkillsUI (wf w) ch
modifyWorld wf (StEquipmentUI w ch)                = StEquipmentUI (wf w) ch
modifyWorld wf (StTargetSelectionAdjactened w t f) = StTargetSelectionAdjactened (wf w) t f
modifyWorld wf (StTargetSelectionDistant w t f)    = StTargetSelectionDistant (wf w) t f
modifyWorld wf (StChoiceSelection w xs i f)        = StChoiceSelection (wf w) xs i f

--------------------------------------------------------------------------------

data SomeGameState = ∀ gse. SomeGS (GameState gse)

--------------------------------------------------------------------------------

runProgramAsPlayer ∷ World → (V2 Int, Int) → Free (ObjectF States) () → SomeGameState
runProgramAsPlayer w p prg = case evalWorld (objectAt p) w of
    Nothing → SomeGS (StNormal w)
    Just o  → let (_, o', sgs) = runObjectMonadForPlayer (p, o, StNormal w) prg `runReader` w
              in  case sgs of
                    (SomeGS gs) → SomeGS (updateVisible (modifyWorld (saveChanges o o') gs))
    where
        saveChanges o o' = execWorld (replaceObject (fst p) o o' {- *> moveObject v o v' -})


withTargetAdjactened ∷ World → (WorldPosition → SomeGameState) → GameState 'TargetSelectionAdjactened
withTargetAdjactened w f = StTargetSelectionAdjactened w (pp w, 0) (TargetActivationF f)
    where
        pp = evalWorld (fst <$> playerPosition)


withTargetDistant ∷ World → (WorldPosition → SomeGameState) → GameState 'TargetSelectionDistant
withTargetDistant w f = StTargetSelectionDistant w (pp w, 0) (TargetActivationF f)
    where
        pp = evalWorld (fst <$> playerPosition)


withChoice ∷ World → [(Char, String)] → (Int → SomeGameState) → GameState 'ChoiceSelection
withChoice w lst f = StChoiceSelection w lst 0 (ChoiceActivationF f)


updateVisible ∷ GameState gse → GameState gse
updateVisible gs =
    let w          = dreamnetWorld gs
        pp         = evalWorld playerPosition w `addStanceHeight` w
        tps        = (`addStanceHeight` w) <$> (memberPosition <$> evalWorld team w)
        raysForAll = (`pointsForOne` w) <$> (pp : tps)
        nw         = execWorld (setVisibility (S.fromList $ mconcat raysForAll)) w
    in  modifyWorld (const nw) gs
    
    -- NOTE resolving 'x' causes lag
    where
        pointsForOne ∷ WorldPosition → World → [V2 Int]
        pointsForOne (p, h) w =
            let points = circle 20 p
                rays   ∷ [[(V2 Int, Bool)]]
                rays   = (\t → evalWorld (castRay (p, h) (t, 3)) w) <$> points
            in  mconcat $ fmap fst . visibleAndOneExtra <$> rays

        visibleAndOneExtra ∷ [(V2 Int, Bool)] → [(V2 Int, Bool)]
        visibleAndOneExtra l =
            let front = takeWhile ((==True) . snd) l
                remv  = dropWhile ((==True) . snd) l
            in  bool (head remv : front) front (null remv)

        -- TODO subtract 1 because standing on the floor counts as if we're in the second 'height box'
        --      floor has to count as having no height, and height calculations must use heights of all
        --      objects in the same cell, not the index itself!
        addStanceHeight ∷ WorldPosition → World → WorldPosition
        addStanceHeight (v, i) = (v,) . max 1 . min 4 . subtract 1 . (+i) . stanceToHeight . valueAt i . evalWorld (cellAt v)

        stanceToHeight = go . fromJustNote "updateVisible!" . preview (_Just.o_state._Person.ch_stance) 
            where
                go Upright = 3
                go Crouch  = 2
                go Prone   = 1


runObjectMonadForPlayer ∷ (MonadReader World m) ⇒ (WorldPosition, Object States, GameState gs) → Free (ObjectF States) () → m (WorldPosition, Object States, SomeGameState)
runObjectMonadForPlayer (cp, o, gs) (Free (Position fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn cp)
runObjectMonadForPlayer (cp, o, gs) (Free (Move v n)) =
    runObjectMonadForPlayer ((v, snd cp), o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (Passable fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn $ view o_passable o)
runObjectMonadForPlayer (cp, o, gs) (Free (SetPassable b n)) =
    let no = o_passable .~ b $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (SeeThrough fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn $ view o_seeThrough o)
runObjectMonadForPlayer (cp, o, gs) (Free (SetSeeThrough b n)) =
    let no = o_seeThrough .~ b $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (CanSee v fn)) =
    asks (evalWorld (castRay (cp) (v, 0))) >>= -- TODO add object height!
        runObjectMonadForPlayer (cp, o, gs) . fn . and . fmap snd
runObjectMonadForPlayer (cp, o, gs) (Free (ChangeSymbol s n)) =
    let no = o_symbol .~ s $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (ChangeMat m n)) =
    let no = o_material .~ m $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (Message _ n)) =
    -- TODO repair set status!
    runObjectMonadForPlayer (cp, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (DoTalk c n)) = do
    w ← ask
    fromMaybe 
        (runObjectMonadForPlayer (cp, o, gs) n)
        (do
            pc ← evalWorld (preview (o_state._Person) <$> playerObject) w
            ch ← preview (o_state._Person) o
            pure $ runObjectMonadForPlayer (cp, o, StConversation w (pc :| [ch]) c) n
        )
runObjectMonadForPlayer (cp, o, gs) (Free (OperateComputer n)) = do
    w ← ask
    maybe
        (runObjectMonadForPlayer (cp, o, gs) n)
        (\cd → runObjectMonadForPlayer (cp, o, StComputerOperation w (fst cp, 1) cd) n)
        (preview (o_state._Computer) o)
runObjectMonadForPlayer (cp, o, gs) (Free (ScanRange r f fn)) = do
    points ← asks (evalWorld (interestingObjects (fst cp) r (f . view o_state)))
    values ← asks (evalWorld (foldr onlyJust [] <$> traverse (fmap (fmap (view o_state) . lastValue) . cellAt) points))
    runObjectMonadForPlayer (cp, o, gs) (fn (zip points values))
    where
        onlyJust (Just x) l = x : l
        onlyJust Nothing  l = l
runObjectMonadForPlayer (cp, o, gs) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → runObjectMonadForPlayer (cp, o, gs) (fn (V2 0 0))
        LineOfSight → runObjectMonadForPlayer (cp, o, gs) (fn (V2 1 1))
runObjectMonadForPlayer (cp, o, gs) (Free (SpawnNewObject _ _ n)) =
    --doWorld $
    --    modifyCell v (addToCell (Object (Symbol '?') "metal" True True 1 s))
    -- TODO fix SpawnNewObject
    runObjectMonadForPlayer (cp, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (RemoveObject _ _ n)) =
    --doWorld $ do
    --    x ← fromJustNote "RemoveObject runObjectMonadForAI" . valueAt i <$> cellAt v
    --    modifyCell v (deleteFromCell x)
    -- TODO fix RemoveObject
    runObjectMonadForPlayer (cp, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (FindObject s fn)) = do
    xs ← asks $ evalWorld $ do
        pp ← fst <$> playerPosition
        interestingObjects pp 60 ((s==) . view o_state)
    if null xs
        then runObjectMonadForPlayer (cp, o, gs) (fn Nothing)
        else do
            let v = head xs
            cellvs ← asks (evalWorld (cellValues <$> cellAt v))
            let mi = s `elemIndex` (view o_state <$> cellvs)
            let r = (v,) <$> mi
            runObjectMonadForPlayer (cp, o, gs) (fn r)
runObjectMonadForPlayer (cp, o, gs) (Pure ()) =
    pure (cp, o, SomeGS gs)
    

