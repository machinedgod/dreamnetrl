{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

, DesignData(..), ddCharacters, ddDevStartingMap

, runProgramAsPlayer, withTargetAdjactened, withTargetDistant, withChoice,
  updateVisible
) where

import Prelude             hiding (map)
import Safe                       (fromJustNote)
import Control.Lens               (makeLenses, view, preview, (^?),
                                   (.~), _Just, (^..), (^.))
import Control.Monad.Free         (Free(Free, Pure))
import Control.Monad.Reader       (Reader, runReader, ask, asks)
import Data.Bool                  (bool)
import Data.Maybe                 (fromMaybe)
import Data.List.NonEmpty         (NonEmpty((:|)))
import Linear                     (V2, V3(V3), _x, _y, _z, _xy)

import qualified Data.Map    as M (Map)
import qualified Data.Set    as S (fromList)

import Dreamnet.Engine.Utils
import Dreamnet.Engine.Object
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Visibility

import Dreamnet.World
import Dreamnet.ObjectStates

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _ddCharacters      ∷ M.Map String DreamnetCharacter
      -- TODO move item dictionaries here
    , _ddDevStartingMap ∷ String
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
      runWithTarget ∷ Safe (V3 Int) → SomeGameState
    }

newtype ChoiceActivationF = ChoiceActivationF {
      runWithChoice ∷ Int → SomeGameState
    }

--------------------------------------------------------------------------------

data GameStateEnum =
      Normal
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
    StComputerOperation ∷ World → Safe (V3 Int) → ComputerData                                → GameState 'ComputerOperation

    StHudTeam     ∷ World → SelectedCharacter → GameState 'HudTeam
    StHudMessages ∷ World                     → GameState 'HudMessages
    StHudWatch    ∷ World → Turn → Button     → GameState 'HudWatch
    StInventoryUI ∷ World                     → GameState 'InventoryUI
    StSkillsUI    ∷ World → DreamnetCharacter → GameState 'SkillsUI
    StEquipmentUI ∷ World → DreamnetCharacter → GameState 'EquipmentUI

    StTargetSelectionAdjactened ∷ World → Maybe Direction  → Int → TargetActivationF → GameState 'TargetSelectionAdjactened
    StTargetSelectionDistant    ∷ World → Safe (V3 Int)          → TargetActivationF → GameState 'TargetSelectionDistant
    StChoiceSelection           ∷ World → [(Char, String)] → Int → ChoiceActivationF → GameState 'ChoiceSelection


dreamnetWorld ∷ GameState gse → World
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
dreamnetWorld (StTargetSelectionDistant w _ _)      = w
dreamnetWorld (StChoiceSelection w _ _ _)           = w


modifyWorld ∷ (World → World) → GameState gse → GameState gse
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
modifyWorld wf (StTargetSelectionAdjactened w d z f) = StTargetSelectionAdjactened (wf w) d z f
modifyWorld wf (StTargetSelectionDistant w t f)      = StTargetSelectionDistant (wf w) t f
modifyWorld wf (StChoiceSelection w xs i f)          = StChoiceSelection (wf w) xs i f

--------------------------------------------------------------------------------

data SomeGameState = ∀ gse. SomeGS (GameState gse)

--------------------------------------------------------------------------------

-- TODO run only if Safe (V3 Int) actually contains an object!
-- TODO this makes it impossible to run programs on held objects
runProgramAsPlayer ∷ World → Safe (V3 Int) → Free (ObjectF States) () → SomeGameState
runProgramAsPlayer w p prg = fromMaybe (SomeGS (StNormal w)) $ do
    o ← objectAt map p
    --o ← cellAt map p ^? followLinksL w._Just
    case runReader (runObjectMonadForPlayer (context o) prg) w of
       (_, no, SomeGS gs) → pure $ SomeGS (updateVisible (modifyWorld (saveChanges no) gs))
    where
        map         = w ^. wMap
        context     = (p, , StNormal w)
        saveChanges = execWorld . doMap . modifyObject p . const
--runProgramAsPlayer w p prg =
--    let wc            = cellAt (w ^. wMap) p
--        (_, no, rsgs) = maybe
--                            (p, wc, SomeGS (StNormal w))
--                            (\o → let (np, nno, sgs) = runReader (pure (p, o, SomeGS $ StNormal w)) w
--                                  in  (np, mkCell nno, sgs))
--                            --Just o  → let (p, o, sgs) = runReader (runObjectMonadForPlayer (p, o, StNormal w) prg) w
--                            (wc ^. wcContents)
--    in  case rsgs of -- Need case to pattern match on SomeGameState
--            (SomeGS gs) → let saveChanges = execWorld . modifyCellM p . const
--                          in  SomeGS (updateVisible (modifyWorld (saveChanges no) gs))


withTargetAdjactened ∷ World → (Safe (V3 Int) → SomeGameState) → GameState 'TargetSelectionAdjactened
withTargetAdjactened w f = StTargetSelectionAdjactened w Nothing (w ^. wPlayer.unpacked._z) (TargetActivationF f)


withTargetDistant ∷ World → (Safe (V3 Int) → SomeGameState) → GameState 'TargetSelectionDistant
withTargetDistant w f = StTargetSelectionDistant w (view wPlayer w) (TargetActivationF f)


withChoice ∷ World → [(Char, String)] → (Int → SomeGameState) → GameState 'ChoiceSelection
withChoice w lst f = StChoiceSelection w lst 0 (ChoiceActivationF f)


-- TODO World → World, not GameState → GameState
updateVisible ∷ GameState gse → GameState gse
updateVisible gs =
    let w          = dreamnetWorld gs
        m          = view wMap w
        pp         = addStanceHeight (view wPlayer w) m
        tps        = (`addStanceHeight` m) <$> (w ^.. wTeam.traverse.tmMemberPosition)
        raysForAll = (`pointsForOne` m) <$> (pp : tps)
        nw         = execWorld (setVisibility (S.fromList $ mconcat raysForAll)) w
    in  modifyWorld (const nw) gs

    -- NOTE resolving 'x' causes lag
    where
        pointsForOne ∷ (VisibleAPI o) ⇒ Safe (V3 Int) → WorldMap b o → [V2 Int]
        pointsForOne p m =
            let points = clipToBounds m . (\v → V3 (v ^. _x) (v ^. _y) 0) <$> circle 20 (unpack p ^. _xy)
                rays   ∷ [[Safe (V2 Int, Bool)]]
                rays   = castRay m p <$> points
            in  mconcat (fmap fst . visibleAndOneExtra <$> rays)

        visibleAndOneExtra ∷ [Safe (V2 Int, Bool)] → [(V2 Int, Bool)]
        visibleAndOneExtra (fmap unpack → l) =
            let front = takeWhile ((==True) . snd) l
                remv  = dropWhile ((==True) . snd) l
            in  bool (head remv : front) front (null remv)

        -- TODO subtract 1 because standing on the floor counts as if we're in the second 'height box'
        --      floor has to count as having no height, and height calculations must use heights of all
        --      objects in the same cell, not the index itself!
        addStanceHeight ∷ Safe (V3 Int) → WorldMap b (Object States) → Safe (V3 Int)
        addStanceHeight v m = clipToBounds m nv
            where
                nv   = V3 <$> view _x <*> view _y <*> const newZ $ unpack v
                newZ = max 1 . min 4 . subtract 1 . (+ view _z (unpack v)) .
                            stanceToHeight m $ cellAt m v

        stanceToHeight m = go . fromJustNote "updateVisible!" . preview stance
            where
                stance     = followLinksL m._Just.oState._Person.chStance
                go Upright = 3
                go Crouch  = 2
                go Prone   = 1


-- TODO get rid of object monad, or at least, rework it heavily!
type RunObjectMonadData o = (Safe (V3 Int), Object States, o)


runObjectMonadForPlayer ∷ RunObjectMonadData (GameState gs) → Free (ObjectF States) () → Reader World (RunObjectMonadData SomeGameState)
runObjectMonadForPlayer (cp, o, gs) (Free (Position fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn cp)
runObjectMonadForPlayer (_, o, gs) (Free (Move v n)) =
    runObjectMonadForPlayer (v, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (Passable fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn $ view oPassable o)
runObjectMonadForPlayer (cp, o, gs) (Free (SetPassable b n)) =
    let no = oPassable .~ b $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (SeeThrough fn)) =
    runObjectMonadForPlayer (cp, o, gs) (fn $ view oSeeThrough o)
runObjectMonadForPlayer (cp, o, gs) (Free (SetSeeThrough b n)) =
    let no = oSeeThrough .~ b $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (CanSee v fn)) =
    asks (\w → castRay (view wMap w) cp v) >>=
    --asks (\w → castRay (view wMap w) (const True) cp v) >>=
        runObjectMonadForPlayer (cp, o, gs) . fn . and . fmap (snd . unpack)
runObjectMonadForPlayer (cp, o, gs) (Free (ChangeSymbol s n)) =
    let no = oSymbol .~ s $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (ChangeMat m n)) =
    let no = oMaterial .~ m $ o
    in  runObjectMonadForPlayer (cp, no, gs) n
runObjectMonadForPlayer (cp, o, _) (Free (Message m _)) =
    -- TODO repair set status!
    --runObjectMonadForPlayer (cp, o, gs) n
    asks ((cp, o,) . SomeGS . flip StExamination m)
    -- Old code, testing ^
    --pure (cp, o, SomeGS gs)
runObjectMonadForPlayer (cp, o, gs) (Free (DoTalk c n)) = do
    w ← ask
    fromMaybe
        (runObjectMonadForPlayer (cp, o, gs) n)
        (do
            pc ← playerObject w ^? oState._Person
            --pc ← evalWorld (preview (oState._Person) <$> playerObject) w
            ch ← o ^? oState._Person
            pure $ runObjectMonadForPlayer (cp, o, StConversation w (pc :| [ch]) c) n
        )
runObjectMonadForPlayer (cp, o, gs) (Free (OperateComputer n)) = ask >>= \w →
    maybe
        (runObjectMonadForPlayer (cp, o, gs) n)
        (\cd → runObjectMonadForPlayer (cp, o, StComputerOperation w cp cd) n)
        (o ^? (oState._Computer))
runObjectMonadForPlayer (cp, o, gs) (Free (ScanRange r f fn)) =
    ask >>= \w →
        let wm     = view wMap w
            cells  = interestingObjects wm cp r (predIfJust wm (f . view oState))
            cells' = foldr (collectNonEmpty wm) [] cells
        in  runObjectMonadForPlayer (cp, o, gs) (fn (fmap (view oState) <$>cells'))
runObjectMonadForPlayer (cp, o, gs) (Free (AcquireTarget s fn)) =
    case s of
        Freeform    → view wPlayer >>= runObjectMonadForPlayer (cp, o, gs) . fn
        LineOfSight → view wPlayer >>= runObjectMonadForPlayer (cp, o, gs) . fn
runObjectMonadForPlayer (cp, o, gs) (Free (SpawnNewObject _ _ n)) =
    --doWorld $
    --    modifyCell v (addToCell (Object (Symbol '?') "metal" True True 1 s))
    -- TODO fix SpawnNewObject
    runObjectMonadForPlayer (cp, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (RemoveObject _ n)) =
    --doWorld $ do
    --    x ← fromJustNote "RemoveObject runObjectMonadForAI" . valueAt i <$> cellAt v
    --    modifyCell v (deleteFromCell x)
    -- TODO fix RemoveObject
    runObjectMonadForPlayer (cp, o, gs) n
runObjectMonadForPlayer (cp, o, gs) (Free (FindObject s fn)) = do
    wm ← view wMap
    pp ← view wPlayer
    case interestingObjects wm pp 60 (interestFilter wm) of
        []    → runObjectMonadForPlayer (cp, o, gs) (fn Nothing)
        (v:_) → runObjectMonadForPlayer (cp, o, gs) (fn (Just v))
            --o ← asks (view (_Just.oState) . cellAt v . view wMap)
            --let r = (v,) <$> mi
    where
        interestFilter wm = maybe False (s==) . preview (followLinksL wm._Just.oState)
runObjectMonadForPlayer (cp, o, gs) (Pure ()) =
    pure (cp, o, SomeGS gs)

