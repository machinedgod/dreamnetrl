{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.ProcessUI
where


import Prelude            hiding (head, (!!))
import Safe                      (at, atMay)
import Control.Lens              (view, preview, traversed, toListOf)
import Control.Monad.Free        (Free(..))
import Data.Bifunctor            (bimap)
import Data.Functor              (($>))
import Data.List                 (genericLength)
import Data.List.NonEmpty        (NonEmpty(..), toList, (!!))
import Data.Maybe                (fromMaybe)
import Linear                    (V2(V2))

import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Character
import Dreamnet.Engine.Object
import qualified Dreamnet.Engine.Input as Input
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration

import Dreamnet.Rendering.Renderer
import Dreamnet.Game

import Design.Items

--------------------------------------------------------------------------------

class ProcessUI (gsi ∷ GameStateEnum) (ev ∷ Input.UIEvent) where
    type UIGameStateOut gsi ev ∷ *
    processUI ∷ (RenderAPI r, Monad r) ⇒ GameState gsi → Input.SUIEvent ev → r (UIGameStateOut gsi ev)

--------------------------------------------------------------------------------

instance ProcessUI 'Examination ('Input.MoveCursor 'North) where
    type UIGameStateOut 'Examination ('Input.MoveCursor 'North) = GameState 'Examination
    processUI gs _ = doScroll scrollUp $> gs


instance ProcessUI 'Examination ('Input.MoveCursor 'South) where
    type UIGameStateOut 'Examination ('Input.MoveCursor 'South) = GameState 'Examination
    processUI gs _ = doScroll scrollDown $> gs

--------------------------------------------------------------------------------

instance ProcessUI 'Conversation ('Input.MoveCursor 'North) where
    type UIGameStateOut 'Conversation ('Input.MoveCursor 'North) = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view chName $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps cn@(Free CChoice{})) _ =
        doChoice selectPrevious $> StConversation w ps cn
    processUI gs _ =
        doScroll scrollUp $> gs


instance ProcessUI 'Conversation ('Input.MoveCursor 'South) where
    type UIGameStateOut 'Conversation ('Input.MoveCursor 'South) = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view chName $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps cn@(Free CChoice{})) _ =
        doChoice selectNext $> StConversation w ps cn
    processUI gs _ =
        doScroll scrollDown $> gs


instance ProcessUI 'Conversation 'Input.SelectChoice where
    type UIGameStateOut 'Conversation 'Input.SelectChoice = GameState 'Conversation
    processUI (StConversation w ps (Free (CName i fn))) e =
        let cname = view chName $ ps !! i
        in  processUI (StConversation w ps (fn cname)) e
    processUI (StConversation w ps (Free (CLastname i fn))) e =
        let clastname = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn clastname)) e
    processUI (StConversation w ps (Free (CNick i fn))) e =
        let cnick = view chLastName $ ps !! i
        in  processUI (StConversation w ps (fn cnick)) e
    processUI (StConversation w ps (Free (CTalk _ _ n))) _ = do
        case n of
            (Free cn) → conversationUpdateUi (view chNickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI (StConversation w ps (Free (CDescribe _ n))) _ = do
        case n of
            (Free cn) → conversationUpdateUi (view chNickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI (StConversation w ps (Free (CReceiveItem _ o n))) _ = do
        -- TODO incorrect, only player now receives items!
        let w' = execWorld (changePlayer (pickUp o)) w
        case n of
            (Free cn) → conversationUpdateUi (view chNickName <$> ps) cn
            _         → pure ()
        pure (StConversation w' ps n)
    processUI (StConversation w ps (Free (CChoice _ fn))) _ = do
        n ← fn <$> currentChoice
        case n of
            (Free cn) → conversationUpdateUi (view chNickName <$> ps) cn
            _         → pure ()
        pure (StConversation w ps n)
    processUI gs _ =
        pure gs


conversationUpdateUi ∷ (RenderAPI g, Monad g, Show o) ⇒ NonEmpty String → ConversationF o a → g ()
conversationUpdateUi _   (CChoice os _)         = conversationUpdateChoice os
conversationUpdateUi nms (CTalk i t _)          = conversationUpdateTalk nms i t
conversationUpdateUi _   (CDescribe t _)        = conversationUpdateDescribe t
conversationUpdateUi nms (CReceiveItem i o _)   = conversationUpdateReceiveItem nms i o
conversationUpdateUi _   _                      = pure ()


conversationUpdateChoice ∷ (RenderAPI g, Monad g) ⇒ [String] → g ()
conversationUpdateChoice os = do
    (p, s) ← (,) <$> positionFor 0 <*> conversationSize
    setChoice (newChoiceData p s os)


conversationUpdateTalk ∷ (RenderAPI g, Monad g) ⇒ NonEmpty String → Int → String → g ()
conversationUpdateTalk nms i t = do
    (p, s) ← (,) <$> positionFor i <*> conversationSize
    setScroll (newScrollData p s (toList nms `atMay` i) t)


conversationUpdateDescribe ∷ (RenderAPI g, Monad g) ⇒ String → g ()
conversationUpdateDescribe t = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    setScroll (newScrollData p s Nothing t)


conversationUpdateReceiveItem ∷ (RenderAPI g, Monad g, Show o) ⇒ NonEmpty String → Int → o → g ()
conversationUpdateReceiveItem nms i o = do
    (p, s) ← (,) <$> positionFor 8 <*> conversationSize
    let t = nms !! i <> " received " <> show o
    setScroll (newScrollData p s Nothing t)

--------------------------------------------------------------------------------

positionFor ∷ (RenderAPI r, Functor r) ⇒ Int → r (V2 Integer)
positionFor i = (\s → fromMaybe (positions s `at` 0) $ (`atMay` i) $ positions s) <$> mainSize
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

conversationSize ∷ (RenderAPI r, Functor r) ⇒ r (V2 Integer)
conversationSize = fmap (`div` 3) . uncurry V2 <$> mainSize

--------------------------------------------------------------------------------

-- TODO this should somehow be a part of computer ObjectAPI code, not here!
-- Note: if I make ability to set the flow function, rather than just gamestate
-- (setting gamestate should probably be an specialization of setting the flow function)
-- at Dreamnet:245 from ObjectAPI, this'll be it.
--------------------------------------------------------------------------------

type family IterateOverHudElements (gs ∷ GameStateEnum) (i ∷ Iteration) ∷ GameStateEnum where
    IterateOverHudElements 'HudTeam 'Next     = 'HudMessages
    IterateOverHudElements 'HudTeam 'Previous = 'HudWatch

    IterateOverHudElements 'HudMessages 'Next     = 'HudWatch
    IterateOverHudElements 'HudMessages 'Previous = 'HudTeam

    IterateOverHudElements 'HudWatch 'Next     = 'HudTeam
    IterateOverHudElements 'HudWatch 'Previous = 'HudMessages

--------------------------------------------------------------------------------

instance ProcessUI 'HudTeam ('Input.Tab i) where
    type UIGameStateOut 'HudTeam ('Input.Tab i) = GameState (IterateOverHudElements 'HudTeam i)
    processUI (StHudTeam w _) (Input.STab SNext)     = pure (StHudMessages w)
    processUI (StHudTeam w _) (Input.STab SPrevious) = pure (StHudWatch w 0 0)


instance ProcessUI 'HudTeam ('Input.MoveCursor 'West) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'West) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SWest) = pure (StHudTeam w (max 0 (i - 1)))


instance ProcessUI 'HudTeam ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'South) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SSouth) = pure (StHudTeam w tp)
        where
            tp = min (i + 3) . genericLength $ toListOf (wTeam.traversed.tmMemberPosition) w


instance ProcessUI 'HudTeam ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'North) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SNorth) = pure (StHudTeam w (max 0 (i - 3)))


instance ProcessUI 'HudTeam ('Input.MoveCursor 'East) where
    type UIGameStateOut 'HudTeam ('Input.MoveCursor 'East) = GameState 'HudTeam
    processUI (StHudTeam w i) (Input.SMoveCursor SEast) = pure (StHudTeam w tp)
        where
            tp = min (i + 1) . genericLength $ toListOf (wTeam.traversed.tmMemberPosition) w


instance ProcessUI 'HudTeam 'Input.SelectChoice where
    type UIGameStateOut 'HudTeam 'Input.SelectChoice = GameState 'SkillsUI
    processUI (StHudTeam w i) _ = pure (StSkillsUI w (completeTeam w `at` i))

--------------------------------------------------------------------------------

instance ProcessUI 'HudMessages ('Input.Tab i) where
    type UIGameStateOut 'HudMessages ('Input.Tab i) = GameState (IterateOverHudElements 'HudMessages i)
    processUI (StHudMessages w) (Input.STab SNext)     = pure (StHudWatch w 0 0)
    processUI (StHudMessages w) (Input.STab SPrevious) = pure (StHudTeam w 0)


instance ProcessUI 'HudMessages ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudMessages ('Input.MoveCursor 'North) = GameState 'HudMessages
    -- TODO scroll
    processUI (StHudMessages w) _ = pure (StHudMessages w)


instance ProcessUI 'HudMessages ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudMessages ('Input.MoveCursor 'South) = GameState 'HudMessages
    -- TODO scroll
    processUI (StHudMessages w) _ = pure (StHudMessages w)


instance ProcessUI 'HudMessages 'Input.SelectChoice where
    type UIGameStateOut 'HudMessages 'Input.SelectChoice = GameState 'HudMessages
    -- TODO use scroll window to show log
    processUI (StHudMessages w) _ = pure (StHudMessages w)

--------------------------------------------------------------------------------

instance ProcessUI 'HudWatch ('Input.Tab i) where
    type UIGameStateOut 'HudWatch ('Input.Tab i) = GameState (IterateOverHudElements 'HudWatch i)
    processUI (StHudWatch w _ _) (Input.STab SNext)     = pure (StHudTeam w 0)
    processUI (StHudWatch w _ _) (Input.STab SPrevious) = pure (StHudMessages w)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'West) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'West) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SWest)  = pure (StHudWatch w t ((b - 1) `mod` 3))


instance ProcessUI 'HudWatch ('Input.MoveCursor 'South) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'South) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SSouth) = pure (StHudWatch w (t - 1) b)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'North) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'North) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SNorth) = pure (StHudWatch w (t + 1) b)


instance ProcessUI 'HudWatch ('Input.MoveCursor 'East) where
    type UIGameStateOut 'HudWatch ('Input.MoveCursor 'East) = GameState 'HudWatch
    processUI (StHudWatch w t b) (Input.SMoveCursor SEast)  = pure (StHudWatch w t ((b + 1) `mod` 3))


instance ProcessUI 'HudWatch 'Input.SelectChoice where
    type UIGameStateOut 'HudWatch 'Input.SelectChoice = GameState 'HudWatch
    processUI gs _ = pure gs


--------------------------------------------------------------------------------

instance ProcessUI 'InventoryUI ('Input.MoveCursor 'North) where
    type UIGameStateOut 'InventoryUI ('Input.MoveCursor 'North) = GameState 'InventoryUI
    processUI gs (Input.SMoveCursor SNorth) = doScroll scrollUp $> gs


instance ProcessUI 'InventoryUI ('Input.MoveCursor 'South) where
    type UIGameStateOut 'InventoryUI ('Input.MoveCursor 'South) = GameState 'InventoryUI
    processUI gs (Input.SMoveCursor SSouth) = doScroll scrollDown $> gs

--------------------------------------------------------------------------------

instance ProcessUI 'SkillsUI ('Input.Tab i) where
    type UIGameStateOut 'SkillsUI ('Input.Tab i) = GameState 'EquipmentUI
    processUI (StSkillsUI w ch) _ = pure (StEquipmentUI w ch)

--------------------------------------------------------------------------------

instance ProcessUI 'EquipmentUI ('Input.Tab i) where
    type UIGameStateOut 'EquipmentUI ('Input.Tab i) = GameState 'SkillsUI
    processUI (StEquipmentUI w ch) _ = pure (StSkillsUI w ch)

--------------------------------------------------------------------------------
-- TODO actually uncomment team code to return, err, team
completeTeam ∷ World → [DreamnetCharacter]
completeTeam w =
    case preview (oState._Person) (playerObject w) of
        Nothing → []
        Just p  → [p]
    {-
    let t = flip evalWorld w $ team >>=
                               traverse (fmap fromJustNote . teamMemberPosition) >>=
                               traverse (fmap fromJustNote . uncurry valueAt . unwrapWorldCoord)
        p = flip evalWorld w $ playerPosition >>=
                               fmap fromJustNote . uncurry valueAt . unwrapWorldCoord
    in  (\(Person chp) → chp) (p ^. oState) : ((\(Person tm) → tm) . view oState <$> t)
    -}

