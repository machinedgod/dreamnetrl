{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Design.DesignAPI
where

import Safe           (at)
import Control.Lens   (makeLenses)
import Linear         (V2)

import qualified Data.Map as M (Map)

import Dreamnet.ChoiceData    (ChoiceData, newChoiceData)
import Dreamnet.ScrollData    (ScrollData, newScrollData)
import Dreamnet.TileMap       (TileMap)
import Dreamnet.Character     (Character)
import Dreamnet.ComputerModel (ComputerData)
import Dreamnet.Conversation  (ConversationNode(..))
import Dreamnet.World         (Object)

--------------------------------------------------------------------------------
-- Object API and objects

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI o where
    designData         ∷ o DesignData
    position           ∷ o (V2 Int)
    move               ∷ V2 Int → o ()
    showInfoWindow     ∷ String → o ()
    showComputerWindow ∷ ComputerData → o ()
    startConversation  ∷ DreamnetCharacter → o ()
    passable           ∷ o Bool
    setPassable        ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough         ∷ o Bool
    setSeeThrough      ∷ Bool → o ()
    canSee             ∷ V2 Int → o Bool
    changeChar         ∷ Char → o ()
    changeMat          ∷ String → o ()
    message            ∷ String → o ()
    put                ∷ States → o ()
    get                ∷ o States
    scanRange          ∷ Word → (Object States → Bool) → o [(V2 Int, Object States)]
    -- Keep adding primitives until you can describe all Map Objects as programs

modify ∷ (ObjectAPI o, Monad o) ⇒ (States → States) → o ()
modify f = get >>= put . f 

--------------------------------------------------------------------------------

data InteractionType = Examine
                     | Operate
                     | Talk
                     | OperateOn
                     | AiTick


newtype Faction = Faction String
                deriving (Eq, Show)


data States = Prop      String
            | Camera    Faction Word
            | Person    DreamnetCharacter
            | Computer  ComputerData
            | Empty
            deriving (Eq, Show)


type DreamnetCharacter = Character (Object States) ConversationNode Faction 

--------------------------------------------------------------------------------

data GameState = Quit
               | Normal
               | Examination        ScrollData
               | ComputerOperation  (V2 Int) Int ComputerData
               | HudTeam            Int
               | HudMessages
               | HudWatch           Int Int
               | ConversationFlow   ConversationNode ScrollData
               | ConversationChoice ConversationNode ChoiceData
               | InventoryUI        ScrollData
               | SkillsUI           DreamnetCharacter
               | EquipmentUI        DreamnetCharacter


createConversationState ∷ V2 Integer → V2 Integer → ConversationNode → GameState
createConversationState pos siz cn@(ChoiceNode opts _) =
    ConversationChoice cn (newChoiceData pos siz opts)
createConversationState pos siz cn@(TalkNode s i ps _) =
    ConversationFlow cn (newScrollData pos siz (Just $ at ps i) s)
createConversationState pos siz cn@(DescriptionNode s _) =
    ConversationFlow cn (newScrollData pos siz Nothing s) -- TODO centered
createConversationState _ _ End =
    Normal

            --use g_gameState >>= \case
            --    Conversation → do
            --        let ch = views o_state (\(Person ch') → ch') $ o
            --        g_conversant .= Just ch
            --        g_conversation .= view ch_conversation ch

            --        use g_conversation >>= \case
            --            (ChoiceNode opts _) → g_choiceWindow %= setOptions opts
            --            (TalkNode s _) → do
            --                pos ← lift (positionFor 0)
            --                siz ← lift conversationSize
            --                g_rendererData.rd_scrollData .= newScrollData pos siz (Just (view ch_name ch)) s
            --            (ListenNode s _) → do
            --                pos ← lift (positionFor 1)
            --                siz ← lift conversationSize
            --                g_rendererData.rd_scrollData .= newScrollData pos siz (Just (view ch_name ch)) s
            --            _ → pure ()
            --        use g_conversation >>= renderConversation

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter 
    , _dd_defaultRedshirt ∷ DreamnetCharacter
    , _dd_startingMap     ∷ TileMap
    }

makeLenses ''DesignData


