{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Design.DesignAPI
where

import Control.Lens   (makeLenses, view)
import Linear         (V2)

import qualified Data.Map as M (Map)

import Dreamnet.ChoiceData   (ChoiceData, newChoiceData)
import Dreamnet.ScrollData   (ScrollData, newScrollData)
import Dreamnet.TileMap      (TileMap)
import Dreamnet.Character    (Character, ch_name)
import Dreamnet.Conversation (ConversationNode(..))
import Dreamnet.World        (Object)

--------------------------------------------------------------------------------
-- Object API and objects

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI o where
    designData        ∷ o DesignData
    position          ∷ o (V2 Int)
    move              ∷ V2 Int → o ()
    showInfoWindow    ∷ String → o ()
    startConversation ∷ DreamnetCharacter → o ()
    passable          ∷ o Bool
    setPassable       ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough        ∷ o Bool
    setSeeThrough     ∷ Bool → o ()
    canSee            ∷ V2 Int → o Bool
    changeChar        ∷ Char → o ()
    changeMat         ∷ String → o ()
    message           ∷ String → o ()
    put               ∷ States → o ()
    get               ∷ o States
    scanRange         ∷ Word → (Object States → Bool) → o [(V2 Int, Object States)]
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
            | Empty
            deriving (Eq, Show) -- TODO just for Debug of UseHeld


type DreamnetCharacter = Character (Object States) ConversationNode Faction 

--------------------------------------------------------------------------------

-- TODO this could be a Character state,
--      this way it circularly ties back into the game
--      (when rendering characters), rather than World
--      having to produce a new GameState
--
--      Dunno what's better
--
--      After 30 seconds - its the same. *direction* of data
--      is the same.
--
--      Bottomline is, changes in the world dictate changes in the
--      player's domain (UI, controls, etc)
data GameState = Normal
               | Examination        ScrollData
               | Operation
               | HudTeam            Int
               | HudMessages
               | HudWatch           Int Int
               | ConversationFlow   DreamnetCharacter ConversationNode ScrollData
               | ConversationChoice DreamnetCharacter ConversationNode ChoiceData
               | InventoryUI        ScrollData
               | SkillsUI           DreamnetCharacter
               | EquipmentUI        DreamnetCharacter
               deriving (Eq, Show)


createConversationState ∷ V2 Integer → V2 Integer → DreamnetCharacter → ConversationNode → GameState
createConversationState pos siz ch cn@(ChoiceNode opts _) =
    ConversationChoice ch cn (newChoiceData pos siz opts)
createConversationState pos siz ch cn@(TalkNode s _) =
    ConversationFlow ch cn (newScrollData pos siz (Just "<CARLA>") s)
createConversationState pos siz ch cn@(ListenNode s _) =
    ConversationFlow ch cn (newScrollData pos siz (Just $ view ch_name ch) s)
createConversationState _ _ _ End =
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


