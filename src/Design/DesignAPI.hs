{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Design.DesignAPI
where

import Safe               (at, atMay)
import Control.Lens       (makeLenses, view)
import Control.Monad.Free (Free)
import Linear             (V2(V2))
import Data.Bifunctor     (bimap)
import Data.Maybe         (fromMaybe)
import Data.Semigroup     ((<>))

import qualified Data.Map as M (Map)

import Dreamnet.ChoiceData         (ChoiceData, newChoiceData)
import Dreamnet.ScrollData         (ScrollData, newScrollData)
import Dreamnet.TileMap            (TileMap)
import Dreamnet.Character          (Character, ch_name)
import Dreamnet.ComputerModel      (ComputerData)
import Dreamnet.Conversation       (ConversationNode(..))
import Dreamnet.ConversationMonad  (ConversationF)
import Dreamnet.World              (Object)

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
                     | OperateOn   States
                     | OperateWith States
                     | AiTick


newtype Faction = Faction String
                deriving (Eq, Show)


-- TODO Not happy with this development!
data States = Prop      String
            | Camera    Faction Word
            | Person    DreamnetCharacter
            | Computer  ComputerData
            | Door
            | Mirror
            | Empty
            deriving (Eq)


instance Show States where
   show (Prop s)      = "Prop " <> s
   show (Camera f l)  = "Camera " <> show f <> ", visible foes: " <> show l
   show (Person ch)   = "Person named " <> view ch_name ch
   show (Computer cd) = "Computer: " <> show cd
   show Door          = "Door"
   show Mirror        = "Mirror"
   show Empty         = "Empty"
 

type DreamnetCharacter = Character States (Free ConversationF ()) Faction 

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

--------------------------------------------------------------------------------

createConversationState ∷ (Integer, Integer) → ConversationNode → GameState
createConversationState ss cn@(ChoiceNode opts _) =
    let sd = newChoiceData (positionFor 0 ss) (conversationSize ss) opts
    in  ConversationChoice cn sd
createConversationState ss cn@(TalkNode s i ps _) =
    let n  = Just $ at ps (fromIntegral i)
        sd = newScrollData (positionFor i ss) (conversationSize ss) n s
    in  ConversationFlow cn sd
createConversationState ss cn@(DescriptionNode s _) =
    let sd = newScrollData (positionFor 8 ss) (conversationSize ss) Nothing s
    in  ConversationFlow cn sd
createConversationState _ End =
    Normal


-- Not sure if these belong in here? Maybe somehow in the renderer? Have ConversationFlow carry higher level data?
positionFor ∷ Word → (Integer, Integer) → V2 Integer
positionFor (fromIntegral → i) s = fromMaybe (positions s `at` 0) . (`atMay` i) . positions $ s
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


conversationSize ∷ (Integer, Integer) → V2 Integer
conversationSize = fmap (`div` 3) . uncurry V2

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter 
    , _dd_defaultRedshirt ∷ DreamnetCharacter
    , _dd_startingMap     ∷ TileMap
    }

makeLenses ''DesignData


