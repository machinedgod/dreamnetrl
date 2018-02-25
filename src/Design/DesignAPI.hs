{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Design.DesignAPI
where

import Control.Lens   (makeLenses)
import Linear         (V2)

import qualified Data.Map as M (Map)

import Dreamnet.TileMap      (TileMap)
import Dreamnet.Character    (Character)
import Dreamnet.Conversation (ConversationNode)
import Dreamnet.World        (Object)

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
               | Examination
               | Operation
               | HudTeam
               | HudMessages
               | HudWatch
               | Conversation
               | InventoryUI
               | CharacterUI
               deriving (Eq, Show)

--------------------------------------------------------------------------------

newtype Faction = Faction String
                deriving (Eq, Show)

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

-- TODO implement Examine as one of interaction types, through making ObjectAPI
-- able to summon UI - this can work if we just change game state
data InteractionType = Examine
                     | Operate
                     | Talk
                     | OperateOn
                     | AiTick

--------------------------------------------------------------------------------

data States = Prop      String
            | Camera    Faction Word
            | Person    DreamnetCharacter
            | Empty
            deriving (Eq, Show) -- TODO just for Debug of UseHeld

--------------------------------------------------------------------------------

type DreamnetCharacter = Character (Object States) ConversationNode Faction 

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter 
    , _dd_defaultRedshirt ∷ DreamnetCharacter
    , _dd_startingMap     ∷ TileMap
    }

makeLenses ''DesignData


