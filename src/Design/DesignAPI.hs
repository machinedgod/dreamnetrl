{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Design.DesignAPI
where

import Safe               (at, atMay)
import Control.Lens       (makeLenses, view)
import Control.Monad.Free (Free)
import Linear             (V2(V2))
import Data.Bifunctor     (bimap)
import Data.Maybe         (fromMaybe, isJust)
import Data.Semigroup     ((<>))

import qualified Data.Map as M (Map)

import Dreamnet.ChoiceData         (ChoiceData, newChoiceData)
import Dreamnet.ScrollData         (ScrollData, newScrollData)
import Dreamnet.TileMap            (TileMap)
import Dreamnet.Character          (ItemTraits(..), SlotType(..), Character, ch_name)
import Dreamnet.ComputerModel      (ComputerData)
import Dreamnet.Conversation       (ConversationNode(..))
import Dreamnet.ConversationMonad  (ConversationF)
import Dreamnet.World              (Symbol, Object)

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
    changeSymbol       ∷ Symbol → o ()
    changeMat          ∷ String → o ()
    message            ∷ String → o ()
    put                ∷ States → o ()
    get                ∷ o States
    scanRange          ∷ Word → (Object States → Bool) → o [(V2 Int, Object States)]
    -- Keep adding primitives until you can describe all Map Objects as programs


modify ∷ (ObjectAPI o, Monad o) ⇒ (States → States) → o ()
modify f = get >>= put . f 

--------------------------------------------------------------------------------

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


instance Show (WearableItem i) where
    show wi = _wi_name wi

instance ItemTraits (WearableItem i) where
    isContainer = isJust . _wi_containerVolume

--------------------------------------------------------------------------------

data AmmoType = LaserjetBattery
              deriving (Eq)


data WeaponItem = WeaponItem {
      _wpi_name     ∷ String
    , _wpi_ammoType ∷ AmmoType
    }
    deriving (Eq)


data AmmoItem = AmmoItem {
      _ami_name        ∷ String
    , _ami_type        ∷ AmmoType
    , _ami_currentLoad ∷ Word
    , _ami_maxLoad     ∷ Word
    }
    deriving (Eq)

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
            | Clothes   (WearableItem States)
            | Weapon    WeaponItem
            | Ammo      AmmoItem
            | Empty
            deriving (Eq)

instance Show States where
    show (Prop s)     = "A " <> s
    show (Camera _ _) = "A camera."
    show (Person ch)  = view ch_name ch
    show (Computer _) = "A computer"
    show Door         = "An generic, common object that switches collidable state when activated."
    show Mirror       = "You're so pretty!"
    show (Clothes wi) = _wi_name wi
    show (Weapon wpi) = _wpi_name wpi
    show (Ammo ami)   = _ami_name ami
    show Empty        = "This, is, uh... nothing."


instance ItemTraits States where
    isContainer (Clothes wi) = isContainer wi
    isContainer _            = False


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
    , _dd_startingMap     ∷ TileMap
    }
makeLenses ''DesignData

--------------------------------------------------------------------------------
-- TODO can't find shit if placed next to datas :-(
makeLenses ''WearableItem
makeLenses ''WeaponItem
makeLenses ''AmmoItem
