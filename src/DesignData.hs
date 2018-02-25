{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module DesignData
( GameState(..)

, Faction(Faction)
, DreamnetCharacter

, DesignData
, dd_characters
, dd_defaultRedshirt
, dd_startingMap

, defaultDesignData
--, characterForName

, InteractionType(..)
, States(..) -- If I close this, then all conversation code needs to be handled here

, ObjectAPI(..)

, objectFromTile
, playerPerson
, programForObject

) where


import Control.Lens           (view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random   (MonadRandom)
import Data.Semigroup         ((<>))

import Dreamnet.MapGenerator (generateMap)
import Dreamnet.TileMap      (Tile, t_char, loadTileMap)
import Dreamnet.TileData     (ttype, readStringProperty, readBoolProperty,
                              readWordProperty)
import Dreamnet.World        (Object(Object))

import Design.DesignAPI
import Design.GameCharacters
import Design.ObjectPrograms

--------------------------------------------------------------------------------

defaultDesignData ∷ (MonadIO r, MonadRandom r) ⇒ r DesignData
--defaultDesignData = generateMap 50 30 >>= \m → pure $
defaultDesignData = loadTileMap "res/apartmentblock" >>= \m → pure $
    DesignData {
      _dd_characters      = characterDictionary
    , _dd_defaultRedshirt = redshirt
    , _dd_startingMap     = m
    }

--------------------------------------------------------------------------------


-- 1) This *could* all be just a single thing. Object type really does not matter here.
-- 2) Actually, it does, because Object carries a specific state, later used by object programs
objectFromTile ∷ Tile → Object States
objectFromTile t@(ttype → "Base")     = Object (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t) 0                         Empty
objectFromTile t@(ttype → "Door")     = Object (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) 4                         (Prop "Door")
objectFromTile t@(ttype → "Stairs")   = Object (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    1                         (Prop "Stairs")
objectFromTile t@(ttype → "Prop")     = Object (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) (4 `readWordProperty` t)  (Prop (1 `readStringProperty` t))
objectFromTile t@(ttype → "Person")   = Object  '@'            "blue"                      False                    True                    3                         (Person $ characterForName (1 `readStringProperty` t))
objectFromTile   (ttype → "Spawn")    = Object  '.'            "concrete"                  True                     True                    0                         Empty -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
objectFromTile t@(ttype → "Camera")   = Object (view t_char t) "green light"               True                     True                    1                         (Camera (Faction $ 1 `readStringProperty` t) 0)
objectFromTile t@(ttype → "Computer") = Object (view t_char t) "metal"                     False                    True                    1                         Empty
objectFromTile t@(ttype → "Item")     = Object (view t_char t) "blue plastic"              True                     True                    0                         (Prop (1 `readStringProperty` t))
objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
-- TODO Errrrrr, this should be done through the tileset???

playerPerson ∷ String → Object States
playerPerson n = Object '@' "metal" False True 3 (Person $ characterForName n)

