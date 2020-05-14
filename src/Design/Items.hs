{-# LANGUAGE UnicodeSyntax #-}

module Design.Items
( module Dreamnet.ObjectStates

, clothesDict, weaponsDict, ammoDict, throwableDict, consumableDict

, laserjet, laserjetClip
, fragmentGrenade
, backpack
, clipBelt
, headband
, armourPiece
, boot
) where

import Control.Lens    (view)

import qualified Data.Map as M (Map, fromList, singleton)

import Dreamnet.ObjectStates

--------------------------------------------------------------------------------
-- Dictionaries
clothesDict ∷ M.Map String (WearableItem States)
clothesDict = M.fromList
    [ asTuple (backpack [])
    ]
    where
        asTuple = (,) <$> view wiName <*> id

weaponsDict ∷ M.Map String WeaponItem
weaponsDict = M.fromList
    []

ammoDict ∷ M.Map String AmmoItem
ammoDict = M.fromList
    []

throwableDict ∷ M.Map String ThrownWeaponItem
throwableDict = M.fromList
    []

consumableDict ∷ M.Map String ConsumableItem
consumableDict = M.fromList
    []

--------------------------------------------------------------------------------
-- Weapons: ranged + clips

laserjet ∷ WeaponItem
laserjet = WeaponItem {
      _wpiName        = "Laserjet"
    , _wpiDescription = "Laserjet is a proprietary Lens & Optix, Inc. product. Basically a concentrated 1m wide ray of focused, coherent heat energy, enough per cubical centimeter to instantly convert any carbon-based organism back to just carbon."
    , _wpiSettings    = M.singleton "power" "10"
    , _wpiAmmoType    = LaserjetBattery
    }


laserjetClip ∷ AmmoItem
laserjetClip = AmmoItem {
      _amiName        = "Laserjet battery"
    , _amiType        = LaserjetBattery
    , _amiCurrentLoad = 100
    , _amiMaxLoad     = 100
    }



--------------------------------------------------------------------------------
-- Weapons: throwable

fragmentGrenade ∷ ThrownWeaponItem
fragmentGrenade = ThrownWeaponItem {
      _twiName = "Fragment grenade"
    }

--------------------------------------------------------------------------------
-- Clothes

backpack ∷ [States] → WearableItem States
backpack items = WearableItem {
      _wiName       = "Military backpack"
    , _wiEquippedAt = Back
    , _wiVolume     = 30 -- TODO so random
    , _wiWeight     = 1.0  -- TODO also random
    , _wiMaterial   = Kevlar
    , _wiCoverage   = 0.8

    , _wiContainerVolume = Just 29
    , _wiStoredItems = items
    }


clipBelt ∷ [States] → WearableItem States
clipBelt items = WearableItem {
      _wiName       = "Military belt"
    , _wiEquippedAt = Belt
    , _wiVolume     = 4 -- TODO so random
    , _wiWeight     = 1.0  -- TODO also random
    , _wiMaterial   = Polyester
    , _wiCoverage   = 0.9

    , _wiContainerVolume = Just 12
    , _wiStoredItems = items
    }


headband ∷ WearableItem States
headband = WearableItem {
      _wiName       = "Delgado's headband"
    , _wiEquippedAt = Head
    , _wiVolume     = 1 -- TODO so random
    , _wiWeight     = 0.1 -- TODO also random
    , _wiMaterial   = Cotton
    , _wiCoverage   = 0.3

    , _wiContainerVolume = Nothing
    , _wiStoredItems = []
    }


armourPiece ∷ SlotType → Material → WearableItem States
armourPiece st mat = WearableItem {
      _wiName       = show mat <> " " <> show st <> " " <> " armour"
    , _wiEquippedAt = st
    , _wiVolume     = 4 -- TODO so random
    , _wiWeight     = 2 -- TODO also random, should depend on material
    , _wiMaterial   = mat
    , _wiCoverage   = 0.7

    , _wiContainerVolume = Nothing
    , _wiStoredItems = []
    }


boot ∷ Material → WearableItem States
boot mat = WearableItem {
      _wiName       = show mat <> " boot"
    , _wiEquippedAt = Foot
    , _wiVolume     = 4 -- TODO so random
    , _wiWeight     = 2 -- TODO also random, should depend on material
    , _wiMaterial   = mat
    , _wiCoverage   = 0.7

    , _wiContainerVolume = Just 6
    , _wiStoredItems = []
    }

