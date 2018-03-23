{-# LANGUAGE UnicodeSyntax #-}

module Design.Items
where

import Data.Semigroup       ((<>))

import Dreamnet.Character   (SlotType(..))

import Design.DesignAPI

--------------------------------------------------------------------------------
-- Weapons: ranged + clips

laserjet ∷ WeaponItem
laserjet = WeaponItem {
      _wpi_name     = "Laserjet"
    , _wpi_ammoType = LaserjetBattery
    }


laserjetClip ∷ AmmoItem
laserjetClip = AmmoItem {
      _ami_name        = "Laserjet battery"
    , _ami_type        = LaserjetBattery
    , _ami_currentLoad = 100
    , _ami_maxLoad     = 100
    }



--------------------------------------------------------------------------------
-- Weapons: throwable

fragmentGrenade ∷ WeaponItem
fragmentGrenade = WeaponItem {
      _wpi_name = "Fragment grenade"
    }

--------------------------------------------------------------------------------
-- Clothes

backpack ∷ [States] → WearableItem States
backpack items = WearableItem {
      _wi_name       = "Military backpack"
    , _wi_equippedAt = Back
    , _wi_volume     = 30 -- TODO so random
    , _wi_weight     = 1.0  -- TODO also random
    , _wi_material   = Kevlar
    , _wi_coverage   = 0.8

    , _wi_containerVolume = Just 29
    , _wi_storedItems = items
    }


clipBelt ∷ [States] → WearableItem States
clipBelt items = WearableItem {
      _wi_name       = "Military belt"
    , _wi_equippedAt = Belt
    , _wi_volume     = 4 -- TODO so random
    , _wi_weight     = 1.0  -- TODO also random
    , _wi_material   = Polyester
    , _wi_coverage   = 0.9

    , _wi_containerVolume = Just 12
    , _wi_storedItems = items
    }


headband ∷ WearableItem States
headband = WearableItem {
      _wi_name       = "Delgado's headband"
    , _wi_equippedAt = Head
    , _wi_volume     = 1 -- TODO so random
    , _wi_weight     = 0.1 -- TODO also random
    , _wi_material   = Cotton
    , _wi_coverage   = 0.3

    , _wi_containerVolume = Nothing
    , _wi_storedItems = []
    }


armourPiece ∷ SlotType → Material → WearableItem States
armourPiece st mat = WearableItem {
      _wi_name       = show mat <> " " <> show st <> " " <> " armour"
    , _wi_equippedAt = st
    , _wi_volume     = 4 -- TODO so random
    , _wi_weight     = 2 -- TODO also random, should depend on material
    , _wi_material   = mat
    , _wi_coverage   = 0.7

    , _wi_containerVolume = Nothing
    , _wi_storedItems = []
    }


boot ∷ Material → WearableItem States
boot mat = WearableItem {
      _wi_name       = show mat <> " boot"
    , _wi_equippedAt = Foot
    , _wi_volume     = 4 -- TODO so random
    , _wi_weight     = 2 -- TODO also random, should depend on material
    , _wi_material   = mat
    , _wi_coverage   = 0.7

    , _wi_containerVolume = Just 6
    , _wi_storedItems = []
    }

