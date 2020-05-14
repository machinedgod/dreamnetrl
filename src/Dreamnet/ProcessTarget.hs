{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Dreamnet.ProcessTarget
where

import Prelude            hiding (head, (!!))
import Control.Lens              (Lens', view, views, _3)
import Control.Lens.Operators
import Data.Singletons           (fromSing)
import Linear                    (V3(V3), _x, _y, _z, _xy)

import qualified Dreamnet.Engine.Input      as Input
import Dreamnet.Engine.Direction
import Dreamnet.Engine.Iteration

import Dreamnet.Game

--------------------------------------------------------------------------------

class ProcessTarget (gsi ∷ GameStateEnum) (ev ∷ Input.TargetEvent) where
    type TgGameStateOut gsi ev ∷ *
    processTarget ∷ GameState gsi → Input.STargetEvent ev → TgGameStateOut gsi ev

--------------------------------------------------------------------------------

instance ProcessTarget 'TargetSelectionAdjactened ('Input.MoveReticule d) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.MoveReticule d) = GameState 'TargetSelectionAdjactened
    processTarget (StTargetSelectionAdjactened w _ z f) (Input.SMoveReticule d) = StTargetSelectionAdjactened w (Just (fromSing d)) z f


instance ProcessTarget 'TargetSelectionAdjactened ('Input.MoveTarget i) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.MoveTarget i) = GameState 'TargetSelectionAdjactened
    processTarget (StTargetSelectionAdjactened w d z f) (Input.SMoveTarget SNext)     = StTargetSelectionAdjactened w d (max (subtract 1 z) 0) f
    processTarget (StTargetSelectionAdjactened w d z f) (Input.SMoveTarget SPrevious) = StTargetSelectionAdjactened w d (min (z + 1) (views (wMap.wmSize._3) fromIntegral w)) f


instance ProcessTarget 'TargetSelectionAdjactened ('Input.SmartTarget i) where
    type TgGameStateOut 'TargetSelectionAdjactened ('Input.SmartTarget i) = GameState 'TargetSelectionAdjactened
    -- TODO implement
    processTarget gs _ = gs


instance ProcessTarget 'TargetSelectionAdjactened 'Input.ConfirmTarget where
    type TgGameStateOut 'TargetSelectionAdjactened 'Input.ConfirmTarget = SomeGameState
    processTarget (StTargetSelectionAdjactened w d z f) _ =
        let dv = (V3 <$> view _x <*> view _y <*> const z) . dirToVec <$> d
            tv = maybe (w ^. wPlayer) (clipToBounds (w ^. wMap)) $
                     (+ view (wPlayer.unpacked) w) <$> dv
        in  runWithTarget f tv

--------------------------------------------------------------------------------

updateSafeVec ∷ WorldMap b o → Lens' (V3 Int) a → (a → a) → Safe (V3 Int) → Safe (V3 Int)
updateSafeVec wm l f = clipToBounds wm . (l %~ f) . unpack


instance ProcessTarget 'TargetSelectionDistant ('Input.MoveReticule d) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.MoveReticule d) = GameState 'TargetSelectionDistant
    processTarget (StTargetSelectionDistant w tp f) (Input.SMoveReticule d) = StTargetSelectionDistant w (updateSafeVec (w ^. wMap) _xy (+ dirToVec' d) tp) f


instance ProcessTarget 'TargetSelectionDistant ('Input.MoveTarget i) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.MoveTarget i) = GameState 'TargetSelectionDistant
    processTarget (StTargetSelectionDistant w tp f) (Input.SMoveTarget SNext) = StTargetSelectionDistant w (updateSafeVec (w ^. wMap) _z lowerTarget tp) f
        where
            lowerTarget i = max 0 (i - 1)
    processTarget (StTargetSelectionDistant w tp f) (Input.SMoveTarget SPrevious) = StTargetSelectionDistant w (updateSafeVec (w ^. wMap) _z maxi tp) f
        where
            maxi i = min (i + 1) $ length (column (w ^. wMap) tp)


instance ProcessTarget 'TargetSelectionDistant ('Input.SmartTarget i) where
    type TgGameStateOut 'TargetSelectionDistant ('Input.SmartTarget i) = GameState 'TargetSelectionDistant
    -- TODO implement
    processTarget gs _ = gs


instance ProcessTarget 'TargetSelectionDistant 'Input.ConfirmTarget where
    type TgGameStateOut 'TargetSelectionDistant 'Input.ConfirmTarget = SomeGameState
    processTarget (StTargetSelectionDistant _ tp f) _ = runWithTarget f tp

