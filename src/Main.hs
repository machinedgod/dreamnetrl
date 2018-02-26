{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random   (MonadRandom, evalRandT, mkStdGen)

import Dreamnet.TileMap   (loadTileMap)
import Dreamnet.Dreamnet

import Design.DesignAPI
import Design.GameCharacters


defaultDesignData ∷ (MonadIO r, MonadRandom r) ⇒ r DesignData
defaultDesignData = do
--  m ← generateMap 50 30
    m ← loadTileMap "res/bar"
    --m ← loadTileMap "res/apartmentblock"
    rnd ← traverse randomizeStats characters
    pure $
        DesignData {
          _dd_characters      = characterDictionary rnd
        , _dd_defaultRedshirt = redshirt
        , _dd_startingMap     = m
        }


main ∷ IO ()
main = 
    let seed = 0
    in  evalRandT defaultDesignData (mkStdGen seed) >>= launchDreamnet
