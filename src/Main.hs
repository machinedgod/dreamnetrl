{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random   (MonadRandom, evalRandT, mkStdGen)

import Dreamnet.TileMap
import Dreamnet.MapGenerator
import Dreamnet.Dreamnet

import Design.DesignAPI
import Design.GameCharacters


defaultDesignData ∷ (MonadIO r, MonadRandom r) ⇒ r DesignData
defaultDesignData = do
    m ← loadTileMap "./res/bar"
    --m ← generateMap
    pure $
        DesignData {
          _dd_characters  = characterDictionary characters
        , _dd_startingMap = m
        }


main ∷ IO ()
main = 
    let seed = 0
    in  evalRandT defaultDesignData (mkStdGen seed) >>= launchDreamnet
