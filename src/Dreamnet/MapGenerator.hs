{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.MapGenerator
where


import Safe                   (headMay)
import Control.Lens           ((%=), uses, views)
import Control.Monad.Random   (MonadRandom)
import Control.Monad.State    (MonadState, execStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Semigroup         ((<>))
import Data.Maybe             (fromMaybe)
import Linear                 (V2(V2))

import qualified Data.Vector as V (singleton, fromList, toList)
import qualified Data.Map    as M (insert, (!))


import Dreamnet.CoordVector
import Dreamnet.TileMap

--------------------------------------------------------------------------------

generateMap ∷ (MonadIO m, MonadRandom m) ⇒ Width → Height → m TileMap
generateMap w h = do
    maps  ← loadPredefined
    flip execStateT (newTileMap w h base) $ do
        prepareTileset
        placeSpawns 
        stampBuilding 2 2 (head maps)
    where
        base = Tile '.' (V.fromList [ "Spawn", "True", "True" ])


prepareTileset ∷ (MonadRandom s, MonadState TileMap s) ⇒ s ()
prepareTileset = m_tileset %= M.insert '╳' (Tile '╳' (V.singleton "Spawn"))


placeSpawns ∷ (MonadRandom s, MonadState TileMap s) ⇒ s ()
placeSpawns = do
    spawn ← uses m_tileset (M.! '╳')
    m_layers %= fmap ( changeTile (V2 1 1) spawn
                     . changeTile (V2 0 1) spawn
                     . changeTile (V2 1 0) spawn
                     . changeTile (V2 0 0) spawn
                     )


stampBuilding ∷ (MonadState TileMap s) ⇒ Word → Word → TileMap → s ()
stampBuilding x y tm = do
    let firstLayer = views m_layers (fromMaybe (error "Loaded map has no layers, so it can't be stamped!") . headMay . V.toList) tm
    m_layers %= fmap (copyData 0 0 (width tm) (height tm) firstLayer x y)


copyData ∷ Word → Word → Width → Height → TileLayer → Word → Word → TileLayer → TileLayer
--copyData sx sy sw sh sl dx dy dl = dl
copyData _ _ _ _ _ _ _ dl = dl



-- TODO add 'isValid' that checks that all necessary files are present for
-- a filename in list
loadPredefined ∷ (MonadIO m) ⇒ m [TileMap]
loadPredefined = traverse (loadTileMap . ("res/" <>)) maps
    where
        maps = [ "apartment0"
               , "bar"
               , "job"
               ]
    