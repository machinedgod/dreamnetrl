{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.MapGenerator
where


import Safe                   (headMay)
import Control.Lens           ((%=), (%~), (.=), uses, view, views)
import Control.Monad.Random   (MonadRandom)
import Control.Monad.State    (MonadState, execStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Semigroup         ((<>))
import Data.Maybe             (fromMaybe)
import Linear                 (V2(V2))

import qualified Data.Vector as V (singleton, fromList, toList, imap, (!?))
import qualified Data.Map    as M (insert, (!), unions)


import Dreamnet.CoordVector
import Dreamnet.TileMap

--------------------------------------------------------------------------------

generateMap ∷ (MonadIO m, MonadRandom m) ⇒ Width → Height → m TileMap
generateMap w h = do
    maps ← loadPredefined
    flip execStateT (newTileMap w h base) $ do
        prepareTileset (view m_tileset <$> maps)
        placeSpawns 
        stampBuilding 4 4 (head maps)
    where
        base = Tile '.' (V.fromList [ "Spawn", "True", "True" ])


prepareTileset ∷ (MonadRandom s, MonadState TileMap s) ⇒ [Tileset] → s ()
prepareTileset ts = m_tileset .= M.unions ts
--prepareTileset = m_tileset %= M.insert '╳' (Tile '╳' (V.singleton "Spawn"))


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
    m_layers %= fmap (copyData (width tm) (height tm) firstLayer x y)


copyData ∷ Width → Height → TileLayer → Word → Word → TileLayer → TileLayer
copyData (fromIntegral → sw) (fromIntegral → sh) sl (fromIntegral → dx) (fromIntegral → dy) dl = l_data %~ V.imap (\i c → replaceTile (coordLin dl i) c) $ dl
    where
        replaceTile ∷ V2 Int → Char → Char
        replaceTile (V2 x y) c = let x' = x - dx
                                     y' = y - dy
                                 in  if x' < 0 || x' >= sw || y' < 0 || y' >= sh
                                         then c
                                         --then views dl.l_data (`at` (dx + x'))
                                         else views l_data (\v → fromMaybe (error "Unreasonable coordinate!") $ v V.!? (linCoord sl (V2 x' y'))) sl



-- TODO add 'isValid' that checks that all necessary files are present for
-- a filename in list
loadPredefined ∷ (MonadIO m) ⇒ m [TileMap]
loadPredefined = traverse (loadTileMap . ("res/" <>)) maps
    where
        maps = [ "apartment0"
               , "bar"
               , "job"
               ]
    
