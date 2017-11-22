{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.TileMap
( TileMap
, m_width
, m_height
, m_data

, linCoord
, coordLin
, outOfBounds
, tileAt
, changeTile
, findObjects
, findSpawnPoints

, Tile(..)
, asciiTable
, loadTileMap
) where

import Control.Monad.IO.Class 
import Control.Lens 
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Bool (bool)
import Linear 

import qualified Data.Map    as Map
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv             as CSV

--------------------------------------------------------------------------------

data TileMap = TileMap {
      _m_width ∷ Word
    , _m_height ∷ Word
    , _m_data ∷ Vec.Vector Char
    , _m_extra ∷ Map.Map (V2 Int) (Vec.Vector String)
    }

makeLenses ''TileMap


linCoord ∷ TileMap → V2 Int → Int
linCoord m (V2 x y) = let w = fromIntegral (m^.m_width)
                      in  y * w + x
{-# INLINE linCoord #-}


coordLin ∷ TileMap → Int → V2 Int
coordLin m i = let w = m^.m_width
                   x = i `mod` (fromIntegral w)
                   y = i `div` (fromIntegral w)
               in  V2 x y 
{-# INLINE coordLin #-}


-- TODO convert to V2 Word
outOfBounds ∷ TileMap → V2 Int → Bool
outOfBounds m (V2 x y) = let w = fromIntegral $ m ^. m_width
                             h = fromIntegral $ m ^. m_height
                         in  x < 0 || y < 0 || x >= w || y >= h


tileAt ∷ TileMap → V2 Int → Tile
tileAt m v = fromMaybe Floor $ (`lookup` asciiTable) $ (m^.m_data) Vec.! linCoord m v


--------------------------------------------------------------------------------

data Tile = OuterWall
          | InnerWall
          | Floor
          | MapSpawn

          | Table
          | Chair
          | OpenedDoor
          | ClosedDoor
          | Computer
          | Person

          | Cupboard
          | Sink
          | Toilet

          | StairsDown
          | StairsUp
          deriving (Eq, Show)


asciiTable = [ ('═', OuterWall)
             , ('║', OuterWall)
             , ('╔', OuterWall)
             , ('╚', OuterWall)
             , ('╗', OuterWall)
             , ('╝', OuterWall)
             , ('╦', OuterWall)
             , ('╩', OuterWall)
             , ('╠', OuterWall)
             , ('╣', OuterWall)
             , ('╟', OuterWall)
             , ('╢', OuterWall)
             , ('╤', OuterWall)
             , ('╧', OuterWall)

             , ('─', InnerWall)
             , ('│', InnerWall)
             , ('┌', InnerWall)
             , ('└', InnerWall)
             , ('┐', InnerWall)
             , ('┘', InnerWall)
             , ('┤', InnerWall)
             , ('├', InnerWall)
             , ('┴', InnerWall)
             , ('┬', InnerWall)
             , ('╡', InnerWall)
             , ('╞', InnerWall)
             , ('╥', InnerWall)
             , ('╨', InnerWall)

             , ('.', Floor)

             , ('╳', MapSpawn)
             , ('#', Table)
             , ('%', Chair)
             , ('\'', OpenedDoor)
             , ('+', ClosedDoor)
             , ('◈', Computer)
             , ('@', Person)

             , ('c', Cupboard)
             , ('s', Sink)
             , ('t', Toilet)

             , ('<', StairsDown)
             , ('>', StairsUp)
             ]


loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    str ← liftIO $ readFile (fp ++ ".tilemap")
    let w     = fromMaybe 0 $ elemIndex '\n' str
        h     = length $ filter (=='\n') str
    extra ← readExtra fp
    return $ TileMap (fromIntegral w) (fromIntegral h) (Vec.fromList $ filter (/='\n') str) extra


readExtra ∷ (MonadIO m) ⇒ FilePath → m (Map.Map (V2 Int) (Vec.Vector String))
readExtra fp = do
    errOrVec ← liftIO $ CSV.decode CSV.HasHeader <$> BS.readFile (fp ++ ".extra")
    case errOrVec of
        Left e  → error $ "Error loading extras: " ++ e
        Right v → let fillDataVectors (x, y, t, d0, d1, d2) m = Map.insert (V2 x y) (Vec.fromList [t, d0, d1, d2])  m
                  in  return $ Vec.foldr fillDataVectors Map.empty v


changeTile ∷ V2 Int → Tile → TileMap → TileMap
changeTile v o m = let c = maybe '.' id $ lookup o $ flipTuple <$> asciiTable
                   in  (m_data . element (linCoord m v) .~ c) m
    where
        flipTuple (x, y) = (y, x)


findObjects ∷ TileMap → (Vec.Vector String → V2 Int → Tile → Maybe a) → Map.Map (V2 Int) a
findObjects m tileToObject =
    let maybeObject v c      = c `lookup` asciiTable >>= tileToObject (fromMaybe Vec.empty $ Map.lookup v $ m ^. m_extra) v 
        insertIntoMap v os o = Map.insert v o os
        findObjects i c os   = let v = coordLin m i
                               in  maybe os (insertIntoMap v os) (maybeObject v c)
    in  Vec.ifoldr findObjects Map.empty (m ^. m_data)


findSpawnPoints ∷ TileMap → [V2 Int]
findSpawnPoints m =
    let isSpawnTile c         = fromMaybe False $ fmap (==MapSpawn) $ c `lookup` asciiTable
        foldSpawnCoords i c l = bool l (coordLin m i : l) (isSpawnTile c)
    in  Vec.ifoldr foldSpawnCoords [] (m ^. m_data)

