{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.TileMap
( Tile(..)
, tileFor
, tileChar

, TileMap
, m_width
, m_height
, m_data
, m_desc
, m_objects
, m_spawnPoints
, m_extra
, linCoord
, linCoord'
, coordLin
, coordLin'
, outOfBounds
, tileAt


, loadTileMap
) where

import Control.Monad.IO.Class 
import Control.Lens 
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, intercalate)
import Data.Bool (bool)
import Linear 

import qualified Data.Map    as Map
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv             as CSV

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
          deriving (Eq, Show, Ord)

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
             , ('#', Chair)
             , ('▮', Table)
             , ('O', Table)
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


tileFor ∷ Char → Tile
tileFor c = fromMaybe err $ c `lookup` asciiTable
    where err = error $ "Unknown tile: " ++ show c


tileChar ∷ Tile → Char
tileChar t = fromMaybe err $ t `lookup` (flipTuple <$> asciiTable)
    where err              = error $ "Tile w/o char! " ++ show t
          flipTuple (x, y) = (y, x)

--------------------------------------------------------------------------------

data TileMap = TileMap {
      _m_width  ∷ Word
    , _m_height ∷ Word
    , _m_data   ∷ Vec.Vector Char
    , _m_desc   ∷ String

    , _m_objects     ∷ Map.Map (V2 Int) Tile
    , _m_spawnPoints ∷ [V2 Int]
    , _m_extra  ∷ Map.Map (V2 Int) (Vec.Vector String)
    }

makeLenses ''TileMap

linCoord' ∷ Int → V2 Int → Int
linCoord' w (V2 x y) = y * w + x
{-# INLINE linCoord' #-}


linCoord ∷ TileMap → V2 Int → Int
linCoord m = linCoord' (fromIntegral $ m^.m_width)
{-# INLINE linCoord #-}


coordLin' ∷ Int → Int → V2 Int
coordLin' w i = let x = i `mod` w
                    y = i `div` w
                in  V2 x y 
{-# INLINE coordLin' #-}


coordLin ∷ TileMap → Int → V2 Int
coordLin m i = coordLin' (fromIntegral $ m^.m_width) i
{-# INLINE coordLin #-}


-- TODO convert to V2 Word
outOfBounds ∷ TileMap → V2 Int → Bool
outOfBounds m (V2 x y) = let w = fromIntegral $ m ^. m_width
                             h = fromIntegral $ m ^. m_height
                         in  x < 0 || y < 0 || x >= w || y >= h


tileAt ∷ TileMap → V2 Int → Tile
tileAt m v = tileFor $ (m^.m_data) Vec.! linCoord m v


--------------------------------------------------------------------------------

loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    str ← liftIO $ readFile (fp ++ ".tilemap")
    let w         = fromMaybe 0 $ elemIndex '\n' str
        h         = length $ filter (=='\n') str
        mdata     = Vec.fromList $ filter (/='\n') str
        cleanData = Vec.map (\c → let t = tileFor c
                                  in  if t /= InnerWall && t /= OuterWall && t /= Floor
                                          then '.'
                                          else c) mdata
    desc  ← readDesc  fp
    extra ← readExtra fp
    return $ TileMap
        (fromIntegral w)
        (fromIntegral h)
        cleanData
        desc
        (findObjects w extra mdata)
        (findSpawnPoints w mdata)
        extra


readDesc ∷ (MonadIO m) ⇒ FilePath → m String
readDesc fp = cleanUpNewlines <$> liftIO $ readFile (fp ++ ".desc")
    where
        cleanUpNewlines s = s


readExtra ∷ (MonadIO m) ⇒ FilePath → m (Map.Map (V2 Int) (Vec.Vector String))
readExtra fp = do
    errOrVec ← liftIO $ CSV.decode CSV.NoHeader <$> BS.readFile (fp ++ ".extra")
    case errOrVec of
        Left e  → error $ "Error loading extras: " ++ e
        Right v → let fillDataVectors (x, y, t, d0, d1, d2) = Map.insert (V2 x y) (Vec.fromList [t, d0, d1, d2])
                  in  return $ Vec.foldr fillDataVectors Map.empty v


changeTile ∷ V2 Int → Tile → TileMap → TileMap
changeTile v o m = (m_data . element (linCoord m v) .~ tileChar o) m


findObjects ∷ Int → Map.Map (V2 Int) (Vec.Vector String) → Vec.Vector Char → Map.Map (V2 Int) Tile
findObjects w mextra mdata =
    let isObject c = let t = tileFor c
                     in  t /= OuterWall && t /= InnerWall && t /= Floor && t /= MapSpawn
        findObjects i c os   = let v = coordLin' w i
                               in  bool os (Map.insert v (tileFor c) os) (isObject c)
    in  Vec.ifoldr findObjects Map.empty mdata


findSpawnPoints ∷ Int → Vec.Vector Char → [V2 Int]
findSpawnPoints w mdata =
    let foldSpawnCoords i c l = bool l (coordLin' w i : l) (tileFor c == MapSpawn)
    in  Vec.ifoldr foldSpawnCoords [] mdata

