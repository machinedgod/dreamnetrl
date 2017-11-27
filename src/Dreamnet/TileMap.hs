{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.TileMap
( Tile(..)
, TileMap
, m_width
, m_height
, m_data
, m_desc
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
             , ('╬', OuterWall)

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
             ]


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

    , _m_spawnPoints ∷ [V2 Int]
    , _m_extra  ∷ Map.Map (V2 Int) (String, String, String, String, Char) -- Should support multiple objects at same location
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
    where
        tileFor c = fromMaybe (err c) $ c `lookup` asciiTable
        err c = error $ "Shouldn't happen! Ran into a tiletype that was not cleaned up during loading! :-O: " ++ [c]




--------------------------------------------------------------------------------

loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    (w, h, mdata, cdata) ← readTilemap fp
    props                ← gatherProps w mdata <$> readTranslationTable fp
    desc                 ← readDesc  fp
    extra                ← readExtra fp

    return $ TileMap
        (fromIntegral w)
        (fromIntegral h)
        cdata
        desc
        (findSpawnPoints w mdata)
        (props `Map.union` extra)


readTilemap ∷ (MonadIO m) ⇒ FilePath → m (Int, Int, Vec.Vector Char, Vec.Vector Char)
readTilemap fp = do
    str ← liftIO $ readFile (fp ++ ".tilemap")
    let w     = fromMaybe 0 $ elemIndex '\n' str
        h     = length $ filter (=='\n') str
        mdata = Vec.fromList $ filter (/='\n') str
        cdata = Vec.map turnTile mdata
    return $ (w, h, mdata, cdata)
    where
        turnTile c@((`lookup` asciiTable) → Just _) = c
        turnTile _                                  = '.'


readTranslationTable ∷ (MonadIO m) ⇒ FilePath → m (Map.Map Char (String, String,String,String, Char))
readTranslationTable fp = let f (c, t, n, p, s) = Map.insert c (t, n, p, s, c)
                          in  loadCsvToTupleMap fp ".trans" f


gatherProps ∷ Int → Vec.Vector Char → Map.Map Char (String,String,String,String,Char) → Map.Map (V2 Int) (String, String,String,String,Char)
gatherProps w mdata trtab = let appendIfObject i c m = maybe m (gatherObject i m) $ Map.lookup c trtab
                                gatherObject i m o   = Map.insert (coordLin' w i) o m
                            in  Vec.ifoldr appendIfObject Map.empty mdata


readDesc ∷ (MonadIO m) ⇒ FilePath → m String
readDesc fp = cleanUpNewlines <$> liftIO $ readFile (fp ++ ".desc")
    where
        cleanUpNewlines s = s


readExtra ∷ (MonadIO m) ⇒ FilePath → m (Map.Map (V2 Int) (String, String, String, String, Char))
readExtra fp = let f (x, y, t, n) = Map.insert (V2 x y) (t, n, "", "", ' ')
               in  loadCsvToTupleMap fp ".extra" f


loadCsvToTupleMap ∷ (MonadIO m, CSV.FromRecord c) ⇒ FilePath → String → (c → Map.Map a b → Map.Map a b) → m (Map.Map a b)
loadCsvToTupleMap fp ext toDataTuple = do
    errOrVec ← liftIO $ CSV.decode CSV.NoHeader <$> BS.readFile (fp ++ ext)
    case errOrVec of
        Left e  → error $ "Error loading '"++fp++"."++ext++"': " ++ e
        Right v → return $ Vec.foldr toDataTuple Map.empty v


changeTile ∷ V2 Int → Tile → TileMap → TileMap
changeTile v o m = (m_data . element (linCoord m v) .~ tileChar o) m


findSpawnPoints ∷ Int → Vec.Vector Char → [V2 Int]
findSpawnPoints w mdata =
    let foldSpawnCoords i c l = bool l (coordLin' w i : l) (c == '╳')
    in  Vec.ifoldr foldSpawnCoords [] mdata

