{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.TileMap
( TileMap
, m_width
, m_height
, m_data

, linCoord
, coordLin
, clipOutOfBounds
, tileAt

, Tile(..)
, asciiTable
, loadTileMap
) where

import Control.Monad.IO.Class 
import Control.Lens 
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Linear 

import qualified Data.Vector as Vec

--------------------------------------------------------------------------------

data TileMap = TileMap {
      _m_width ∷ Word
    , _m_height ∷ Word
    , _m_data ∷ Vec.Vector Char
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


clipOutOfBounds ∷ [V2 Int] → [V2 Int]
clipOutOfBounds = filter (\(V2 x y) → x >= 0 && y >= 0)


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
          deriving (Eq, Show)


asciiTable = [ ('═', OuterWall)
             , ('║', OuterWall)
             , ('╔', OuterWall)
             , ('╚', OuterWall)
             , ('╗', OuterWall)
             , ('╝', OuterWall)
             , ('─', InnerWall)
             , ('│', InnerWall)
             , ('┌', InnerWall)
             , ('└', InnerWall)
             , ('┐', InnerWall)
             , ('┘', InnerWall)
             , ('.', Floor)
             , ('╳', MapSpawn)

             , ('#', Table)
             , ('%', Chair)
             , ('\'', OpenedDoor)
             , ('+', ClosedDoor)
             , ('◈', Computer)
             , ('@', Person)
             ]


loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    str ← liftIO $ readFile fp
    let w = fromMaybe 0 $ elemIndex '\n' str
        h = length str - w
    return $ TileMap (fromIntegral w) (fromIntegral h) (Vec.fromList $ filter (/='\n') str)


