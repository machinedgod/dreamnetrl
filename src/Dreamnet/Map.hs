{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.Map
( Map
, m_width
, m_height
, m_data

, linCoord
, coordLin
, clipOutOfBounds
, tileAt

, Tile(..)
, asciiTable
, loadMap
) where

import Control.Monad.IO.Class 
import Control.Lens 
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Linear 

import qualified Data.Vector as Vec

--------------------------------------------------------------------------------

data Map = Map {
      _m_width ∷ Word
    , _m_height ∷ Word
    , _m_data ∷ Vec.Vector Char
    }

makeLenses ''Map


linCoord ∷ V2 Int → Map → Int
linCoord (V2 x y) m = let w = fromIntegral (m^.m_width)
                      in  y * w + x
{-# INLINE linCoord #-}


coordLin ∷ Int → Map → V2 Int
coordLin i m = let w = m^.m_width
                   x = i `mod` (fromIntegral w)
                   y = i `div` (fromIntegral w)
               in  V2 x y 
{-# INLINE coordLin #-}


clipOutOfBounds ∷ [V2 Int] → [V2 Int]
clipOutOfBounds = filter (\(V2 x y) → x >= 0 && y >= 0)

-- TODO so fucking shaky :-D
--      replace with making objects during loading
tileAt ∷ Map → V2 Int → Tile
tileAt m v = fromMaybe Floor $ (`lookup` asciiTable) $ (m^.m_data) Vec.! linCoord v m


--------------------------------------------------------------------------------

data Tile = OuterWall
          | InnerWall
          | Floor
          | Spawn
          | Table
          | Chair
          | OpenedDoor
          | ClosedDoor
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
             , ('╳', Spawn)
             , ('#', Table)
             , ('%', Chair)
             , ('\'', OpenedDoor)
             , ('+', ClosedDoor)
             ]


loadMap ∷ (MonadIO m) ⇒ FilePath → m Map
loadMap fp = do
    str ← liftIO $ readFile fp
    let w = fromMaybe 0 $ elemIndex '\n' str
        h = length str - w
    return $ Map (fromIntegral w) (fromIntegral h) (Vec.fromList $ filter (/='\n') str)


