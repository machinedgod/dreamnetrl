{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.TileMap
( Tile
, t_char
, t_data

, TileLayer
, l_width
, l_height
, l_data
, l_tileset

, TileMap
, m_width
, m_height
, m_layers
, m_positioned
, m_desc
, tileAt
, changeTile
, findAll

, loadTileMap
) where

import Safe

import Control.Monad.IO.Class 
import Control.Lens 
import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Bool (bool)
import Linear 

import qualified Data.Map    as M
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv             as CSV

import Dreamnet.CoordVector

--------------------------------------------------------------------------------

data Tile = Tile {
      _t_char ∷ Char
    , _t_data ∷ V.Vector String
    }
    deriving(Show)
            
makeLenses ''Tile

--------------------------------------------------------------------------------

data TileLayer = TileLayer {
      _l_width      ∷ Word
    , _l_height     ∷ Word
    , _l_data       ∷ V.Vector Char
    , _l_tileset    ∷ M.Map Char Tile
    }
    deriving(Show)

makeLenses ''TileLayer

instance CoordVector TileLayer where
    width = view l_width


tileAt ∷ TileLayer → V2 Int → Tile
tileAt tl v = let char      = (tl^.l_data) V.! linCoord tl v
                  maybeTile = char `M.lookup` (tl^.l_tileset)
                  err       = error $ "Couldn't find a Tile instance in the Tileset for the layer: " <> [char]
              in  fromMaybe err maybeTile


changeTile ∷ V2 Int → Tile → TileLayer → TileLayer
changeTile v t tl = l_data . element (linCoord tl v) .~ (t^.t_char) $ tl


findAll ∷ Char → TileLayer → [V2 Int]
findAll ch tl = let foldCoord i c l = bool l (coordLin tl i : l) (c == ch)
                in  V.ifoldr foldCoord [] (tl^.l_data)

--------------------------------------------------------------------------------

type Width  = Word
type Height = Word


data TileMap = TileMap {
      _m_width      ∷ Width
    , _m_height     ∷ Height
    , _m_layers     ∷ V.Vector TileLayer
    , _m_positioned ∷ M.Map (V2 Int) [Tile]
    , _m_desc       ∷ String
    }
    deriving(Show)

makeLenses ''TileMap

instance CoordVector TileMap where
    width = view m_width

--------------------------------------------------------------------------------

makeFilename ∷ FilePath → String → FilePath
makeFilename fp s = fp <> "." <> s


makeFilename' ∷ FilePath → String → Word → FilePath
makeFilename' fp s i = fp <> "." <> s <> show i


loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    layer0 ← readLayer fp 0
    desc   ← liftIO $ readFile (makeFilename fp "desc")
    pos    ← readPositioned (makeFilename fp "positioned")

    let w = layer0^.l_width
        h = layer0^.l_height
    return $ TileMap
        (fromIntegral w)
        (fromIntegral h)
        (V.fromList [ layer0 ])
        pos
        desc


readLayer ∷ (MonadIO m) ⇒ FilePath → Word → m TileLayer
readLayer fp i = do
    mapStr   ← liftIO $ readFile (makeFilename' fp "layer" i)
    tableStr ← liftIO $ BS.readFile (makeFilename' fp "set" i)

    let w     = findWidth mapStr
        h     = findHeight mapStr
        ldata = cleanNewlines mapStr
        tset  = createTable tableStr
    return $ TileLayer w h ldata tset
    where
        findWidth     = fromIntegral . fromMaybe 0 . elemIndex '\n'
        findHeight    = fromIntegral . length . filter (=='\n')
        cleanNewlines = V.fromList . filter (/='\n')
        createTable   = either err makeMap . CSV.decode CSV.NoHeader
        err e         = error $ "Can't parse " <> makeFilename' fp "set" i <> ": " <> e
        makeMap       = V.foldr' insertTile M.empty
        insertTile v  = let t = createTile v
                        in  M.insert (t^.t_char) t
        createTile v  = let char  = head . V.head $ v
                            edata = V.drop 1 v
                        in  Tile char edata


readPositioned ∷ (MonadIO m) ⇒ FilePath → m (M.Map (V2 Int) [Tile])
readPositioned fp = fmap makeTable . liftIO . BS.readFile $ fp
    where
        makeTable    = either decodeErr vectorsToMap . CSV.decode CSV.NoHeader
        decodeErr e  = error $ "CSV decoding error in '" <> fp <> "': " <> e
        vectorsToMap = V.foldr insertTile M.empty
        insertTile v = M.insert (pos v) [tile v]
        pos v        = let err = "Invalid coord when specifying positioned tile in '" <> fp <> "'"
                           x   = readNote err (V.head v)
                           y   = readNote err (V.head $ V.drop 1 v)
                       in  V2 x y
        tile v = Tile (charForType $ V.head $ V.drop 2 v) (V.drop 2 v)
        charForType "Person" = '@'
        charForType "Item"   = '['
        charForType _        = '?'

