{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.Engine.TileMap
( module Dreamnet.Engine.CoordVector

, Tile(Tile)
, t_char
, t_data

, ttype
, readBoolProperty
, readWordProperty
, readStringProperty

, TileLayer
, l_data
, newTileLayer

, Tileset
, TileMap
, m_layers
, m_tileset
, m_positioned
, m_desc
, newTileMap
, tileAt
, changeTile
, findAll

, loadTileMap
) where

import Safe

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens           (makeLenses, view, (^.), (.~), element)
import Data.Semigroup         ((<>))
import Data.Maybe             (fromMaybe)
import Data.List              (elemIndex)
import Data.Bool              (bool)
import Linear                 (V2(V2))
import System.Directory       (doesFileExist)

import qualified Data.Map             as M
import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv             as CSV

import Dreamnet.Engine.CoordVector

--------------------------------------------------------------------------------

data Tile = Tile {
      _t_char ∷ Char
    , _t_data ∷ V.Vector String
    }
    deriving(Show)
makeLenses ''Tile


ttype ∷ Tile → String
ttype = fromMaybe (error "Tile type not set!") . (V.!? 0) . view t_data
{-# INLINE ttype #-}


readBoolProperty ∷ Int → Tile → Bool
readBoolProperty i = maybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") (readNote "Failed to read Bool property ") . (V.!? i) . view t_data


readWordProperty ∷ Int → Tile → Word
readWordProperty i = maybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") (readNote "Failed to read Word property ") . (V.!? i) . view t_data


readStringProperty ∷ Int → Tile → String
readStringProperty i = fromMaybe (error $ "Tile property ix:" <> show i <> " doesn't exist!") . (V.!? i) . view t_data

--------------------------------------------------------------------------------

data TileLayer = TileLayer {
      _l_width      ∷ Word
    , _l_height     ∷ Word
    , _l_data       ∷ V.Vector Char
    }
    deriving(Show)

makeLenses ''TileLayer

instance CoordVector TileLayer where
    width  = view l_width
    height = view l_height


newTileLayer ∷ Width → Height → Char → TileLayer
newTileLayer w h c = TileLayer w h (V.replicate (fromIntegral $ w * h) c)


tileAt ∷ TileLayer → Tileset → V2 Int → Tile
tileAt tl ts v = let char      = (tl^.l_data) V.! linCoord tl v
                     maybeTile = char `M.lookup` ts
                     err       = error $ "Couldn't find a Tile instance in the Tileset for the layer: " <> [char]
                 in  fromMaybe err maybeTile


changeTile ∷ V2 Int → Tile → TileLayer → TileLayer
changeTile v t tl = l_data . element (linCoord tl v) .~ (t^.t_char) $ tl


findAll ∷ Char → TileLayer → [V2 Int]
findAll ch tl = let foldCoord i c l = bool l (coordLin tl i : l) (c == ch)
                in  V.ifoldr foldCoord [] (tl^.l_data)

--------------------------------------------------------------------------------

type Tileset = M.Map Char Tile

data TileMap = TileMap {
      _m_width      ∷ Width
    , _m_height     ∷ Height
    , _m_layers     ∷ V.Vector TileLayer
    , _m_tileset    ∷ Tileset
    , _m_positioned ∷ M.Map (V2 Int) [Tile]
    , _m_desc       ∷ String
    }
    deriving(Show)

makeLenses ''TileMap

instance CoordVector TileMap where
    width  = view m_width
    height = view m_height


newTileMap ∷ Width → Height → Tile → TileMap
newTileMap w h base =
    TileMap w h
        (V.singleton (newTileLayer w h (view t_char base)))
        (M.singleton (view t_char base) base)
        M.empty
        ""

--------------------------------------------------------------------------------

makeFilename ∷ FilePath → String → FilePath
makeFilename fp s = fp <> "." <> s


makeFilename' ∷ FilePath → String → Word → FilePath
makeFilename' fp s i = fp <> "." <> s <> show i


loadTileMap ∷ (MonadIO m) ⇒ FilePath → m TileMap
loadTileMap fp = do
    ts     ← readTileset fp
    lc     ← layerCount fp
    layers ← traverse (readLayer fp) [0..lc - 1]
    desc   ← liftIO $ readFile (makeFilename fp "desc")
    pos    ← readPositioned (makeFilename fp "positioned")

    let w = (layers !! 0) ^.l_width
        h = (layers !! 0) ^.l_height
    return $ TileMap
        (fromIntegral w)
        (fromIntegral h)
        (V.fromList layers)
        ts
        pos
        desc


readTileset ∷ (MonadIO m) ⇒ FilePath → m Tileset
readTileset fp = liftIO $ either err makeMap . CSV.decode CSV.NoHeader <$>
                            BS.readFile (makeFilename fp "set")

    where
        err e   = error $ "Can't parse " <> makeFilename fp "set" <> ": " <> e
        makeMap = V.foldr' insertTile M.empty
        insertTile v  = let t = createTile v
                        in  M.insert (t^.t_char) t
        createTile v  = let char  = head . V.head $ v
                            edata = V.drop 1 v
                        in  Tile char edata


layerCount ∷ (MonadIO m) ⇒ FilePath → m Word
layerCount fp = fromIntegral <$> countLayers 0
    where
        countLayers ∷ (MonadIO m) ⇒ Int → m Int
        countLayers acc = let lfp = makeFilename' fp "layer" (fromIntegral acc)
                          in  liftIO (doesFileExist lfp) >>=
                                bool (pure acc)
                                     (countLayers (acc + 1))


readLayer ∷ (MonadIO m) ⇒ FilePath → Word → m TileLayer
readLayer fp i = do
    mapStr   ← liftIO $ readFile (makeFilename' fp "layer" i)
    let w     = findWidth mapStr
        h     = findHeight mapStr
        ldata = cleanNewlines mapStr
    return $ TileLayer w h ldata
    where
        findWidth     = fromIntegral . fromMaybe 0 . elemIndex '\n'
        findHeight    = fromIntegral . length . filter (=='\n')
        cleanNewlines = V.fromList . filter (/='\n')


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

