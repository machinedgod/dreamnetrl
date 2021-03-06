{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Dreamnet.Engine.TileMap
( module Dreamnet.Engine.CoordVector

, Tile(Tile), tChar, tData

, ttype, readBoolProperty, readWordProperty, readIntProperty
, readStringProperty

, TileLayer, lSize, lData, newTileLayer, codeAt, tileAt, changeTile, findAll
, loadLayer

, Tileset, TileMap, mSize, mLayers, mTileset, mPositioned, mDesc
, newTileMap, loadTileMap
)
where


import Safe

import Control.Lens           (makeLenses, view, (^.), (.~), element, _1, _2)
import Control.Monad          ((<=<))
import Data.Maybe             (fromMaybe)
import Data.List              (elemIndex)
import Data.List.NonEmpty     (NonEmpty(..))
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
      _tChar ∷ Char
    , _tData ∷ V.Vector String
    }
    deriving(Show)
makeLenses ''Tile


ttype ∷ Tile → String
ttype = fromMaybe (error "Tile type not set!") . (V.!? 0) . view tData
{-# INLINE ttype #-}


readBoolProperty ∷ Int → Tile → Maybe Bool
readBoolProperty i = readMay <=< (V.!? i) . view tData
{-# INLINE readBoolProperty #-}


readWordProperty ∷ Int → Tile → Maybe Word
readWordProperty i = readMay <=< (V.!? i) . view tData
{-# INLINE readWordProperty #-}


readIntProperty ∷ Int → Tile → Maybe Int
readIntProperty i = readMay <=< (V.!? i) . view tData
{-# INLINE readIntProperty #-}


readStringProperty ∷ Int → Tile → Maybe String
readStringProperty i = (V.!? i) . view tData
{-# INLINE readStringProperty #-}

--------------------------------------------------------------------------------

data TileLayer = TileLayer {
      _lSize ∷ (Width, Height)
    , _lData ∷ V.Vector Char
    }
    deriving(Show)
makeLenses ''TileLayer


instance CoordVector TileLayer where
    width  = view (lSize._1)
    height = view (lSize._2)


newTileLayer ∷ Width → Height → Char → TileLayer
newTileLayer w h c = TileLayer (w, h) (V.replicate (fromIntegral $ squared w h) c)


codeAt ∷ TileLayer → V2 Int → Char
codeAt tl v = (tl^.lData) V.! linCoord tl v


tileAt ∷ TileLayer → Tileset → V2 Int → Tile
tileAt tl ts v =
    let char      = codeAt tl v
        maybeTile = char `M.lookup` ts
        err       = error $ "Couldn't find a Tile instance in the Tileset \
                            \for the layer: " <> [char]
    in  fromMaybe err maybeTile


changeTile ∷ V2 Int → Tile → TileLayer → TileLayer
changeTile v t tl = lData . element (linCoord tl v) .~ (t^.tChar) $ tl


findAll ∷ Char → TileLayer → [V2 Int]
findAll ch tl = let foldCoord i c l = bool l (coordLin tl i : l) (c == ch)
                in  V.ifoldr foldCoord [] (tl^.lData)


loadLayer ∷ FilePath → IO TileLayer
loadLayer fp = do
    mapStr ← readFile fp
    let w     = findWidth mapStr
        h     = findHeight mapStr
        ldata = cleanNewlines mapStr
    pure $ TileLayer (w, h) ldata
    where
        findWidth     = fromIntegral . fromMaybe 0 . elemIndex '\n'
        findHeight    = fromIntegral . length . filter (=='\n')
        cleanNewlines = V.fromList . filter (/='\n')

--------------------------------------------------------------------------------

type Tileset = M.Map Char Tile

--------------------------------------------------------------------------------

data TileMap = TileMap {
      _mSize       ∷ (Width, Height)
    , _mLayers     ∷ NonEmpty TileLayer
    , _mTileset    ∷ Tileset
    , _mPositioned ∷ M.Map (V2 Int) [Tile]
    , _mDesc       ∷ String
    }
    deriving(Show)
makeLenses ''TileMap


instance CoordVector TileMap where
    width  = view (mSize._1)
    height = view (mSize._2)


newTileMap ∷ Width → Height → Tile → TileMap
newTileMap w h base =
    TileMap (w, h)
        (newTileLayer w h (view tChar base) :| [])
        (M.singleton (view tChar base) base)
        M.empty
        ""

--------------------------------------------------------------------------------

makeFilename ∷ FilePath → String → FilePath
makeFilename fp s = fp <> "." <> s


makeFilename' ∷ FilePath → String → Word → FilePath
makeFilename' fp s i = fp <> "." <> s <> show i


loadTileMap ∷ FilePath → IO (Either String TileMap)
loadTileMap fp = layerCount fp >>= \lc →
    if lc < 1
        then Left  <$> pure "No layers defined; need at least a base layer."
        else Right <$> continueLoading lc
    where
        continueLoading lc = do
            ts     ← readTileset fp
            base   ← readLayer fp 0
            layers ← traverse (readLayer fp) [1..lc - 1]
            desc   ← readFile (makeFilename fp "desc")
            pos    ← readPositioned (makeFilename fp "positioned")

            pure $ TileMap
                (head layers ^. lSize)
                (base :| layers)
                ts
                pos
                desc


readTileset ∷ FilePath → IO Tileset
readTileset fp = either err makeMap . CSV.decode CSV.NoHeader <$>
                    BS.readFile (makeFilename fp "set")
    where
        err e   = error $ "Can't parse " <> makeFilename fp "set" <> ": " <> e
        makeMap = V.foldr' insertTile M.empty
        insertTile v  = let t = createTile v
                        in  M.insert (t^.tChar) t
        createTile v  = let char  = head . V.head $ v
                            edata = V.drop 1 v
                        in  Tile char edata


layerCount ∷ FilePath → IO Word
layerCount fp = fromIntegral <$> countLayers 0
    where
        countLayers ∷ Int → IO Int
        countLayers acc = let lfp = makeFilename' fp "layer" (fromIntegral acc)
                          in  doesFileExist lfp >>=
                                bool (pure acc)
                                     (countLayers (acc + 1))


readLayer ∷ FilePath → Word → IO TileLayer
readLayer fp i = loadLayer (makeFilename' fp "layer" i)


readPositioned ∷ FilePath → IO (M.Map (V2 Int) [Tile])
readPositioned fp = fmap makeTable . BS.readFile $ fp
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

