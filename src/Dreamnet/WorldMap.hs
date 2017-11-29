{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.WorldMap
( Object(..)
, isPassable
, isSeeThrough
, objectDescription

, Visibility(..)

, WorldMap
, wm_width
, wm_height
, wm_data
, wm_visible
, wm_desc
, wm_spawns

, fromTileMap
, outOfBounds
, objectAt
, interestingObjects
) where

import Control.Lens
import Data.Bool (bool)
import Linear

import qualified Data.Vector as V
import qualified Data.Map    as M


import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap
import Dreamnet.TileData

--------------------------------------------------------------------------------

data Object = Wall
            | Floor
            | Door       Bool       -- <-- Is opened?
            | Stairs     Bool       -- <-- Going up?
            | Prop
            | Container
            | Person
            | Union Object Object
            deriving (Show)


isPassable ∷ Object → Bool
isPassable Wall          = False
isPassable Floor         = True
isPassable (Door o)      = o
isPassable (Stairs _)    = True
isPassable Prop          = False -- TODO depends on stats!
isPassable Container     = False -- TODO depends on stats!
isPassable Person        = False -- TODO depends on alliance!
isPassable (Union o1 o2) = isPassable o1 && isPassable o2
{-# INLINE isPassable #-}


isSeeThrough ∷ Object → Bool
isSeeThrough Wall          = False
isSeeThrough Floor         = True
isSeeThrough (Door o)      = o
isSeeThrough (Stairs _)    = True
isSeeThrough Prop          = True -- TODO depends on stats!
isSeeThrough Container     = True -- TODO depends on stats!
isSeeThrough Person        = True -- TODO depends on alliance!
isSeeThrough (Union o1 o2) = isSeeThrough o1 && isSeeThrough o2
{-# INLINE isSeeThrough #-}


objectDescription ∷ Object → String
objectDescription Wall          = "A wall."
objectDescription Floor         = "A floor."
objectDescription (Door o)      = "Just a common door. They're " ++ bool "closed." "opened." o
objectDescription (Stairs t)    = "If map changing would've been coded in, you would use these to go " ++ bool "down." "up." t
objectDescription Prop          = "A prop"
objectDescription Container     = "A container"
objectDescription Person        = "A person"
objectDescription (Union o1 o2) = objectDescription o1 ++ " and " ++ objectDescription o2
{-# INLINE objectDescription #-}

--------------------------------------------------------------------------------

data Visibility = Visible
                | Known
                | Unknown
                deriving (Eq, Show)

--------------------------------------------------------------------------------

data WorldMap = WorldMap {
      _wm_width   ∷ Word
    , _wm_height  ∷ Word
    , _wm_data    ∷ V.Vector Object
    , _wm_visible ∷ V.Vector Visibility
    , _wm_desc    ∷ String
    , _wm_spawns  ∷ V.Vector (V2 Int) 
    }

makeLenses ''WorldMap


instance CoordVector WorldMap where
    width = view wm_width


fromTileMap ∷ TileMap → WorldMap 
fromTileMap tm = 
    WorldMap {
      _wm_width   = tm^.m_width
    , _wm_height  = tm^.m_height
    , _wm_data    = mergeLayers $ V.map layerToObject (tm^.m_layers)
    , _wm_visible = V.replicate squareSize Unknown 
    , _wm_desc    = tm^.m_desc
    , _wm_spawns  = V.fromList $ findAll '╳' (V.head $ tm^.m_layers) -- TODO this has to be proofed for the future a bit
    }
    where
        squareSize   = fromIntegral (tm^.m_width * tm^.m_height)


layerToObject ∷ TileLayer → V.Vector Object
layerToObject tl = charToObject (tl^.l_tileset) <$> (tl^.l_data)
    where
        charToObject ts c = let maybeTile = c `M.lookup` ts
                            in  maybe Floor objectFromTile maybeTile
        objectFromTile (ttype → "Floor")       = Floor
        objectFromTile (ttype → "Wall")        = Wall
        objectFromTile t@(ttype → "Door")      = Door   (doorPassable t)
        objectFromTile t@(ttype → "Stairs")    = Stairs (stairsUp t)
        objectFromTile t@(ttype → "Prop")      = Prop      -- TODO pickup other stuff here
        objectFromTile t@(ttype → "Container") = Container -- TODO pickup other stuff here
        objectFromTile t@(ttype → "Person")    = Person    -- TODO pickup other stuff here
        objectFromTile _                       = error "Unknown object type" -- TODO cast as prop!


mergeLayers ∷ V.Vector (V.Vector Object) → V.Vector Object
mergeLayers   (V.length → 0) = error "No layers to merge!"
mergeLayers i@(V.length → 1) = V.head i
mergeLayers i@(V.length → 2) = uncurry Union <$> V.zip  (i V.! 0) (i V.! 1)
mergeLayers i@(V.length → 3) = merge3 <$> V.zip3 (i V.! 0) (i V.! 1) (i V.! 2)
mergeLayers i@(V.length → 4) = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)
mergeLayers i                = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)

merge3 ∷ (Object, Object, Object) → Object
merge3 (o1, o2, o3) = Union o1 (Union o2 o3)

merge4 ∷ (Object, Object, Object, Object) → Object
merge4 (o1, o2, o3, o4) = Union o1 (merge3 (o2, o3, o4))


outOfBounds ∷ WorldMap → V2 Int → Bool
outOfBounds m (V2 x y)
    | x < 0                                = True
    | y < 0                                = True
    | x >= (fromIntegral $ m ^. wm_width)  = True
    | y >= (fromIntegral $ m ^. wm_height) = True
    | otherwise                            = False


objectAt ∷ V2 Int → WorldMap → Object
objectAt v m = views wm_data (V.! linCoord m v) m


interestingObjects ∷ V2 Int → Word → WorldMap → [V2 Int]
interestingObjects v r m =
    let points = filter (not . outOfBounds m) (floodFillRange r v)
    in  foldr collectObjects [] points
    where
        collectObjects x l = let o = objectAt x m
                             in  case o of
                                    Wall  → l
                                    Floor → l
                                    _     → x : l


--removeFromWorldPile ∷ (MonadWorld w) ⇒ V2 Int → Item → w ()
--removeFromWorldPile v i = w_items %= M.update (wrapMaybe . removeFromList i) v
--    where
--        removeFromList i l = filter (/=i) l
--        wrapMaybe [] = Nothing
--        wrapMaybe l  = Just l
--
--

