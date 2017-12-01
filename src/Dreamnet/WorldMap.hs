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
import Data.Bool  (bool)
import Data.Maybe (fromMaybe)
import Linear

import qualified Data.Vector as V
import qualified Data.Map    as M


import Dreamnet.DesignData
import Dreamnet.CoordVector
import Dreamnet.Utils
import Dreamnet.TileMap
import Dreamnet.TileData
import Dreamnet.Character

--------------------------------------------------------------------------------

-- TODO Container should be a flag on prop or something?
-- TODO eventually, all objects will be squashed into the general
--      object that cojoins all their different properties
--      Basically, a Prop with all the interaction features out-coded
data Object = Base      Char Bool Bool               -- <-- Passable, seeThrough
            | Door      Bool                         -- <-- Is opened?
            | Stairs    Bool                         -- <-- Going up?
            | Prop      Char String String Bool Bool -- <-- Name, Material, passable, see through
            | Person    Character
            | Computer
            | ItemO     String
            | Union Object Object
            deriving (Eq, Show)


isPassable ∷ Object → Bool
isPassable (Base _ p _)     = p
isPassable (Door o)         = o
isPassable (Stairs _)       = True
isPassable (Prop _ _ _ p _) = p
isPassable (Person _)       = False -- TODO Check if allied
isPassable Computer         = False
isPassable (ItemO _)        = True
isPassable (Union o1 o2)    = isPassable o1 && isPassable o2
{-# INLINE isPassable #-}


isSeeThrough ∷ Object → Bool
isSeeThrough (Base _ _ s)     = s
isSeeThrough (Door o)         = o
isSeeThrough (Stairs _)       = True
isSeeThrough (Prop _ _ _ _ s) = s
isSeeThrough (Person _)       = True
isSeeThrough Computer         = True
isSeeThrough (ItemO _)        = True
isSeeThrough (Union o1 o2)    = isSeeThrough o1 && isSeeThrough o2
{-# INLINE isSeeThrough #-}


objectDescription ∷ Object → Maybe String
objectDescription (Base _ _ _)     = Nothing
objectDescription (Door o)         = Just $ "Just a common door. They're " ++ bool "closed." "opened." o
objectDescription (Stairs t)       = Just $ "If map changing would've been coded in, you would use these to go " ++ bool "down." "up." t
objectDescription (Prop _ n _ _ _) = Just $ "A " ++ n ++ "."
objectDescription (Person c)       = Just $ "Its " ++ (c^.ch_name) ++ "."
objectDescription Computer         = Just $ "Your machine. You wonder if Devin mailed you about the job."
objectDescription (ItemO n)        = Just $ "A " ++ n ++ "."
objectDescription (Union o1 o2)    = let md  = objectDescription o1
                                         md2 = objectDescription o2
                                     in  md `mappend` Just ", " `mappend` md2
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
    , _wm_data    = let layerData         = mergeLayers $ V.map layerToObject (tm^.m_layers)
                        maybeObjects i    = M.lookup (coordLin tm i) (tm^.m_positioned)
                        mergeTiles []     = error "Positioned tile list empty, instead of not being added at all! :-O"
                        mergeTiles [t]    = objectFromTile t
                        mergeTiles l      = foldr1 Union $ fmap objectFromTile l
                        addPositioned i o = maybe o (Union o . mergeTiles) (maybeObjects i)
                    in  V.imap addPositioned layerData
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
                                err       = error ("Char " ++ [c] ++ " doesn't exist in the tileset!")
                            in  maybe err objectFromTile maybeTile


objectFromTile ∷ Tile → Object
objectFromTile t@(ttype → "Base")   = Base (t^.t_char) (1 `readBoolProperty` t)  (2 `readBoolProperty` t)
objectFromTile t@(ttype → "Door")   = Door (1 `readBoolProperty` t)
objectFromTile t@(ttype → "Stairs") = Stairs (1 `readBoolProperty` t)
objectFromTile t@(ttype → "Prop")   = Prop (t^.t_char) (1 `readStringProperty` t) (4 `readStringProperty` t) (2 `readBoolProperty` t)  (3 `readBoolProperty` t)
-- TODO DO NOT USE DEFAULT DESIGN DATA! Use the one passed by Dyre!
objectFromTile t@(ttype → "Person") = let name      = 1 `readStringProperty` t
                                          maybeChar = M.lookup name (defaultDesignData^.dd_characters)
                                      in  Person (fromMaybe (defaultDesignData^.dd_defaultRedshirt) maybeChar)
objectFromTile   (ttype → "Spawn")  = Base '.' True True -- TODO shitty hardcoding, spawns should probably be generalized somehow!
objectFromTile   (ttype → "Computer") = Computer
objectFromTile t@(ttype → "Item")   = ItemO (1 `readStringProperty` t)
objectFromTile t                    = error $ "Can't convert Tile type into Object: " ++ show t


mergeLayers ∷ V.Vector (V.Vector Object) → V.Vector Object
mergeLayers   (V.length → 0) = error "No layers to merge!"
mergeLayers i@(V.length → 1) = V.head i
mergeLayers i@(V.length → 2) = merge2 <$> V.zip  (i V.! 0) (i V.! 1)
mergeLayers i@(V.length → 3) = merge3 <$> V.zip3 (i V.! 0) (i V.! 1) (i V.! 2)
mergeLayers i@(V.length → 4) = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)
mergeLayers i                = merge4 <$> V.zip4 (i V.! 0) (i V.! 1) (i V.! 2) (i V.! 3)

merge2 ∷ (Object, Object) → Object
merge2 (o1, o2) = bool o1 (Union o1 o2) (o1 /= o2)

merge3 ∷ (Object, Object, Object) → Object
merge3 (o1, o2, o3) = let oTop = merge2 (o2, o3) 
                      in  bool o1 (Union o1 oTop) (o1 /= oTop)

merge4 ∷ (Object, Object, Object, Object) → Object
merge4 (o1, o2, o3, o4) = let oTop = merge3 (o2, o3, o4)
                          in  bool o1 (Union o1 oTop) (o1 /= oTop)


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
                                    (Base _ _ _) → l
                                    _            → x : l


--removeFromWorldPile ∷ (MonadWorld w) ⇒ V2 Int → Item → w ()
--removeFromWorldPile v i = w_items %= M.update (wrapMaybe . removeFromList i) v
--    where
--        removeFromList i l = filter (/=i) l
--        wrapMaybe [] = Nothing
--        wrapMaybe l  = Just l
--
--

