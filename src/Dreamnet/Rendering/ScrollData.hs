{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.Rendering.ScrollData
( ScrollData
, sd_position
, sd_size
, sd_title

, newScrollData
, newScrollData'
, scrollUp
, scrollDown
, visibleLines

, lineWidth
, maxLines

, isAtTop
, hasMoreLines
) where


import Control.Lens (makeLenses, view, views, (%~), (+~))
import Linear       (V2, _x, _y)
import Data.List    (intercalate, genericTake)

import qualified Data.Vector as V (Vector, fromList)

import Dreamnet.Engine.Utils (lines')

--------------------------------------------------------------------------------

data ScrollData = ScrollData {
      _sd_lines     ∷ [String]
    , _sd_startLine ∷ Int
    , _sd_title     ∷ Maybe String

    , _sd_position  ∷ V2 Integer
    , _sd_size      ∷ V2 Integer
    }
    deriving (Eq, Show)
makeLenses ''ScrollData


newScrollData ∷ V2 Integer → V2 Integer → Maybe String → String → ScrollData
newScrollData p s t txt =
    ScrollData {
      _sd_lines     = intercalate [""] $ lines' (views _x (subtract 6) s) (fromIntegral . length) " " . words <$> lines txt
    , _sd_startLine = 0
    , _sd_title     = t

    , _sd_position  = p
    , _sd_size      = s
    }


newScrollData' ∷ V2 Integer → V2 Integer → Maybe String → [String] → ScrollData
newScrollData' p s t lst =
    ScrollData {
      _sd_lines     = genericTake (views _x (subtract 6) s) <$> lst
    , _sd_startLine = 0
    , _sd_title     = t

    , _sd_position  = p
    , _sd_size      = s
    }


scrollUp ∷ ScrollData → ScrollData
scrollUp = sd_startLine %~ (\i → max 0 (i - 1))


scrollDown ∷ ScrollData → ScrollData
scrollDown sd = let nsl = views sd_startLine (+1) sd -- <------ Where are these +1's coming from???
                    tlc = views sd_lines length sd
                in  if nsl <= tlc - maxLines sd
                        then sd_startLine +~ 1 $ sd
                        else sd


maxLines ∷ (Num a) ⇒ ScrollData → a
maxLines = fromIntegral . subtract 2 . view (sd_size._y) -- TODO should depend on the title being present!


lineWidth ∷ (Num a) ⇒ ScrollData → a
lineWidth = fromIntegral . subtract 6 . view (sd_size._x) -- (-6) border, padding, arrow widgets


visibleLines ∷ ScrollData → V.Vector String
visibleLines sd =
    V.fromList . take (maxLines sd) . drop (view sd_startLine sd) . view sd_lines $ sd


isAtTop ∷ ScrollData → Bool
isAtTop = (/= 0) . view sd_startLine 


hasMoreLines ∷ ScrollData → Bool
hasMoreLines sd = length (view sd_lines sd) >= (view sd_startLine sd + 1) + maxLines sd
