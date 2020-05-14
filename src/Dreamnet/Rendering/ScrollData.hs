{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.Rendering.ScrollData
( ScrollData, sdPosition, sdSize, sdTitle

, newScrollData, newScrollData', scrollUp, scrollDown, visibleLines
, lineWidth, maxLines, isAtTop, hasMoreLines
) where


import Control.Lens (makeLenses, view, views, (%~), (+~))
import Linear       (V2, _x, _y)
import Data.List    (intercalate, genericTake)

import qualified Data.Vector as V (Vector, fromList)

import Dreamnet.Engine.Utils (fmt)

--------------------------------------------------------------------------------

data ScrollData = ScrollData {
      _sdLines     ∷ [String]
    , _sdStartLine ∷ Int
    , _sdTitle     ∷ Maybe String

    , _sdPosition  ∷ V2 Integer
    , _sdSize      ∷ V2 Integer
    }
    deriving (Eq, Show)
makeLenses ''ScrollData


newScrollData ∷ V2 Integer → V2 Integer → Maybe String → String → ScrollData
newScrollData p s t txt =
    ScrollData {
      _sdLines     = intercalate [""] $ fmt (views _x (subtract 6) s) (fromIntegral . length) " " . words <$> lines txt
    , _sdStartLine = 0
    , _sdTitle     = t

    , _sdPosition  = p
    , _sdSize      = s
    }


newScrollData' ∷ V2 Integer → V2 Integer → Maybe String → [String] → ScrollData
newScrollData' p s t lst =
    ScrollData {
      _sdLines     = genericTake (views _x (subtract 6) s) <$> lst
    , _sdStartLine = 0
    , _sdTitle     = t

    , _sdPosition  = p
    , _sdSize      = s
    }


scrollUp ∷ ScrollData → ScrollData
scrollUp = sdStartLine %~ (\i → max 0 (i - 1))


scrollDown ∷ ScrollData → ScrollData
scrollDown sd = let nsl = views sdStartLine (+1) sd -- <------ Where are these +1's coming from???
                    tlc = views sdLines length sd
                in  if nsl <= tlc - maxLines sd
                        then sdStartLine +~ 1 $ sd
                        else sd


maxLines ∷ (Num a) ⇒ ScrollData → a
maxLines = fromIntegral . subtract 2 . view (sdSize._y) -- TODO should depend on the title being present!


lineWidth ∷ (Num a) ⇒ ScrollData → a
lineWidth = fromIntegral . subtract 6 . view (sdSize._x) -- (-6) border, padding, arrow widgets


visibleLines ∷ ScrollData → V.Vector String
visibleLines sd =
    V.fromList . take (maxLines sd) . drop (view sdStartLine sd) . view sdLines $ sd


isAtTop ∷ ScrollData → Bool
isAtTop = (/= 0) . view sdStartLine


hasMoreLines ∷ ScrollData → Bool
hasMoreLines sd = length (view sdLines sd) >= (view sdStartLine sd + 1) + maxLines sd
