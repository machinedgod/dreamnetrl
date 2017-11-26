{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ScrollModel
( ScrollModelAPI(..)
, ScrollModel
, createScrollModel
, visibleLines
, isAtTop
, hasMoreLines

, lineList
) where

import Prelude hiding (head)
import Safe
import Control.Lens
import Control.Monad.State
import Data.Bool (bool)

import qualified Data.Vector as Vec

--------------------------------------------------------------------------------

class ScrollModelAPI m where
    setText      ∷ String → m ()
    setLineWidth ∷ Word → m ()
    setMaxLines  ∷ Word → m ()
    scrollUp     ∷ m ()
    scrollDown   ∷ m ()
    
--------------------------------------------------------------------------------

-- In case I forget: Data types here are INT and not WORD because WORD
-- wraps around instead of going into negative numbers or clipping.
-- Bit me in the ass nicely...
data ScrollData = ScrollData {
      _sd_text ∷ String
    , _sd_lineWidth ∷ Int
    , _sd_maxLines ∷ Int
    , _sd_startLine ∷ Int
    }

makeLenses ''ScrollData


defaultScrollData ∷ ScrollData
defaultScrollData = ScrollData "" 0 0 0

--------------------------------------------------------------------------------

type ScrollModel = State ScrollData ()


createScrollModel ∷ Word → Word → ScrollModel
createScrollModel w ml = put $ ScrollData "" (fromIntegral w) (fromIntegral ml) 0


visibleLines ∷ ScrollModel → Vec.Vector String
visibleLines st = let sd        = execState st defaultScrollData
                      text      = view sd_text      sd
                      width     = view sd_lineWidth sd
                      maxLines  = view sd_maxLines  sd
                      startLine = view sd_startLine sd
                  in  Vec.take maxLines $ Vec.drop startLine (lineList width text)


isAtTop ∷ ScrollModel → Bool
isAtTop sm = let sd = execState sm defaultScrollData
             in  sd ^. sd_startLine == 0


hasMoreLines ∷ ScrollModel → Bool
hasMoreLines sm = let sd = execState sm defaultScrollData
                  in  totalLineCount sd >= (sd^.sd_startLine + 1) + sd^.sd_maxLines -- Where are these +1's coming from???

--------------------------------------------------------------------------------

instance ScrollModelAPI (State ScrollData) where
    setText s      = do
        sd_text      .= s
        sd_startLine .= 0
    setLineWidth w = sd_lineWidth .= fromIntegral w
    setMaxLines  l = sd_maxLines  .= fromIntegral l
    scrollUp       = sd_startLine %= (\i → max 0 (i - 1))
    scrollDown     = do
        nsl ← uses sd_startLine (+1)  -- <--------------------------------------------- Like this one here too!
        tlc ← totalLineCount <$> get
        vlc ← use sd_maxLines
        when (nsl <= tlc - vlc) $
            sd_startLine += 1 


totalLineCount ∷ ScrollData → Int
totalLineCount sd = let width    = sd ^. sd_lineWidth
                        text     = sd ^. sd_text
                    in  Vec.length (lineList width text)


-- TODO sometime in the future, fix this without hackish folds and reverses and
--      trimming of empty lines
--      I *do* understand how folds work, but I'm too tired now to focus on it
--      enough to get it right
lineList ∷ Int → String → Vec.Vector String
lineList w s = Vec.fromList $ dropWhile (=="") $ reverse $ foldl (concatLines w) [] (lines s)
    where
        concatLines ∷ Int → [String] → String → [String]
        concatLines w newLines l = let bls = foldl (breakLine w) [] (words l)
                                   in  bls ++ newLines ++ [""]

        breakLine ∷ Int → [String] → String → [String]
        breakLine w lines word = let topLine = headDef (replicate w ' ') lines
                                     modLine = topLine ++ ' ' : word
                                 in  if length modLine > w
                                         then word : lines
                                         else modLine : drop 1 lines

