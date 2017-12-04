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
) where

import Prelude hiding (head)

import Control.Lens
import Control.Monad.State
import Data.Monoid

import qualified Data.Vector as Vec


--------------------------------------------------------------------------------

class ScrollModelAPI m where
    setText      ∷ String → m ()
    setLines     ∷ [String] → m ()
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
    , _sd_lines ∷ [String]
    , _sd_lineWidth ∷ Int
    , _sd_maxLines ∷ Int
    , _sd_startLine ∷ Int
    }

makeLenses ''ScrollData


defaultScrollData ∷ ScrollData
defaultScrollData = ScrollData "" [] 0 0 0

--------------------------------------------------------------------------------

type ScrollModel = State ScrollData ()


createScrollModel ∷ Word → Word → ScrollModel
createScrollModel w ml = put $ ScrollData "" [] (fromIntegral w) (fromIntegral ml) 0


visibleLines ∷ ScrollModel → Vec.Vector String
visibleLines st = let sd        = execState st defaultScrollData
                      maxLines  = view sd_maxLines  sd
                      startLine = view sd_startLine sd
                  in  Vec.fromList $ take maxLines $ drop startLine $ view sd_lines sd


isAtTop ∷ ScrollModel → Bool
isAtTop sm = let sd = execState sm defaultScrollData
             in  sd ^. sd_startLine == 0


hasMoreLines ∷ ScrollModel → Bool
hasMoreLines sm = let sd = execState sm defaultScrollData
                  in  totalLineCount sd >= (sd^.sd_startLine + 1) + sd^.sd_maxLines -- Where are these +1's coming from???

--------------------------------------------------------------------------------

instance ScrollModelAPI (State ScrollData) where
    setText s      = do
        w ← use sd_lineWidth
        sd_text      .= s
        sd_lines     .= lines' w length " " (words s)
        sd_startLine .= 0
    setLines l     = sd_lines     .= l
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
totalLineCount sd = views sd_lines length sd



lines' ∷ (Eq a, Monoid a, Ord b, Foldable t) ⇒ b → (a → b) → a → t a → [a]
lines' l lf sep xs = let (ln, r) = line'' l lf sep xs
                     in  if null r
                           then ln : []
                           else ln : lines' l lf sep r


line'' ∷ (Eq a, Monoid a, Ord b, Foldable t) ⇒ b → (a → b) → a → t a → (a, [a])
line'' l lf sep = foldl (\(f, b) x → if b /= mempty
                                       then (f, b <> [x])
                                       else if lf (f <> sep <> x) < l
                                         then (f <> sep <> x, b)
                                         else (f, [x])) (mempty, [])

{-
-- TODO sometime in the future, fix this without hackish folds and reverses and
--      trimming of empty lines
--      I *do* understand how folds work, but I'm too tired now to focus on it
--      enough to get it right
--      Additonally, this is one ugly fucking type
lineList ∷ (Eq a) ⇒ [a] → ([a] → Int) → Int → [[a]] → [[a]]
lineList sep len w = dropWhile (==mempty) . reverse . foldl (\l → concatLines sep len l . splitOn sep) []
    where
        concatLines ∷ (Monoid a) ⇒ a → (a → Int) → [a] → [a] → [a]
        concatLines sep len newLines ls = let bls = foldl (joinLine sep len) [] ls
                                          in  bls <> newLines <> mempty

        -- Before adding more polymorphism, consider 'headMay' and (:)
        joinLine ∷ (Monoid a) ⇒ a → (a → Int) → [a] → a → [a]
        joinLine sep len lns word = let hedM    = foldr (const . Just) Nothing
                                        topLine = fromMaybe mempty $ hedM lns
                                        modLine = topLine <> sep <> word
                                    in  if len modLine > w
                                                then word : lns
                                                else modLine : drop 1 lns
-}
 
{-
-- Eventually use this one...
line ∷ (Monoid a) ⇒ (a → Int) → Int → a → [a] → a   -- <-- Last parameters: this is a fold :-O
line _   _ _   []    = mempty
line len w sep (x:xs)
    | w >= len x + len sep = x <> sep <> line len (w - (len x + len sep)) sep xs
    | w >= len x           = x
    | otherwise            = mempty


-- Rewriten as fold, rather than recursion
line' ∷ (Eq a, Monoid a, Ord b, Foldable t) ⇒ b → (a → b) → a → t a → (a, a)
line' l lf sep = foldl (\(f, b) x → if b /= mempty
                                      then (f, b <> sep <> x)
                                      else if lf (f <> sep <> x) < l
                                        then (f <> sep <> x, b)
                                        else (f, x)) (mempty, mempty)
-}
