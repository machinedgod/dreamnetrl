{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ScrollWindow
( ScrollData

, createScrollData
, setText
, setLines
, scrollUp
, scrollDown

, clearScrollWindow
, renderScrollWindow

, lines'
, line''
) where


import Control.Lens  (makeLenses, view, views, (%~), (+~), (.~))
import Control.Monad (when, unless)
import Data.Foldable (foldl', toList)
import Data.Monoid   ((<>), mempty)
import Data.List     (intercalate)

import qualified Data.Vector as V (fromList, imapM_)
import qualified UI.NCurses  as C

--------------------------------------------------------------------------------

-- In case I forget: Data types here are INT and not WORD because WORD
-- wraps around instead of going into negative numbers or clipping.
-- Bit me in the ass nicely...
data ScrollData = ScrollData {
      _sd_lines     ∷ [String]
    , _sd_lineWidth ∷ Int
    , _sd_maxLines  ∷ Int
    , _sd_startLine ∷ Int

    , _sd_window    ∷ C.Window
    }

makeLenses ''ScrollData


createScrollData ∷ C.Curses ScrollData
createScrollData = do
    (rows, columns) ← C.screenSize
    let mainWidth  = columns
        mainHeight = rows

    let examineW = 40
        examineH = 10
        --examineX = (mainWidth - examineW) `div` 2
        --examineY = (mainHeight - examineH) `div` 2
        examineX = 2
        examineY = 1
    win ← C.newWindow examineH examineW examineY examineX

    let scrollW = fromIntegral $ examineW - 6 -- border, padding, arrow widgets
        scrollH = fromIntegral $ examineH - 2
    return $ ScrollData [] scrollW scrollH 0 win

--------------------------------------------------------------------------------

setText ∷ String → ScrollData → ScrollData
setText s sd = let w   = view sd_lineWidth sd
                   ls  = intercalate [""] $ lines' w length " " . words <$> lines s
               in  setLines ls sd


setLines ∷ [String] → ScrollData → ScrollData
setLines ls = sd_lines .~ ls


scrollUp ∷ ScrollData → ScrollData
scrollUp = sd_startLine %~ (\i → max 0 (i - 1))


scrollDown ∷ ScrollData → ScrollData
scrollDown sd = let nsl = views sd_startLine (+1) sd -- <------ Where are these +1's coming from???
                    tlc = views sd_lines length sd
                    vlc = view sd_maxLines sd
                in  if nsl <= tlc - vlc
                        then sd_startLine +~ 1 $ sd
                        else sd

--------------------------------------------------------------------------------

lines' ∷ (Eq a, Monoid a, Ord b, Foldable t) ⇒ b → (a → b) → a → t a → [a]
lines' l lf sep xs = let (ln, r) = line'' l lf sep xs
                     in  if null r && ln == mempty
                           then error "Phrase longer than limit (cannot be separated by separator!)"
                           else if null r
                             then ln : []
                             else ln : lines' l lf sep r


line'' ∷ (Eq a, Monoid a, Ord b, Foldable t) ⇒ b → (a → b) → a → t a → (a, [a])
line'' l lf sep = foldl' (\(f, b) x → if b /= mempty
                                        then (f, b <> [x])
                                        else if lf (f <> sep <> x) < l
                                          then (f <> sep <> x, b)
                                          else (f, [x])) (mempty, [])

--------------------------------------------------------------------------------

clearScrollWindow ∷ ScrollData → C.Curses ()
clearScrollWindow sd = C.updateWindow (view sd_window sd) C.clear


renderScrollWindow ∷ ScrollData → C.Curses ()
renderScrollWindow sd =
    let w            = view sd_window sd
        maxLines     = view sd_maxLines sd
        startLine    = view sd_startLine sd
        visibleLines = views sd_lines (V.fromList . take maxLines . drop startLine) sd
        isAtTop      = views sd_startLine (==0) sd
        hasMoreLines = views sd_lines (\ls → length ls >= (startLine + 1) + maxLines) sd -- <-------------------------------- Like this one here too!
    in  C.updateWindow w $ do
            C.clear
            C.drawBorder (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '╭' [])
                         (Just $ C.Glyph '╮' [])
                         (Just $ C.Glyph '╰' [])
                         (Just $ C.Glyph '╯' [])
            (r, c) ← C.windowSize
            V.imapM_ drawLine visibleLines
            unless isAtTop $ drawUpWidget c
            when hasMoreLines $ drawDownWidget r c
    where
        drawLine ∷ Int → String → C.Update ()
        drawLine i s = do
            C.moveCursor (fromIntegral i + 1) 2
            C.drawString s
        drawUpWidget cols = do
            C.moveCursor 1 (cols - 3)
            C.drawGlyph (C.Glyph '▲' [])
        drawDownWidget rows cols = do
            C.moveCursor (rows - 2) (cols - 3)
            C.drawGlyph (C.Glyph '▼' [])

