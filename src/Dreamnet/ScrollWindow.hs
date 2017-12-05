{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ScrollWindow
( ScrollWindowAPI(..)
, ScrollData

, createScrollWindow
, updateScrollWindow

, clearScrollWindow
, renderScrollWindow
) where


import Prelude hiding (head)

import Control.Lens
import Control.Monad.State
import Data.Monoid

import qualified Data.Vector as V
import qualified UI.NCurses as C

--------------------------------------------------------------------------------

class ScrollWindowAPI m where
    setText    ∷ String → m ()
    setLines   ∷ [String] → m ()
    scrollUp   ∷ m ()
    scrollDown ∷ m ()

--------------------------------------------------------------------------------

-- In case I forget: Data types here are INT and not WORD because WORD
-- wraps around instead of going into negative numbers or clipping.
-- Bit me in the ass nicely...
data ScrollData = ScrollData {
      _sd_lines ∷ [String]
    , _sd_lineWidth ∷ Int
    , _sd_maxLines ∷ Int
    , _sd_startLine ∷ Int

    , _sd_window ∷ C.Window
    }

makeLenses ''ScrollData


createScrollWindow ∷ C.Curses ScrollData
createScrollWindow = do
    --(rows, columns) ← C.screenSize
    --let mainWidth  = columns
    --    mainHeight = rows

    let examineW = 60
        examineH = 20
        --examineX = (mainWidth - examineW) `div` 2
        --examineY = (mainHeight - examineH) `div` 2
        examineX = 2
        examineY = 1
    win ← C.newWindow examineH examineW examineY examineX

    let scrollW = fromIntegral $ examineW - 6 -- border, padding, arrow widgets
        scrollH = fromIntegral $ examineH - 2
    return $ ScrollData [] scrollW scrollH 0 win

--------------------------------------------------------------------------------

newtype ScrollWindowM a = ScrollWindowM { runScrollWindowM ∷ State ScrollData a }
                        deriving (Functor, Applicative, Monad, MonadState ScrollData)


updateScrollWindow ∷ ScrollWindowM () → ScrollData → ScrollData
updateScrollWindow sw sd = execState (runScrollWindowM sw) sd


instance ScrollWindowAPI ScrollWindowM where
    setText s = do
        w ← use sd_lineWidth
        setLines (lines' w length " " (words s))

    setLines ls = sd_lines .= ls

    scrollUp = sd_startLine %= (\i → max 0 (i - 1))

    scrollDown = do
        nsl ← uses sd_startLine (+1)  -- <------ Where are these +1's coming from???
        tlc ← uses sd_lines length
        vlc ← use sd_maxLines
        when (nsl <= tlc - vlc) $
            sd_startLine += 1 

--------------------------------------------------------------------------------

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

