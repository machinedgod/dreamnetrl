{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}

module Dreamnet.UI.InformationWindow
( drawCenteredWindow
, clearCenteredWindow
) where

import Prelude hiding (head)
import Safe
import qualified Data.Vector as Vec

import Dreamnet.ScrollModel
import Dreamnet.Renderer
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

clearCenteredWindow ∷ (MonadRender r) ⇒ r ()
clearCenteredWindow = use rd_examineWindow >>= (`updateWindow` Curses.clear)
    

-- TODO add title & shit
drawCenteredWindow ∷ (MonadRender r) ⇒ r ()
drawCenteredWindow = do
    sm ← use rd_scrollModel
    w  ← use rd_examineWindow
    clearCenteredWindow
    updateWindow w $ do
        Curses.setTouched True
        Curses.drawBorder (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '╭' [])
                          (Just $ Curses.Glyph '╮' [])
                          (Just $ Curses.Glyph '╰' [])
                          (Just $ Curses.Glyph '╯' [])
        (r, c) ← Curses.windowSize
        Vec.imapM_ drawLine $ visibleLines sm
        unless (isAtTop sm) $ drawUpWidget r c
        when (hasMoreLines sm) $ drawDownWidget r c
    where
        drawLine ∷ Int → String → Curses.Update ()
        drawLine i s = do
            Curses.moveCursor (fromIntegral i + 1) 2
            Curses.drawString s
        drawUpWidget rows cols = do
            Curses.moveCursor 1 (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▲' [])
        drawDownWidget rows cols = do
            Curses.moveCursor (rows - 2) (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▼' [])
