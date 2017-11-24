{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}

module Dreamnet.UI.InformationWindow
( drawCenteredWindow
) where

import Dreamnet.Renderer
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

-- TODO add title & shit
drawCenteredWindow ∷ (MonadRender r) ⇒ String → String → r ()
drawCenteredWindow t s = do
    w ← view (re_data.rd_examineWindow)
    updateWindow w $ do
        Curses.clear
        Curses.drawBorder (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '╭' [])
                          (Just $ Curses.Glyph '╮' [])
                          (Just $ Curses.Glyph '╰' [])
                          (Just $ Curses.Glyph '╯' [])
        Curses.moveCursor 2 2

        -- TODO TRIM and draw lines wiht 'words' to fit
        Curses.drawString s


