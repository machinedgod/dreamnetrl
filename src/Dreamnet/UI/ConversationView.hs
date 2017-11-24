{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}

module Dreamnet.UI.ConversationView
( clearConversationWindow
, drawConversationWindow
) where

import Dreamnet.Renderer
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

clearConversationWindow ∷ (MonadRender r) ⇒ Word → r ()
clearConversationWindow i = do
    w ← view $ case i of
        0 → re_data.rd_conversationWindow0
        1 → re_data.rd_conversationWindow1
        _ → re_data.rd_conversationWindow0
    updateWindow w Curses.clear


-- TODO REDO as multiple ncurses windows
drawConversationWindow ∷ (MonadRender r) ⇒ Word → String → String → r ()
drawConversationWindow i n s = do
    w ← view $ case i of
        0 → re_data.rd_conversationWindow0
        1 → re_data.rd_conversationWindow1
        _ → re_data.rd_conversationWindow0

    updateWindow w (drawWindow n s)
    where
        drawWindow n s = do
            Curses.clear
            Curses.drawBorder (Just $ Curses.Glyph '│' [])
                              (Just $ Curses.Glyph '│' [])
                              (Just $ Curses.Glyph '─' [])
                              (Just $ Curses.Glyph '─' [])
                              (Just $ Curses.Glyph '╭' [])
                              (Just $ Curses.Glyph '╮' [])
                              (Just $ Curses.Glyph '╰' [])
                              (Just $ Curses.Glyph '╯' [])

            (rows, columns) ← Curses.windowSize

            drawNameBox columns n
            drawText s
            drawWidgets rows columns
        drawNameBox cols n = do
            let trimmedName  = take (fromIntegral cols - 7) n
            Curses.moveCursor 2 0
            Curses.drawGlyph (Curses.Glyph '├' [])
            Curses.moveCursor 1 2
            Curses.drawString trimmedName
            Curses.moveCursor 0 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '┬' [])
            Curses.moveCursor 2 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '╯' [])
            Curses.moveCursor 2 1
            Curses.drawLineH (Just $ Curses.Glyph '─' []) (fromIntegral (length trimmedName) + 2)
            Curses.moveCursor 1 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '│' [])
        drawText s = do
            Curses.moveCursor 3 2
            Curses.drawString s
        drawWidgets rows cols = do
            Curses.moveCursor 1 (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▲' [])
            Curses.moveCursor (rows - 2) (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▼' [])
