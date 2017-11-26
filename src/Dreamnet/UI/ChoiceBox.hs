{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.UI.ChoiceBox
( drawChoice
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Vector as Vec

import Dreamnet.Input
import Dreamnet.Renderer
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

drawChoice ∷ (MonadRender r) ⇒ Word → Vec.Vector String → r ()
drawChoice c ls = use rd_choiceWindow >>= \w → updateWindow w $ do
    Curses.clear
    Curses.drawBorder (Just $ Curses.Glyph '╷' [])
                      (Just $ Curses.Glyph '╷' [])
                      (Just $ Curses.Glyph '╶' [])
                      (Just $ Curses.Glyph '╶' [])
                      (Just $ Curses.Glyph '╭' [])
                      (Just $ Curses.Glyph '╮' [])
                      (Just $ Curses.Glyph '╰' [])
                      (Just $ Curses.Glyph '╯' [])

    Vec.imapM_ drawChoiceLine ls
    drawSelectionWidget c

    where
        drawChoiceLine i l = do
            (_, columns) ← Curses.windowSize
            let lineStart     = 5
            let maxLineLength = columns - lineStart - 2 
            Curses.moveCursor (fromIntegral i + 1) lineStart
            Curses.drawString (take (fromIntegral maxLineLength) l)
        drawSelectionWidget i = do
            let widgetStart = 2
            Curses.moveCursor (fromIntegral i + 1) widgetStart
            Curses.drawGlyph (Curses.Glyph '»' [])
