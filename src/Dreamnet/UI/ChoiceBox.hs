{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.UI.ChoiceBox
( ChoiceModel(..)
, cm_options
, cm_currentSelection

, drawChoice
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Vector as Vec

import Dreamnet.Input
import Dreamnet.Renderer
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------

data ChoiceModel = ChoiceModel {
      _cm_options ∷ Vec.Vector String
    , _cm_currentSelection ∷ Word
    }

makeLenses ''ChoiceModel

--------------------------------------------------------------------------------

type ChoiceM a = State ChoiceModel a 

--------------------------------------------------------------------------------

controlChoice ∷ UIEvent → ChoiceM Bool
controlChoice MoveUp = do
    cm_currentSelection -= 1 
    return False
controlChoice MoveDown = do
    maxV ← uses cm_options (\v → fromIntegral $ Vec.length v - 1) 
    cm_currentSelection %= min maxV . (+1)
    return False
controlChoice SelectChoice = return True
controlChoice Back = return False

--------------------------------------------------------------------------------

drawChoice ∷ (MonadRender r) ⇒ Word → Vec.Vector String → r ()
drawChoice c ls = view (re_data.rd_choiceWindow) >>= \w → updateWindow w $ do
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
