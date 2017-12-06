{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.ChoiceWindow
( ChoiceData
, setOptions
, selectNext
, selectPrevious
, commit
, createChoiceData
, drawChoiceWindow
) where

import Control.Lens
import qualified Data.Vector as V

import qualified UI.NCurses as C

--------------------------------------------------------------------------------

data ChoiceData = ChoiceData {
      _cd_options ∷ V.Vector String
    , _cd_currentSelection ∷ Int

    , _cd_window ∷ C.Window
    }


makeLenses ''ChoiceData


createChoiceData ∷ C.Curses ChoiceData
createChoiceData = do
    (rows, columns) ← C.screenSize
    let mainWidth  = columns
        mainHeight = rows - 8 -- Hud

    let lowLeftW = mainWidth `div` 3
        lowLeftH = mainHeight `div` 3
        lowLeftX = 0
        lowLeftY = mainHeight `div` 3 * 2

    w ← C.newWindow lowLeftH lowLeftW lowLeftY lowLeftX
    return $ ChoiceData V.empty 0 w

--------------------------------------------------------------------------------

setOptions ∷ [String] → ChoiceData → ChoiceData
setOptions ls = cd_options .~ V.fromList ls


selectNext ∷ ChoiceData → ChoiceData
selectNext cd = let maxI = views cd_options (subtract 1 . V.length) cd
                in  cd_currentSelection %~ min maxI  . (+1) $ cd


selectPrevious ∷ ChoiceData → ChoiceData
selectPrevious = cd_currentSelection %~ max 0 . subtract 1
    

commit ∷ ChoiceData → Word
commit = views cd_currentSelection fromIntegral

--------------------------------------------------------------------------------

drawChoiceWindow ∷ ChoiceData → C.Curses ()
drawChoiceWindow cd =
    let w  = view cd_window cd
        ls = view cd_options cd
        c  = view cd_currentSelection cd
    in  C.updateWindow w $ do
            C.clear
            C.drawBorder (Just $ C.Glyph '╷' [])
                         (Just $ C.Glyph '╷' [])
                         (Just $ C.Glyph '╶' [])
                         (Just $ C.Glyph '╶' [])
                         (Just $ C.Glyph '╭' [])
                         (Just $ C.Glyph '╮' [])
                         (Just $ C.Glyph '╰' [])
                         (Just $ C.Glyph '╯' [])

            V.imapM_ drawChoiceLine ls
            drawSelectionWidget c
    where
        drawChoiceLine i l = do
            (_, columns) ← C.windowSize
            let lineStart     = 5
            let maxLineLength = columns - lineStart - 2 
            C.moveCursor (fromIntegral i + 1) lineStart
            C.drawString (take (fromIntegral maxLineLength) l)
        drawSelectionWidget i = do
            let widgetStart = 2
            C.moveCursor (fromIntegral i + 1) widgetStart
            C.drawGlyph (C.Glyph '»' [])
