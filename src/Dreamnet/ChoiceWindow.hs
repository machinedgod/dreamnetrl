{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.ChoiceWindow
( ChoiceWindowAPI(..)
, ChoiceData
, commit
, createChoiceWindow
, updateChoiceWindow
, drawChoiceWindow
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V

import qualified UI.NCurses as C

--------------------------------------------------------------------------------

class ChoiceWindowAPI c where
    setOptions ∷ [String] → c ()
    selectNext ∷ c ()
    selectPrevious ∷ c ()
    
--------------------------------------------------------------------------------

data ChoiceData = ChoiceData {
      _cd_options ∷ V.Vector String
    , _cd_currentSelection ∷ Int

    , _cd_window ∷ C.Window
    }


makeLenses ''ChoiceData


newtype ChoiceWindowM a = ChoiceWindowM { runChoiceWindowM ∷ State ChoiceData a }
                        deriving (Functor, Applicative, Monad, MonadState ChoiceData)


instance ChoiceWindowAPI ChoiceWindowM where
    setOptions ls = cd_options .= V.fromList ls
    selectNext = do
        maxI ← uses cd_options (subtract 1 . V.length)
        cd_currentSelection %= min maxI  . (+1)
    selectPrevious = cd_currentSelection %= max 0 . subtract 1
    

updateChoiceWindow ∷ ChoiceWindowM () → ChoiceData → ChoiceData
updateChoiceWindow cu cd = execState (runChoiceWindowM cu) cd


commit ∷ ChoiceData → Word
commit = views cd_currentSelection fromIntegral

--------------------------------------------------------------------------------

createChoiceWindow ∷ C.Curses ChoiceData
createChoiceWindow = do
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
