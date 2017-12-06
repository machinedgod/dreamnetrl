{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dreamnet.Conversation
( ConversationNode(..)
, pick
, advance

, createConversationWindow
, clearConversationWindow
, drawConversationWindow
) where


import Prelude hiding ((!!))
import Safe

import Linear
import Data.Maybe (fromMaybe)

import qualified UI.NCurses as C

--------------------------------------------------------------------------------

data ConversationNode = TalkNode   String   ConversationNode
                      | ListenNode String   ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | End
                      deriving (Eq, Show)


pick ∷ Word → ConversationNode → ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
pick _ (TalkNode   _ n)  = n
pick _ (ListenNode _ n)  = n
pick i (ChoiceNode _ ns) = ns `at` fromIntegral i
pick _ End               = End


advance ∷ ConversationNode → ConversationNode
advance n@(ChoiceNode _ _) = pick 0 n
advance (TalkNode _ n)     = n
advance (ListenNode _ n)   = n
advance End                = End

--------------------------------------------------------------------------------

positions ∷ (Integer, Integer) → [V2 Integer]
positions (rows, columns) =
    let hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight
    in  [ V2 0 (mainHeight `div` 3 * 2)
        , V2 (mainWidth `div` 3 * 2) 0
        ]


createConversationWindow ∷ C.Curses C.Window
createConversationWindow = do
    (rows, columns) ← C.screenSize

    let hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight

    let w = mainWidth `div` 3
        h = mainHeight `div` 3

    C.newWindow h w 0 0


clearConversationWindow ∷ C.Window → C.Curses ()
clearConversationWindow w = C.updateWindow w C.clear


-- TODO REDO as multiple ncurses windows
drawConversationWindow ∷ Word → String → String → C.Window →  C.Curses ()
drawConversationWindow i n s w = do
    (V2 x y) ← (\ss → fromMaybe (positions ss `at` 0) $ (`atMay` fromIntegral i) $ positions ss) <$> C.screenSize

    C.updateWindow w $ do
        C.moveWindow y x
        C.drawBorder (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '╭' [])
                     (Just $ C.Glyph '╮' [])
                     (Just $ C.Glyph '╰' [])
                     (Just $ C.Glyph '╯' [])

        (rows, columns) ← C.windowSize

        drawNameBox columns
        drawText
        drawWidgets rows columns
    where
        drawNameBox cols = do
            let trimmedName  = take (fromIntegral cols - 7) n
            C.moveCursor 2 0
            C.drawGlyph (C.Glyph '├' [])
            C.moveCursor 1 2
            C.drawString trimmedName
            C.moveCursor 0 (fromIntegral (length trimmedName) + 3)
            C.drawGlyph (C.Glyph '┬' [])
            C.moveCursor 2 (fromIntegral (length trimmedName) + 3)
            C.drawGlyph (C.Glyph '╯' [])
            C.moveCursor 2 1
            C.drawLineH (Just $ C.Glyph '─' []) (fromIntegral (length trimmedName) + 2)
            C.moveCursor 1 (fromIntegral (length trimmedName) + 3)
            C.drawGlyph (C.Glyph '│' [])
        drawText = do
            C.moveCursor 3 2
            C.drawString s
        drawWidgets rows cols = do
            C.moveCursor 1 (cols - 3)
            C.drawGlyph (C.Glyph '▲' [])
            C.moveCursor (rows - 2) (cols - 3)
            C.drawGlyph (C.Glyph '▼' [])
