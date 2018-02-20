{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}

module Dreamnet.Conversation
( ConversationNode(..)
, pick
, advance

, conversationSize
, positionFor
) where


import Safe       (atMay, at)
import Data.Maybe (fromMaybe)
import Linear     (V2(V2))

import qualified UI.NCurses as C (Curses, screenSize)

--------------------------------------------------------------------------------

-- Note: NEVER slap equality on recursive data structures
data ConversationNode = TalkNode   String   ConversationNode
                      | ListenNode String   ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | End
                      deriving (Show)


pick ∷ ConversationNode → Word → ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
pick (TalkNode   _ n)  _ = n
pick (ListenNode _ n)  _ = n
pick (ChoiceNode _ ns) i = ns `at` fromIntegral i
pick End               _ = End


advance ∷ ConversationNode → ConversationNode
advance n@(ChoiceNode _ _) = pick n 0
advance (TalkNode _ n)     = n
advance (ListenNode _ n)   = n
advance End                = End

--------------------------------------------------------------------------------

positions ∷ (Integer, Integer) → [V2 Integer]
positions (rows, columns) =
    let hudHeight  = 13
        mainWidth  = columns
        mainHeight = rows - hudHeight
    in  [ V2 0 (mainHeight `div` 3 * 2)
        , V2 (mainWidth `div` 3 * 2) 0
        ]


conversationSize ∷ C.Curses (V2 Integer)
conversationSize = do
    (rows, columns) ← C.screenSize
    let hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight
    let w = mainWidth `div` 3
        h = mainHeight `div` 3
    pure (V2 w h)


positionFor ∷ Word → C.Curses (V2 Integer)
positionFor i =
    (\ss → fromMaybe (positions ss `at` 0) $ (`atMay` fromIntegral i) $ positions ss) <$> C.screenSize

