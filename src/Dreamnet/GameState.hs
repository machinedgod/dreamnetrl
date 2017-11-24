{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}

module Dreamnet.GameState
( GameState(..)
) where


import Dreamnet.Conversation

data GameState = Normal
               | Examination   String
               | Interaction
               | Conversation  ConversationNode

