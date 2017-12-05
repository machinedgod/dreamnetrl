{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}

module Dreamnet.GameState
( GameState(..)
) where


import Dreamnet.Conversation

-- TODO this could be a Character state,
--      this way it circularly ties back into the game
--      (when rendering characters), rather than World
--      having to produce a new GameState
--
--      Dunno what's better
--
--      After 30 seconds - its the same. *direction* of data
--      is the same.
--
--      Bottomline is, changes in the world dictate changes in the
--      player's domain (UI, controls, etc)
data GameState = Normal
               | Examination   String -- ScrollModel?
               | Interaction
               | Conversation  String ConversationNode
               | InventoryUI
               | CharacterUI
               deriving(Eq)

