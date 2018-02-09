{-# LANGUAGE UnicodeSyntax #-}

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

-- TODO REMOVE extra data from gamestates. Use a different storage place (current aim?)
data GameState = Normal
               | Examination   String -- ScrollModel?
               | Interaction
               | Conversation  String ConversationNode
               | InventoryUI
               | CharacterUI



instance Show GameState where
    show Normal             = "Normal"
    show (Examination _)    = "Examination"
    show Interaction        = "Interaction"
    show (Conversation _ _) = "Conversation"
    show InventoryUI        = "InventoryUI"
    show CharacterUI        = "CharacterUI"

