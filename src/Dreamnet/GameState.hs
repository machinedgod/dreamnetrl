{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.GameState
( GameState(..)
) where


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
               | Examination
               | Operation
               | HudTeam
               | HudMessages
               | HudWatch
               | Conversation
               | InventoryUI
               | CharacterUI
               deriving (Eq, Show)
