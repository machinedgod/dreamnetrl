{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

module Design.ObjectPrograms
where

import Control.Lens     (view, views)
import Data.Semigroup   ((<>))
import Data.Bool        (bool)

import Dreamnet.World     (Object, o_symbol, o_state)
import Dreamnet.Character (ch_name, ch_faction, ch_description)

import Design.DesignAPI
import Design.GameCharacters (characterForName)

--------------------------------------------------------------------------------

programForObject ∷ (ObjectAPI o, Monad o) ⇒ Object States → InteractionType → o ()
programForObject (view o_symbol → '+')          it      = door it
programForObject (view o_symbol → '/')          it      = door it
programForObject (view o_state  → (Camera _ _)) it      = camera it
programForObject (view o_state  → (Person _))   it      = person it
programForObject (view o_state  → (Computer _)) it      = computer it
programForObject (view o_symbol → 'm')          it      = mirror it
programForObject (view o_state  → (Prop n))     Examine = message $ "A " <> n
programForObject _                              _       = pure ()

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
door Examine =
    passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeChar $ bool '+' '/' c
door Talk =
    message "\"Open sesame!\""
door _ =
    pure ()



lock ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
lock Examine =
    message "Its a lock allright"
lock Operate =
    message "Absentmindedly, you try and use your finger as a key, completely oblivious to the fact that this wouldn't work even back when your grandad was a little boy."
lock Talk =
    message "\"Unlock, NOW!\""
lock _ =
    pure ()



computer ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
computer Examine =
    message "Screen, keyboard, cartridge connector.. yeah, pretty standard machine there."
computer Operate =
    get >>= \(Computer cd) → showComputerWindow cd
computer Talk =
    message "*khm* \"LOGIN - CARLA\"..."
computer _ =
    pure ()



person ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
person Examine =
    get >>= \(Person ch) → showInfoWindow (view ch_description ch)
person Operate =
    get >>= \(Person ch) → message $ "Even you yourself are unsure about what exactly you're trying to pull off, but " <> view ch_name ch <> " meets your 'operation' attempts with suspicious look."
person Talk =
    get >>= \(Person ch) → startConversation ch
person _ =
    pure ()



camera ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
camera Examine =
    message "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care."
camera Operate = do
    os   ← scanRange 8 ((=='@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    traverse isFoe viso >>= (\v → modify (\(Camera f _) → Camera f (fromIntegral v))) . length . filter id
    get >>= message . ("Camera alarm level: " <>) . (\(Camera l _) → show l)
    where
        isFoe o = (views o_state (\(Person ch) → view ch_faction ch) o /=) . (\(Camera f _) → f) <$> get
camera _ = 
    pure ()



mirror ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
mirror Examine = do
    designData >>= showInfoWindow . view ch_description . characterForName "Carla" . view dd_characters
mirror Talk =
    designData >>= startConversation . characterForName "Carla" . view dd_characters
mirror _ =
    pure ()


