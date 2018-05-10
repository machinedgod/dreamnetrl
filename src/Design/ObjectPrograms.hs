{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Design.ObjectPrograms
where

import Control.Lens     (view, views)
import Data.Semigroup   ((<>))
import Data.Bool        (bool)

import Dreamnet.Engine.World     (Symbol(Symbol), o_symbol, o_state,
                                  ObjectAPI(..))
import Dreamnet.Engine.Character (ch_name, ch_faction, ch_description)
import Design.ComputerModel
import Design.DesignAPI

--------------------------------------------------------------------------------

data InteractionType = Examine
                     | Operate
                     | Talk
                     | OperateOn   States
                     | OperateWith States

--------------------------------------------------------------------------------

genericProp ∷ (ObjectAPI States o, Monad o) ⇒ String → InteractionType → o ()
genericProp n Examine =
    message $ "A " <> n
genericProp _ _ =
    pure ()



genericClothes ∷ (ObjectAPI States o, Monad o) ⇒ WearableItem i → InteractionType → o ()
genericClothes wi Examine =
    message $ "A " <> view wi_name wi
genericClothes _ _ =
    pure ()



genericWeapon ∷ (ObjectAPI States o, Monad o) ⇒ WeaponItem → InteractionType → o ()
genericWeapon wpi Examine =
    message $ "Nice weapon, a " <> view wpi_name wpi
genericWeapon _ Operate =
    message "This weapon doesn't seem to have any configs or settings."
genericWeapon _ Talk =
    message "You smirk as you think fondly of a vintage show you watched as a kid, with a policeman that used to talk to his gun. You don't think you're nearly cool enough to pull it off, so you put your weapon down."
genericWeapon _ (OperateOn s) =
    message $ "Boom boom! " <> show s <> " is dead!"
genericWeapon wpi (OperateWith (Ammo ami)) =
    message $ "You reload the " <> view wpi_name wpi <> " with ammo clip: " <> view ami_name ami
genericWeapon _ _ =
    pure ()



genericAmmo ∷ (ObjectAPI States o, Monad o) ⇒ AmmoItem → InteractionType → o ()
genericAmmo ami Examine =
    message $ "Nice ammo, a " <> view ami_name ami
genericAmmo ami Operate =
    message $ "You change configuration of this " <> view ami_name ami
genericAmmo _ (OperateOn (Weapon wpi)) =
    message $ "You reload the " <> view wpi_name wpi
genericAmmo _ (OperateWith (Weapon wpi)) =
    message $ "While you find its really difficult and time consuming, somehow you manage to insert the clip into " <> view wpi_name wpi <> " by ramming it over the clip."
genericAmmo _ _ =
    pure ()
    


genericThrowable ∷ (ObjectAPI States o, Monad o) ⇒ ThrownWeaponItem → InteractionType → o ()
genericThrowable twi Examine =
    message $ "Nice throwable, a " <> view twi_name twi
genericThrowable twi Operate =
    message $ "The " <> view twi_name twi <> " is now armed, gulp."
genericThrowable twi (OperateOn (Person ch)) =
    message $ "You try to show your " <> view twi_name twi <> " up " <> view ch_name ch <> "'s ass."
genericThrowable twi (OperateWith s) =
    message $ "You try and hit the " <> view twi_name twi <> " with " <> show s <> ". You can't possibly imagine this being a good idea, but you keep trying nevertheless."
genericThrowable _ _ =
    pure ()

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI a o, Monad o) ⇒ InteractionType → o ()
door Examine =
    passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeSymbol $ bool (Symbol '+') (Symbol '/') c
door Talk =
    message "\"Open sesame!\""
door _ =
    pure ()



lock ∷ (ObjectAPI a o, Monad o) ⇒ InteractionType → o ()
lock Examine =
    message "Its a lock allright"
lock Operate =
    message "Absentmindedly, you try and use your finger as a key, completely oblivious to the fact that this wouldn't work even back when your grandad was a little boy."
lock Talk =
    message "\"Unlock, NOW!\""
lock (OperateWith (Prop "key")) =
    message "Unlocked!"
lock (OperateWith _) =
    message "That won't work."
lock _ =
    pure ()



computer ∷ (ObjectAPI States o, Monad o) ⇒ ComputerData → InteractionType → o ()
computer _ Examine =
    message "Screen, keyboard, cartridge connector.. yeah, pretty standard machine there."
computer _ Operate =
    message "Should be showing computer window now, but how?"
    --showComputerWindow cd
computer _ Talk =
    message "*khm* \"LOGIN - CARLA\"..."
computer _ _ =
    pure ()



person ∷ (ObjectAPI States o, Monad o) ⇒ DreamnetCharacter → InteractionType → o ()
person ch Examine =
    message $ view ch_description ch
person ch Operate =
    message $ "Even you yourself are unsure about what exactly you're trying to pull off, but " <> view ch_name ch <> " meets your 'operation' attempts with suspicious look."
person ch Talk =
    message $ "Here, I should be starting a conversation with " <> view ch_name ch <> ", but how to encode that?"
    --startConversation ch
person _ _ =
    pure ()



-- TODO mixes general Object knowledge with States knowledge. This is an issue!
camera ∷ (ObjectAPI States o, Monad o) ⇒ Faction → Word → InteractionType → o ()
camera _ _ Examine =
    message "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care."
camera f l Operate = do
    os   ← scanRange 8 ((==Symbol '@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    -- traverse isFoe viso >>= (\v → modifyState (\(Camera f _) → Camera f (fromIntegral v))) . length -- . filter id
    message $ "Camera alarm level: " <> show l
    where
        isFoe o = f /= views o_state (\(Person ch) → view ch_faction ch) o
camera _ _ _ = 
    pure ()



mirror ∷ (ObjectAPI a o, Monad o) ⇒ DreamnetCharacter → InteractionType → o ()
mirror ch Examine =
    message $ view ch_description ch
mirror ch Talk =
    message $ view ch_name ch <> " is about to start conversation with the mirror (technically, its talking to themselves), somehow..."
mirror _ _ =
    pure ()


