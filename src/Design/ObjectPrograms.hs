{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

module Design.ObjectPrograms
where

import Control.Lens     (view, views)
import Data.Semigroup   ((<>))
import Data.Bool        (bool)

import Dreamnet.Engine.World     (Symbol(Symbol), o_symbol, o_state)
import Dreamnet.Engine.Character (ch_name, ch_faction, ch_description)

import Design.DesignAPI
import Design.GameCharacters (characterForName)

--------------------------------------------------------------------------------

-- TODO So, I get all States data here. Maybe this is the place to feed it
--      into programs?
programForState ∷ (ObjectAPI o, Monad o) ⇒ States → InteractionType → o ()
programForState (Prop _)      it = genericProp it
programForState (Camera _ _)  it = camera it
programForState (Person _)    it = person it
programForState (Computer _)  it = computer it
programForState Door          it = door it
programForState Mirror        it = mirror it
programForState (Clothes _)   it = genericClothes it
programForState (Weapon _)    it = genericWeapon it
programForState (Ammo _)      it = genericAmmo it
programForState (Throwable _) it = genericThrowable it
programForState Empty         _  = pure ()

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
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



lock ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
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
    os   ← scanRange 8 ((==Symbol '@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    traverse isFoe viso >>= (\v → modify (\(Camera f _) → Camera f (fromIntegral v))) . length . filter id
    get >>= message . ("Camera alarm level: " <>) . (\(Camera l _) → show l)
    where
        isFoe o = (views o_state (\(Person ch) → view ch_faction ch) o /=) . (\(Camera f _) → f) <$> get
camera _ = 
    pure ()



mirror ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
mirror Examine =
    designData >>= showInfoWindow . view ch_description . characterForName "Carla" . view dd_characters
mirror Talk =
    designData >>= startConversation . characterForName "Carla" . view dd_characters
mirror _ =
    pure ()


genericProp ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
genericProp Examine =
    get >>= message . ("A " <>) . (\(Prop n) → n)
genericProp _ =
    pure ()


genericClothes ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
genericClothes Examine =
    get >>= message . ("A " <>) . (\(Clothes wi) → view wi_name wi)
genericClothes _ =
    pure ()


genericWeapon ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
genericWeapon Examine =
    get >>= message . ("Nice weapon, a " <>) . (\(Weapon wpi) → view wpi_name wpi)
genericWeapon Operate =
    message "This weapon doesn't seem to have any configs or settings."
genericWeapon Talk =
    message "You smirk as you think fondly of a vintage show you watched as a kid, with a policeman that used to talk to his gun. You don't think you're nearly cool enough to pull it off, so you put your weapon down."
genericWeapon (OperateOn s) =
    message $ "Boom boom! " <> show s <> " is dead!"
genericWeapon (OperateWith (Ammo ami)) =
    message $ "You reload the gun with ammo clip: " <> view ami_name ami
genericWeapon _ =
    pure ()


genericAmmo ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
genericAmmo Examine =
    get >>= message . ("Nice ammo, a " <>) . (\(Ammo ami) → view ami_name ami)
genericAmmo Operate =
    get >>= message . ("You change configuration of this " <>)  . (\(Ammo ami) → view ami_name ami)
genericAmmo (OperateOn s) =
    message $ "You reload the " <> show s
genericAmmo (OperateWith (Weapon wpi)) =
    message $ "While you find its really difficult and time consuming, somehow you manage to insert the clip into " <> view wpi_name wpi <> " by ramming it over the clip."
genericAmmo _ =
    pure ()
    


genericThrowable ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
genericThrowable Examine =
    get >>= message . ("Nice throwable, a " <>) . (\(Ammo ami) → view ami_name ami)
genericThrowable Operate =
    message "The thing is now armed, gulp."
genericThrowable (OperateOn (Person ch)) =
    message $ "You try to show your grenade up " <> view ch_name ch <> "'s ass."
genericThrowable (OperateWith s) =
    message $ "You try and hit the grenade with " <> show s <> ". You can't possibly imagine this being a good idea, but you keep trying nevertheless."
genericThrowable _ =
    pure ()


