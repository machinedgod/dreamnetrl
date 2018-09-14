{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Design.ObjectPrograms
where

import Control.Lens       (view, (^.))
import Control.Monad.Free (Free)
import Data.Semigroup     ((<>))
import Data.Bool          (bool)

import Dreamnet.Engine.Object
import Dreamnet.Engine.World
import Dreamnet.Engine.Character
import Dreamnet.Engine.Conversation

import Dreamnet.Game
import Dreamnet.ComputerModel

--------------------------------------------------------------------------------

type DreamnetObjectAPI s o = (ObjectAPI o, Monad o, ObjectAPIState o ~ s, ObjectAPIConversation o ~ Free (ConversationF s))

--------------------------------------------------------------------------------

genericProp ∷ (DreamnetObjectAPI s o) ⇒ String → String → InteractionType s → o ()
genericProp _ d Examine =
    message d
genericProp _ _ _ =
    pure ()



genericClothes ∷ (DreamnetObjectAPI s o) ⇒ WearableItem i → InteractionType s → o ()
genericClothes wi Examine =
    message ("A " <> view wi_name wi)
genericClothes _ _ =
    pure ()



genericWeapon ∷ (DreamnetObjectAPI States o) ⇒ WeaponItem → InteractionType States → o ()
genericWeapon wpi Examine =
    message (wpi ^. wpi_description)
genericWeapon wpi Operate =
    message ("This weapon has these settings: " <> show (wpi ^. wpi_settings))
genericWeapon _ Talk =
    message "You smirk as you think fondly of a vintage show you watched as a kid, starring a policeman that used to talk to his gun. You don't think you're nearly cool enough to pull it off, so you put your weapon down."
genericWeapon _ (OperateOn s) = do
    mv ← findObject s
    case mv of
        Just v → do
            uncurry removeObject v
            message ("Boom boom! " <> show s <> " is dead!")
        Nothing →
            message "Well, there's nothing there?"
genericWeapon wpi (OperateWith (Ammo ami)) =
    message ("You reload the " <> view wpi_name wpi <> " with ammo clip: " <> view ami_name ami)
genericWeapon _ _ =
    pure ()



genericAmmo ∷ (DreamnetObjectAPI States o) ⇒ AmmoItem → InteractionType States → o ()
genericAmmo ami Examine =
    message ("Nice ammo, a " <> view ami_name ami)
genericAmmo ami Operate =
    message ("You change configuration of this " <> view ami_name ami)
genericAmmo _ (OperateOn (Weapon wpi)) =
    message ("You reload the " <> view wpi_name wpi)
genericAmmo _ (OperateWith (Weapon wpi)) =
    message ("While you find its really difficult and time consuming, somehow you manage to insert the clip into " <> view wpi_name wpi <> " by ramming it over the clip.")
genericAmmo _ _ =
    pure ()



genericThrowable ∷ (DreamnetObjectAPI States o) ⇒ ThrownWeaponItem → InteractionType States → o ()
genericThrowable twi Examine =
    message ("Nice throwable, a " <> view twi_name twi)
genericThrowable twi Operate =
    message ("The " <> view twi_name twi <> " is now armed, gulp.")
genericThrowable twi (OperateOn (Person ch)) =
    message ("You try to shove your " <> view twi_name twi <> " up " <> view ch_name ch <> "'s ass.")
genericThrowable twi (OperateWith s) =
    message ("You try and hit the " <> view twi_name twi <> " with " <> show s <> ". You can't possibly imagine this being a good idea, but you keep trying nevertheless.")
genericThrowable _ _ =
    pure ()



genericConsumable ∷ (DreamnetObjectAPI States o) ⇒ ConsumableItem → DreamnetCharacter → InteractionType States → o ()
genericConsumable ci _ Examine =
    message ("A " <> view ci_name ci)
genericConsumable ci ch Operate =
    message (view ch_name ch <> " eats/drinks " <> view ci_name ci)
genericConsumable _ _ _ =
    pure ()

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
genericDoor ∷ (DreamnetObjectAPI States o) ⇒ InteractionType States → o ()
genericDoor Examine =
    passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
genericDoor Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeSymbol $ bool (Symbol '+') (Symbol '/') c
genericDoor Talk =
    doTalk $ do
        talk 0   "Open sesame!"
        talk 1   "..."
        describe "Quite as expected, doors are deaf to your pleas."
genericDoor _ =
    pure ()



lock ∷ (DreamnetObjectAPI States o) ⇒ InteractionType States → o ()
lock Examine =
    message "Its a lock allright"
lock Operate =
    message "Absentmindedly, you try and use your finger as a key, completely oblivious to the fact that this wouldn't work even back when your grandad was a little boy."
lock Talk =
    message "\"Unlock, NOW!\""
lock (OperateWith (Prop "key" _)) =
    message "Unlocked!"
lock (OperateWith _) =
    message "That won't work."
lock _ =
    pure ()



genericComputer ∷ (DreamnetObjectAPI States o) ⇒ ComputerData → InteractionType States → o ()
genericComputer _ Examine =
    message "Screen, keyboard, cartridge connector.. yeah, pretty standard machine there."
genericComputer _ Operate =
    operateComputer
genericComputer _ Talk =
    doTalk $ do
        talk 0   "*khm* LOGIN - CARLA..."
        talk 1   "<electric buzz>"
        talk 0   "I said, LOGIN - CARLA."
        talk 1   "<electric buzz>"
        describe "It doesn't take too long, but you figure out that this specific computer does not react to voice commands."
genericComputer _ _ =
    pure ()



genericPerson ∷ (DreamnetObjectAPI States o) ⇒ DreamnetCharacter → InteractionType States → o ()
genericPerson ch Examine =
    message (view ch_description ch)
genericPerson ch Operate =
    message ("Even you yourself are unsure about what exactly you're trying to pull off, but " <> view ch_name ch <> " meets your 'operation' attempts with suspicious look.")
genericPerson ch Talk =
    doTalk (view ch_conversation ch)
genericPerson _ _ =
    pure ()



-- TODO mixes general Object knowledge with States knowledge. This is an issue!
genericCamera ∷ (DreamnetObjectAPI States o) ⇒ Faction → Word → InteractionType s → o ()
genericCamera _ _ Examine =
    message "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care."
genericCamera f _ Operate = do
    os   ← scanRange 8 (\case
                            Person _ → True
                            _        → False)
    viso ← fmap (snd . fst) . filter snd . zip os <$> traverse (canSee . fst) os
    -- traverse isFoe viso >>= (\v → modifyState (\(Camera f _) → Camera f (fromIntegral v))) . length -- . filter id
    message $ "Camera alarm level: " <> show (length $ isFoe <$> viso)
    where
        isFoe (Person ch) = f /= view ch_faction ch -- TODO Fix this with maybe monad
        isFoe _           = False
genericCamera _ _ _ =
    pure ()



mirror ∷ (DreamnetObjectAPI States o) ⇒ DreamnetCharacter → InteractionType States → o ()
mirror ch Examine =
    doTalk (describe (view ch_description ch))
mirror ch Talk =
    doTalk (view ch_conversation ch)
mirror _ _ =
    pure ()

