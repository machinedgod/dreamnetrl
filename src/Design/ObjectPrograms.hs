{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Design.ObjectPrograms
where

import Control.Lens     (view, views, (^.))
import Data.Semigroup   ((<>))
import Data.Bool        (bool)
import Data.Functor     (($>))

import Dreamnet.Engine.World
import Dreamnet.Engine.Character
import Dreamnet.Engine.Conversation
import Dreamnet.Engine.ConversationMonad

import Design.ComputerModel
import Design.DesignAPI

--------------------------------------------------------------------------------

genericProp ∷ (ObjectAPI s o, Monad o) ⇒ String → InteractionType s → o GameState
genericProp n Examine =
    message ("A " <> n) $> Normal
genericProp _ _ =
    pure Normal



genericClothes ∷ (ObjectAPI s o, Monad o) ⇒ WearableItem i → InteractionType s → o GameState
genericClothes wi Examine =
    message ("A " <> view wi_name wi) $> Normal
genericClothes _ _ =
    pure Normal



genericWeapon ∷ (ObjectAPI States o, Monad o) ⇒ WeaponItem → InteractionType States → o GameState
genericWeapon wpi Examine =
    message (wpi ^. wpi_description) $> Normal
genericWeapon wpi Operate =
    message ("This weapon has these settings: " <> show (wpi ^. wpi_settings)) $> Normal
genericWeapon _ Talk =
    message "You smirk as you think fondly of a vintage show you watched as a kid, with a policeman that used to talk to his gun. You don't think you're nearly cool enough to pull it off, so you put your weapon down." $> Normal
genericWeapon _ (OperateOn s) =
    message ("Boom boom! " <> show s <> " is dead!") $> Normal
genericWeapon wpi (OperateWith (Ammo ami)) =
    message ("You reload the " <> view wpi_name wpi <> " with ammo clip: " <> view ami_name ami) $> Normal
genericWeapon _ _ =
    pure Normal



genericAmmo ∷ (ObjectAPI States o, Monad o) ⇒ AmmoItem → InteractionType States → o GameState
genericAmmo ami Examine =
    message ("Nice ammo, a " <> view ami_name ami) $> Normal
genericAmmo ami Operate =
    message ("You change configuration of this " <> view ami_name ami) $> Normal
genericAmmo _ (OperateOn (Weapon wpi)) =
    message ("You reload the " <> view wpi_name wpi) $> Normal
genericAmmo _ (OperateWith (Weapon wpi)) =
    message ("While you find its really difficult and time consuming, somehow you manage to insert the clip into " <> view wpi_name wpi <> " by ramming it over the clip.") $> Normal
genericAmmo _ _ =
    pure Normal
    


genericThrowable ∷ (ObjectAPI States o, Monad o) ⇒ ThrownWeaponItem → InteractionType States → o GameState
genericThrowable twi Examine =
    message ("Nice throwable, a " <> view twi_name twi) $> Normal
genericThrowable twi Operate =
    message ("The " <> view twi_name twi <> " is now armed, gulp.") $> Normal
genericThrowable twi (OperateOn (Person ch)) =
    message ("You try to shove your " <> view twi_name twi <> " up " <> view ch_name ch <> "'s ass.") $> Normal
genericThrowable twi (OperateWith s) =
    message ("You try and hit the " <> view twi_name twi <> " with " <> show s <> ". You can't possibly imagine this being a good idea, but you keep trying nevertheless.") $> Normal
genericThrowable _ _ =
    pure Normal



genericConsumable ∷ (ObjectAPI States o, Monad o) ⇒ ConsumableItem → DreamnetCharacter → InteractionType States → o GameState
genericConsumable ci _ Examine =
    message ("A " <> view ci_name ci) $> Normal
genericConsumable ci ch Operate =
    message (view ch_name ch <> " eats/drinks " <> view ci_name ci) $> Normal
genericConsumable _ _ _ =
    pure Normal

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI s o, Monad o) ⇒ InteractionType s → o GameState
door Examine =
    passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened." >> pure Normal
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeSymbol $ bool (Symbol '+') (Symbol '/') c
    pure Normal
door Talk =
    pure $ Conversation $
        runConversationF_temp [ "Carla"
                              , "Door" 
                              ]
                              doorConvo
    where
        doorConvo = do
            talk 0   "Open sesame!"
            reply    "..."
            describe "Quite as expected, doors are deaf to your pleas."
door _ =
    pure Normal



lock ∷ (ObjectAPI States o, Monad o) ⇒ InteractionType States → o ()
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



computer ∷ (ObjectAPI s o, Monad o) ⇒ ComputerData → InteractionType s → o GameState
computer _ Examine =
    message "Screen, keyboard, cartridge connector.. yeah, pretty standard machine there." $> Normal
computer cd Operate =
    position >>= \p → pure (ComputerOperation (p, 1) cd)
computer _ Talk =
    pure $ Conversation $
        runConversationF_temp [ "Carla"
                              , "Computer" 
                              ]
                              computerConvo
    where
        computerConvo = do
            talk 0   "*khm* LOGIN - CARLA..."
            reply    "<electric buzz>"
            reply    "I said, LOGIN - CARLA."
            reply    "<electric buzz>"
            describe "It doesn't take too long, but you figure out that this specific computer does not react to voice commands."
computer _ _ =
    pure Normal



person ∷ (ObjectAPI s o, Monad o) ⇒ DreamnetCharacter → InteractionType s → o GameState
person ch Examine =
    message (view ch_description ch) $> Normal
person ch Operate =
    message ("Even you yourself are unsure about what exactly you're trying to pull off, but " <> view ch_name ch <> " meets your 'operation' attempts with suspicious look.") $> Normal
person ch Talk =
    pure $ Conversation $
        runConversationF_temp [ "Carla_HARDCODED"
                              , view ch_name ch
                              ]
                              (view ch_conversation ch)
person _ _ =
    pure Normal



-- TODO mixes general Object knowledge with States knowledge. This is an issue!
camera ∷ (ObjectAPI States o, Monad o) ⇒ Faction → Word → InteractionType s → o GameState
camera _ _ Examine =
    message "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care." $> Normal
camera f _ Operate = do
    os   ← scanRange 8 ((==Symbol '@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    -- traverse isFoe viso >>= (\v → modifyState (\(Camera f _) → Camera f (fromIntegral v))) . length -- . filter id
    message $ "Camera alarm level: " <> show (length $ isFoe <$> viso)
    pure Normal
    where
        isFoe o = f /= views o_state (\(Person ch) → view ch_faction ch) o -- TODO Fix this with maybe monad
camera _ _ _ = 
    pure Normal



mirror ∷ (ObjectAPI s o, Monad o) ⇒ DreamnetCharacter → InteractionType s → o GameState
mirror ch Examine =
    pure $ Conversation $
        runConversationF_temp [ view ch_name ch
                              , "Mirror " <> view ch_name ch
                              ]
                              (describe (view ch_description ch))
    --message $ view ch_description ch
mirror ch Talk =
    pure $ Conversation $
        runConversationF_temp [ view ch_name ch
                              , "Mirror " <> view ch_name ch
                              ]
                              (view ch_conversation ch)
mirror _ _ =
    pure Normal


