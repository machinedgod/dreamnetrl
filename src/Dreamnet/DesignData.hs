{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Dreamnet.DesignData
( GameState(..)

, Faction(Faction)
, DreamnetCharacter

, DesignData
, dd_characters
, dd_defaultRedshirt
, dd_startingMap

, defaultDesignData
, characterForName

, InteractionType(..)
, States
, queryGeneric

, ObjectAPI(..)
, door
, computer
, person
, generic
, camera

, objectFromTile
, playerPerson

) where


import Control.Lens         (makeLenses, view, views)
import Control.Monad.Random (MonadRandom)
import Data.Semigroup       ((<>))
import Data.Maybe           (fromMaybe)
import Data.Bool            (bool)
import Linear               (V2)

import qualified Data.Map as M (Map, empty, singleton, fromList, (!), lookup,
                                insert)

import Dreamnet.TileMap      (Tile, t_char)
import Dreamnet.TileData     (ttype, readStringProperty, readBoolProperty,
                              readWordProperty)
import Dreamnet.World        (Object(Object), o_symbol, o_state)
import Dreamnet.Conversation (ConversationNode(ChoiceNode, TalkNode, ListenNode, End))
import Dreamnet.Character

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

newtype Faction = Faction String
                deriving (Eq, Show)

facGenpop ∷ Faction
facGenpop = Faction "genpop"
facCarla ∷ Faction
facCarla = Faction "carla"

--------------------------------------------------------------------------------

type DreamnetCharacter = Character Item ConversationNode Faction 

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String DreamnetCharacter 
    , _dd_defaultRedshirt ∷ DreamnetCharacter
    , _dd_startingMap     ∷ String
    }

makeLenses ''DesignData


defaultDesignData ∷ (MonadRandom r) ⇒ r DesignData
defaultDesignData = pure $
    DesignData {
      _dd_characters      = M.fromList $ toNamedTuple <$> characters
    , _dd_defaultRedshirt = newCharacter "?" facGenpop redshirtConvo
    , _dd_startingMap     = "res/bar"
    }
    where
        toNamedTuple = (,) <$> view ch_name <*> id

        characters ∷ [DreamnetCharacter]
        characters =
            [ newCharacter "Carla"   facCarla End
            , newCharacter "Raj"     facCarla rajConvo
            , newCharacter "Delgado" facCarla delgadoConvo

            , newCharacter "Moe"    facGenpop moeConvo
            , newCharacter "Johnny" facGenpop johnnyConvo
            , newCharacter "Sally"  facGenpop sallyConvo
            ]



characterForName ∷ DesignData → String → DreamnetCharacter
characterForName dd name =
    let maybeChar = M.lookup name (view dd_characters dd)
    in  (fromMaybe (view dd_defaultRedshirt dd) maybeChar)




--------------------------------------------------------------------------------
-- Object API and objects

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI o where
    position         ∷ o (V2 Int)
    move             ∷ V2 Int → o ()
    requestGameState ∷ GameState → o ()
    passable         ∷ o Bool
    setPassable      ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough       ∷ o Bool
    setSeeThrough    ∷ Bool → o ()
    canSee           ∷ V2 Int → o Bool
    changeChar       ∷ Char → o ()
    changeMat        ∷ String → o ()
    message          ∷ String → o ()
    put              ∷ States → o ()
    get              ∷ o States
    scanRange        ∷ Word → (Object States → Bool) → o [(V2 Int, Object States)]
    -- Keep adding primitives until you can describe all Map Objects as programs

modify ∷ (ObjectAPI o, Monad o) ⇒ (States → States) → o ()
modify f = get >>= put . f 

--------------------------------------------------------------------------------

-- TODO implement Examine as one of interaction types, through making ObjectAPI
-- able to summon UI - this can work if we just change game state
data InteractionType = Operate
                     | Talk
                     | OperateOn
                     | AiTick

--------------------------------------------------------------------------------

data States = Person  DreamnetCharacter
            | Generic (M.Map String String)
            deriving (Eq, Show) -- TODO just for Debug of UseHeld


insertGeneric ∷ String → String → States → States
insertGeneric k v (Generic m) = Generic $ M.insert k v m
insertGeneric k v _ = Generic $ M.singleton k v


queryGeneric ∷ String → States → String
queryGeneric k (Generic m) = m M.! k
queryGeneric _ _           = ""

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeChar $ bool '+' '\'' c
    --passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
    message $ "Doors are now " <>  bool "closed." "opened." c
door Talk =
    message "\"Open sesame!\" you yell, but doors are deaf and numb to your pleas."
door OperateOn =
    message "Operating door on something else..."
door AiTick =
    pure ()


computer ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
computer Operate =
    message "You can't login to this machine."
computer Talk =
    message "You'd think that in this age, computers would actually respond to voice commands. As it is, this one actually does not."
computer OperateOn =
    message "Operating computer on something else. Yikes."
computer AiTick =
    pure ()


person ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
person Operate = do
    name ← queryGeneric "name" <$> get
    message $ "Unsure yourself about what exactly you're trying to pull off, " <> name <> " meets your 'operation' attempts with suspicious look."
person Talk =
    requestGameState Conversation
person OperateOn = do
    name ← queryGeneric "name" <$> get
    message $ "You try and operate " <> name <> " on whatever. Lol."
person AiTick =
    pure ()


generic ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o () 
generic Operate =
    message "Trying to interact with whatever."
generic Talk =
    message "Trying to talk to whatever."
generic OperateOn =
    message "You try and operate whatever on whatever."
generic AiTick =
    pure ()


camera ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
camera AiTick = do
    os   ← scanRange 8 ((=='@') . view o_symbol)
    viso ← traverse (canSee . fst) os >>=
               pure . fmap (snd . fst) . filter snd . zip os
    traverse isFoe viso >>= (\v → modify (insertGeneric "level" v)) . show . length . filter id
    get >>= message . ("Camera alarm level: " <>) . queryGeneric "level"
    where
        isFoe o = (views o_state (queryGeneric "alliance") o /=) . queryGeneric "alliance" <$> get
camera _ = 
    message "That won't work."


--objectToChar (Stairs u)       = bool '<' '>' u
--objectToChar (Camera l)       = intToDigit l
--objectToChar Computer         = '$'
--objectToChar (ItemO _)        = '['


--isPassable (Person c)       = or . fmap nameMatches <$> team
--    where
--        nameMatches c' = view ch_name c == view (e_object.ch_name) c'


-- TODO this *could* all be just a single thing. Object type really does not matter here.
objectFromTile ∷ Tile → Object States
objectFromTile t@(ttype → "Base")     = Object (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t) 0                         "<base>" (Generic M.empty)
objectFromTile t@(ttype → "Door")     = Object (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) 4                         ("Just a common door. They're " <> bool "closed." "opened." (1 `readBoolProperty` t)) (Generic M.empty)
objectFromTile t@(ttype → "Stairs")   = Object (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    1                         ("If map changing would've been coded in, you would use these to go " <> bool "down." "up." (1 `readBoolProperty` t)) (Generic M.empty)
objectFromTile t@(ttype → "Prop")     = Object (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) (4 `readWordProperty` t)  ("A " <> (1 `readStringProperty` t) <> ".") (Generic M.empty)
objectFromTile t@(ttype → "Person")   = Object  '@'            "blue"                      False                    True                    3                         ("Its " <> (1 `readStringProperty` t) <> ".") (Generic $ M.fromList [ ("name", 1 `readStringProperty` t), ("alliance", 2 `readStringProperty` t)])
objectFromTile   (ttype → "Spawn")    = Object  '.'            "concrete"                  True                     True                    0                         "Spawn point. You really should not be able to examine this?" (Generic M.empty) -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
objectFromTile t@(ttype → "Camera")   = Object (view t_char t) "green light"               True                     True                    1                         "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care." (Generic $ M.fromList [ ("level", "0"), ("alliance", 1 `readStringProperty` t)])
objectFromTile t@(ttype → "Computer") = Object (view t_char t) "metal"                     False                    True                    1                         "Your machine. You wonder if Devin mailed you about the job." (Generic M.empty)
objectFromTile t@(ttype → "Item")     = Object (view t_char t) "blue plastic"              True                     True                    0                         ("A " <> (1 `readStringProperty` t) <> ".") (Generic M.empty)
objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
-- TODO Errrrrr, this should be done through the tileset???

playerPerson ∷ String → Object States
playerPerson n = Object '@' "metal" False True 3 ("Its " <> n <> ".") (Generic $ M.fromList [("name", n), ("alliance", "player")])





--------------------------------------------------------------------------------
-- Conversations
-- TODO add some conversation builder monad, to make this easier to enter
-- TODO do not really explain stuff. Let player figure it out by playing.

redshirtConvo ∷ ConversationNode
redshirtConvo = ListenNode "Beat it, lizzie!" End


johnnyConvo ∷ ConversationNode
johnnyConvo =
    ListenNode "Yea?" $
      TalkNode "Hey dude, I'm Carla" $
        ListenNode "Who gives a shit? Now would you mind, we're in the middle of something."
          End


sallyConvo ∷ ConversationNode
sallyConvo =
    ListenNode "Mmmm, hello _there_ hot stuff." $
      TalkNode "Hi, I am..." $
        ListenNode "Yeah, listen, not tonight sugar puffs, I'm in the middle of something, OK?"
          End


rajConvo ∷ ConversationNode
rajConvo =
    ListenNode "I believe in you, Cal."
        End


delgadoConvo ∷ ConversationNode
delgadoConvo =
    ListenNode "Cal, lets wrap this up quickly and get the fuck out of here."
        End


moeConvo ∷ ConversationNode
moeConvo =
    TalkNode "Hey Moe, how's it going?"
      $ ListenNode "Hey Cal, pissed and shitty, in other words - nothing new. What can I get you?" mainBranch
    where
        mainBranch = ChoiceNode [ "Ask about Devin"
                                , "News"
                                , "Smalltalk"
                                , "Trade"
                                , "Leave"
                                ]
                         [ TalkNode "Hey, um, was Devin passing by?" devinBranch
                         , TalkNode "Heard anything new regarding the biz?" bizBranch
                         , TalkNode "So... what's new with you?" smalltalkBranch
                         , TalkNode "Listen, can you get me one..." tradeBranch
                         , TalkNode "I'm actually in a bit of a hurry, so I gotta go. Later Moe..." End
                         ]
        devinBranch = ListenNode "Devin, huh? Maybe... why?"
                      $ TalkNode "Just looking for him, he said he had a job..."
                        $ ListenNode "Free piece of advice, Cal, information is a commodity here. Just this once, I'll shoot this one for free: yes, the kid passed by, he left 2 hours ago, though."
                          $ TalkNode "Hm, thanks, I guess..." mainBranch
        bizBranch = ListenNode "No, I haven't heard anything new regarding 'the biz'. What are you, a moron? Don't walk into my bar talking shit you'll get us both arrested."
                    $ TalkNode "Sorry I didn't mean to..."
                      $ ListenNode "Yeah yeah yeah spare me... if you want some icecream, the credit scanner is right there."
                        $ ChoiceNode
                            [ "Ask about ice cream"
                            , "<back>"
                            ]
                            [ TalkNode "Ice cream? You sell ice cream?"
                                $ ListenNode "Jesus you're green!"
                                  $ TalkNode "..."
                                    $ ListenNode "Listen, I'm going to go on a limb here: you want to try chocolate. That'll be 200c. Use the credit scanner over there."
                                      $ TalkNode "Right." mainBranch -- TODO pressure on and provoke him?
                            , mainBranch
                            ]
        smalltalkBranch = ListenNode "Absolutely fucking nothing! Why?"
                          $ ListenNode "<while you'd love to know intimate details about his life, Moe looks like he might pull a shotgun on you if you keep pressing>"
                            $ ChoiceNode [ "Back off"
                                         , "Press on"
                                         ]
                                         [ TalkNode "Uh, nothing, nothing, sorry..."
                                             $ ListenNode "Yeah, figured so myself. Liz." mainBranch
                                         , TalkNode "I'm interested in your private life!"
                                             $ ListenNode "Now kid... you'll drop this, and I won't blow a gaping hole between your tits, that'll be much larger than your brain. Right?"
                                               $ TalkNode "Right."
                                                $ ListenNode "Good. Was there something else? If not, I'm kinda busy, if you get my drift." mainBranch
                                         ]
        tradeBranch = ChoiceNode [ "Chocolate icecream"
                                 , "Beer"
                                 ]
                                 [ icecream
                                 , TalkNode "Can I get a beer?" beer
                                 ]
        icecream = TalkNode "I'd like some of your chocolate icecream, please."
                   $ ListenNode "Sure. 200c gets you two scoops. Now what do you want?"
                     $ ChoiceNode [ "Icecream"
                                  , "Choices?"
                                  ]
                                  [ TalkNode "Well... icecream, no?"
                                    $ ListenNode "<Moe facepalms and sighs deeply>"
                                      $ ListenNode "Let me draw it to you, lizzie. People. Contacts. Jobs, or 'biz' if you'd like. Chocolate, you get to ask about people. Per scoop."
                                        $ TalkNode "Oh."
                                          $ TalkNode "Yeah, 'oh'. Now, what will it be? I'm busy here." chocolateIcecream
                                  , TalkNode "What are my choices?"
                                    $ ListenNode "Three people you see in the bar." chocolateIcecream
                                  ]
        chocolateIcecream = ChoiceNode [ "Dude on the bar"
                                       , "Dude at the table with a girl"
                                       , "Girl at the table with the dude"
                                       , "Not right now"
                                       ]
                                       [ TalkNode "Who's the dude at the bar?"
                                         $ ListenNode "That's Delgado. He's almost a part of the inventory."
                                           $ TalkNode "Some local drunk?"
                                             $ ListenNode "Watch your mouth! He's done jobs that'd make your skin crawl, and he's done more than you'll ever live to do. He is a vet. Got fucked up. Buy him few drinks, ask him. Once he starts about that old story, he never stops." chocolateIcecream
                                       , TalkNode "Who is that dude over there at the table?"
                                         $ ListenNode "Augmented information trader from up the sprawl. Word is he took a job and ended up carrying some encrypted yak crap up in that shitbox. Now, yaks are after their property - destruction of it, if you get my drift. He's laying low, looking for some mythical hacker who can take that shit out of his head. tldr - stay away from him, unless you want to end up sliced like a deli." chocolateIcecream
                                       , TalkNode "Who is the gal?"
                                         $ ListenNode "Street muscle. She's wired up to the teeth, though, and top grade hardware, not some backstreet workshop shit. Your buddy Dev tried running a bioscan on her from the bar and failed a smoke test. Said 'er wetware fried his probe."
                                           $ TalkNode "No kidding!"
                                             $ ListenNode "No kidding."
                                               $ TalkNode "Although, knowing Devin, it just ticked him off. He has a boner for peeking under the skirt. All more interesting when its not a five minute job."
                                                 $ ListenNode "Backed off this one, though. Gal walked over to the bar, ordered him a drink. Said her metal can pin ice breakers to an inch within 5 mile radius. Asked Dev politely to have a drink and asked if he likes olives. I put them in just to piss him, so the kid's looking at me and saying 'Um, no, I do not'."
                                                   $ ListenNode "So our gal over there says 'Well, please allow me', then slides some fred krueger slash wolverine steel from her finger. Both me and the kid gaping eyes, she pricks the olive and eats it, then walks away."
                                                     $ TalkNode "Shit!"
                                                       $ ListenNode "Her price range must be through the roof, but the jacko over there can probably handle it." chocolateIcecream
                                       , TalkNode "Actually, not right now. I wanted to ask..."
                                         $ mainBranch
                                       ]

        beer = ListenNode "Sure. Skinny, fat or extra fat?"
               $ ChoiceNode [ "Skinny"
                            , "Fat"
                            , "Extra fat"
                            , "What?"
                            , "Changed my mind"
                            ]
                            [ TalkNode "Skinny, please."
                              $ ListenNode "Retro vibes today, eh? Sure, sure, skinny it is." mainBranch
                            , TalkNode "Fat, please"
                              $ ListenNode "One fat draft, coming right up." mainBranch
                            , TalkNode "Extra fat, please"
                              $ ListenNode "Er, you sure? Its not even noon. Aiming to booze away until weekend?"
                                $ ChoiceNode [ "Yes"
                                             , "No"
                                             ]
                                             [ TalkNode "Yeah, give me that extra fat."
                                               $ ListenNode "Sure, sure. Extra fat coming right up." mainBranch
                                             , TalkNode "Actually, you're right, I changed my mind." stillWantBeer
                                             ]
                            , TalkNode "Skinny? Fat? What do you mean?"
                              $ ListenNode "Laced beer, Cal! Synthemesc. You've been living under the rock?"
                                $ TalkNode "Yeah... sorry... I don't know what's up with me today..." stillWantBeer
                            ]
        stillWantBeer = ListenNode "Still want beer?"
                        $ ChoiceNode [ "Yeah"
                                     , "Nope"
                                     ]
                                     [ beer
                                     , mainBranch
                                     ]
