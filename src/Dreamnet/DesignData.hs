{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
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
--, characterForName

, InteractionType(..)
, States(..) -- If I close this, then all conversation code needs to be handled here

, ObjectAPI(..)
, door
, computer
, person
, camera

, objectFromTile
, playerPerson
, programForObject

) where


import Control.Lens         (makeLenses, view, views)
import Control.Monad.Random (MonadRandom)
import Data.Semigroup       ((<>))
import Data.Bool            (bool)
import Data.List            (intercalate)
import Linear               (V2)

import qualified Data.Map as M (Map, fromList)

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
    , _dd_defaultRedshirt = newCharacter "?" redshirtDesc facGenpop redshirtConvo
    , _dd_startingMap     = "res/bar"
    }
    where
        toNamedTuple = (,) <$> view ch_name <*> id

        characters ∷ [DreamnetCharacter]
        characters =
            [ newCharacter "Carla"   carlaDesc   facCarla End
            , newCharacter "Raj"     rajDesc     facCarla rajConvo
            , newCharacter "Delgado" delgadoDesc facCarla delgadoConvo

            , newCharacter "Moe"    moeDesc    facGenpop moeConvo
            , newCharacter "Johnny" johnnyDesc facGenpop johnnyConvo
            , newCharacter "Sally"  sallyDesc  facGenpop sallyConvo
            ]



--characterForName ∷ DesignData → String → DreamnetCharacter
--characterForName dd name =
--    let maybeChar = M.lookup name (view dd_characters dd)
--    in  (fromMaybe (view dd_defaultRedshirt dd) maybeChar)




--------------------------------------------------------------------------------
-- Object API and objects

-- Note: remember not to add GAME actions or PLAYER actions, just WORLD actions
class ObjectAPI o where
    position          ∷ o (V2 Int)
    move              ∷ V2 Int → o ()
    showInfoWindow    ∷ String → o ()
    startConversation ∷ DreamnetCharacter → o ()
    passable          ∷ o Bool
    setPassable       ∷ Bool → o () -- Creates a state, creates and object. NO!
    seeThrough        ∷ o Bool
    setSeeThrough     ∷ Bool → o ()
    canSee            ∷ V2 Int → o Bool
    changeChar        ∷ Char → o ()
    changeMat         ∷ String → o ()
    message           ∷ String → o ()
    put               ∷ States → o ()
    get               ∷ o States
    scanRange         ∷ Word → (Object States → Bool) → o [(V2 Int, Object States)]
    -- Keep adding primitives until you can describe all Map Objects as programs

modify ∷ (ObjectAPI o, Monad o) ⇒ (States → States) → o ()
modify f = get >>= put . f 

--------------------------------------------------------------------------------

-- TODO implement Examine as one of interaction types, through making ObjectAPI
-- able to summon UI - this can work if we just change game state
data InteractionType = Examine
                     | Operate
                     | Talk
                     | OperateOn
                     | AiTick

--------------------------------------------------------------------------------

data States = Door
            | Camera  Faction Word
            | Person  DreamnetCharacter
            | Empty
            deriving (Eq, Show) -- TODO just for Debug of UseHeld


-- 1) This *could* all be just a single thing. Object type really does not matter here.
-- 2) Actually, it does, because Object carries a specific state, later used by object programs
objectFromTile ∷ Tile → Object States
objectFromTile t@(ttype → "Base")     = Object (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t) 0                         Empty
objectFromTile t@(ttype → "Door")     = Object (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) 4                         Door
objectFromTile t@(ttype → "Stairs")   = Object (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    1                         Empty
objectFromTile t@(ttype → "Prop")     = Object (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) (4 `readWordProperty` t)  Empty
objectFromTile t@(ttype → "Person")   = Object  '@'            "blue"                      False                    True                    3                         (Person $ let n = 1 `readStringProperty` t in newCharacter n (descriptionForName n) (Faction $ 2 `readStringProperty` t) (conversationForName n))
objectFromTile   (ttype → "Spawn")    = Object  '.'            "concrete"                  True                     True                    0                         Empty -- TODO shitty hardcoding, spawns should probably be generalized somehow!) 
objectFromTile t@(ttype → "Camera")   = Object (view t_char t) "green light"               True                     True                    1                         (Camera (Faction $ 1 `readStringProperty` t) 0)
objectFromTile t@(ttype → "Computer") = Object (view t_char t) "metal"                     False                    True                    1                         Empty
objectFromTile t@(ttype → "Item")     = Object (view t_char t) "blue plastic"              True                     True                    0                         Empty
objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
-- TODO Errrrrr, this should be done through the tileset???

playerPerson ∷ String → Object States
playerPerson n = Object '@' "metal" False True 3 (Person $ newCharacter n (descriptionForName n) facCarla (conversationForName n))


programForObject ∷ (ObjectAPI o, Monad o) ⇒ Object States → InteractionType → o ()
programForObject (view o_state → Door)         = door
programForObject (view o_state → (Camera _ _)) = camera
programForObject (view o_state → (Person _))   = person
programForObject _                             = const (pure ())

--------------------------------------------------------------------------------

-- | Toggles collision and character on interaction
door ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
door Examine =
    passable >>= message . ("Just a common door. They're " <>) . bool "closed." "opened."
door Operate = do
    c ← passable >>= setPassable . not >> passable
    setSeeThrough c
    changeChar $ bool '+' '\'' c
door Talk =
    message "\"Open sesame!\""
door _ =
    pure ()


computer ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
computer Examine =
    message "Screen, keyboard, cartridge connector.. yeah, pretty standard machine there."
computer Operate =
    message "<IMPLEMENT ME>"
computer Talk =
    message "*khm* \"LOGIN - CARLA\"..."
computer _ =
    pure ()


person ∷ (ObjectAPI o, Monad o) ⇒ InteractionType → o ()
person Examine = do
    desc ← (\(Person ch) → view ch_description ch) <$> get
    showInfoWindow desc
person Operate = do
    name ← (\(Person ch) → view ch_name ch) <$> get
    message $ "Even you yourself are unsure about what exactly you're trying to pull off, but " <> name <> " meets your 'operation' attempts with suspicious look."
person Talk = do
    ch ← (\(Person ch) → ch) <$> get
    startConversation ch
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


--objectToChar (Stairs u)       = bool '<' '>' u
--objectToChar (Camera l)       = intToDigit l
--objectToChar Computer         = '$'
--objectToChar (ItemO _)        = '['


--isPassable (Person c)       = or . fmap nameMatches <$> team
--    where
--        nameMatches c' = view ch_name c == view (e_object.ch_name) c'





--------------------------------------------------------------------------------
-- People
-- TODO add some conversation builder monad, to make this easier to enter
-- TODO do not really explain stuff. Let player figure it out by playing.

descriptionForName ∷ String → String
descriptionForName "Carla"   = carlaDesc
descriptionForName "Raj"     = rajDesc
descriptionForName "Delgado" = delgadoDesc

descriptionForName "Johnny"  = johnnyDesc
descriptionForName "Sally"   = sallyDesc
descriptionForName "Moe"     = moeDesc
descriptionForName _         = redshirtDesc


conversationForName ∷ String → ConversationNode
conversationForName "Carla"   = carlaConvo
conversationForName "Raj"     = rajConvo
conversationForName "Delgado" = delgadoConvo

conversationForName "Johnny"  = johnnyConvo
conversationForName "Sally"   = sallyConvo
conversationForName "Moe"     = moeConvo
conversationForName _         = redshirtConvo


--------------------------------------------------------------------------------

-- What happens when Carla looks in the mirror
carlaDesc ∷ String
carlaDesc = intercalate "\n"
    [ "Scrawny, beautiful, capable. You know you are. You also hope no one figures out quickly enough that you are."
    , "In this business, being considered green has certain advantages."
    ]

-- Few conversations of Carla giving herself moral support
carlaConvo ∷ ConversationNode
carlaConvo = TalkNode "Hi." End



rajDesc ∷ String
rajDesc = intercalate "\n"
    [ "Tiny, skinny and looking like she'd bolt in a second if she could."
    , "Poor Raj, you think, wondering if she understands how much you feel out of place as well. Looking at her cute, young face, you find that she's beautiful in her own way."
    , " If things didn't happen for her the way they did, it'd be hard to imagine her even throwing a look down your way, if you'd happen to pass her by in the street."
    ]

rajConvo ∷ ConversationNode
rajConvo =
    ListenNode "I believe in you, Cal."
        End


delgadoDesc ∷ String
delgadoDesc = intercalate "\n"
    [ "Scrawny, scarred and tough as nails."
    , "When you look in his eyes without giving a tell, you see that there's unspeakable depth and pain, but as soon as he catches your eyes, an imaginary door comes slamming down, the depth disappears and and Delgado's face takes that of the one everyone knows the best: mean SOB."
    , "You wonder if this is a conscious effort to maintain illusion of superiority, or a subconscious defense mechanism. Either way, there's certain type of abstract beauty in there, and somewhere very, very far away in the depths of your own brain - you find yourself thinking about how does Major Phillipe Delgado look naked."
    ]

delgadoConvo ∷ ConversationNode
delgadoConvo =
    ListenNode "Cal, lets wrap this up quickly and get the fuck out of here."
        End

--------------------------------------------------------------------------------

redshirtDesc ∷ String
redshirtDesc = "You never saw this person in your life."

redshirtConvo ∷ ConversationNode
redshirtConvo = ListenNode "Beat it, lizzie!" End



sallyDesc ∷ String
sallyDesc = "Slim, well-built and gorgeous, this woman looks like she's packing serious hardware. Her scrawny short black hair falls just short of shiny mirrors where her eyes are supposed to be. Probably HUD augments."

sallyConvo ∷ ConversationNode
sallyConvo =
    ListenNode "Mmmm, hello _there_ hot stuff." $
      TalkNode "Hi, I am..." $
        ListenNode "Yeah, listen, not tonight sugar puffs, I'm in the middle of something, OK?"
          End



johnnyDesc ∷ String
johnnyDesc = "If he'd have asian facial features, he'd look like a textbook sarariman."

johnnyConvo ∷ ConversationNode
johnnyConvo =
    ListenNode "Yea?" $
      TalkNode "Hey dude, I'm Carla" $
        ListenNode "Who gives a shit? Now would you mind, we're in the middle of something."
          End



moeDesc ∷ String
moeDesc = intercalate "\n"
    [ "You never met anyone whose name fit them better. Black side hair and shiny dome, white sleeveless shirt covered with beer and fat stains, covering his giant belly - somehow, you are *sure* Moe would present a formidable opponent in close quarters."
    , "Its common knowledge that he used to run, but Devin told you bits and pieces of his past that make you feel tremendous respect towards this relic of the old age."
    ]

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
