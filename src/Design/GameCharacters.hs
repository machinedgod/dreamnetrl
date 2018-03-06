{-# LANGUAGE UnicodeSyntax #-}

module Design.GameCharacters
( redshirt
, characters
, characterForName
, characterDictionary

, randomizeStats
)
where


import Control.Lens         (view, (.~))
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List            (intercalate)
import Data.Maybe           (fromMaybe)
import Data.Semigroup       ((<>))

import qualified Data.Map as M (Map, fromList, lookup)

import Dreamnet.Conversation
import Dreamnet.Character

import Design.DesignAPI

--------------------------------------------------------------------------------

randomizeStats ∷ (MonadRandom r) ⇒ DreamnetCharacter → r DreamnetCharacter
randomizeStats ch = do
    rndMelee         ← MeleeCombatSkills   <$> pure 0 <*> r <*> r <*> r <*> r <*> r
    rndRanged        ← RangedCombatSkills  <$> pure 0 <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r
    rndThrowing      ← ThrowingSkills      <$> pure 0 <*> r <*> r <*> r <*> r
    rndEngineering   ← EngineeringSkills   <$> pure 0 <*> r <*> r <*> r <*> r <*> r     
    rndCommunication ← CommunicationSkills <$> pure 0 <*> r <*> r <*> r <*> r <*> r <*> r
    rndInfiltration  ← InfiltrationSkills  <$> pure 0 <*> r <*> r <*> r <*> r

    pure . (ch_meleeCombat   .~ rndMelee)
         . (ch_rangedCombat  .~ rndRanged)
         . (ch_throwing      .~ rndThrowing)
         . (ch_engineering   .~ rndEngineering)
         . (ch_communication .~ rndCommunication)
         . (ch_infiltration  .~ rndInfiltration)
         $ ch
    where
        r = getRandomR (1, 99)
    

--------------------------------------------------------------------------------

facGenpop ∷ Faction
facGenpop = Faction "genpop"

facCarla ∷ Faction
facCarla = Faction "carla"

--------------------------------------------------------------------------------

redshirt ∷ DreamnetCharacter
redshirt = newCharacter "?" "?" "?" desc facGenpop convo
    where
        desc  = "You never saw this person in your life."
        convo = talk 1 "Beat it, lizzie!"


characters ∷ [DreamnetCharacter]
characters =
    [ carla, hideo, devin, delgado, raj, nova, cobra, kman
    , moe, johnny, sally
    ]


characterDictionary ∷ [DreamnetCharacter] → M.Map String DreamnetCharacter
characterDictionary chs = M.fromList $ toNamedTuple <$> chs
    where
        toNamedTuple = (,) <$> view ch_name <*> id


characterForName ∷ String → M.Map String DreamnetCharacter → DreamnetCharacter
characterForName n = fromMaybe (error "No character with that name!") . M.lookup n

--------------------------------------------------------------------------------

carla ∷ DreamnetCharacter
carla = newCharacter "Carla" "D'Addario" "La Piovra" desc facCarla convo
    where
        desc = intercalate "\n"
            [ "Scrawny, beautiful, capable. You know you are. You also hope no one figures out quickly enough that you are."
            , "In this business, being considered green has certain advantages."
            ]
        convo = talk 0 "Hi."


hideo ∷ DreamnetCharacter
hideo = newCharacter "Hideo" "Hattori" "Tetsuo" desc facCarla convo
    where
        desc = "<DESCRIPTION MISSING>"
        convo = talk 1 "I believe in you, Cal."


devin ∷ DreamnetCharacter
devin = newCharacter "Devin" "Dorsett" "570rm" desc facCarla convo
    where
        desc = "<DESCRIPTION MISSING>"
        convo = talk 1 "I believe in you, Cal."


delgado ∷ DreamnetCharacter
delgado = newCharacter "Phillipe" "Delgado" "Sarge" desc facCarla convo
    where
        desc = intercalate "\n"
            [ "Scrawny, scarred and tough as nails."
            , "When you look in his eyes without giving a tell, you see that there's unspeakable depth and pain, but as soon as he catches your eyes, an imaginary door comes slamming down, the depth disappears and and Delgado's face takes that of the one everyone knows the best: mean SOB."
            , "You wonder if this is a conscious effort to maintain illusion of superiority, or a subconscious defense mechanism. Either way, there's certain type of abstract beauty in there, and somewhere very, very far away in the depths of your own brain - you find yourself thinking about how does Major Phillipe Delgado look naked."
            ]
        convo = talk 1 "Cal, lets wrap this up quickly and get the fuck out of here."


raj ∷ DreamnetCharacter
raj = newCharacter "Qaayenaat" "Rajan" "Raj" desc facCarla convo
    where
        desc = intercalate "\n"
            [ "Tiny, skinny and looking like she'd bolt in a second if she could."
            , "Poor Raj, you think, wondering if she understands how much you feel out of place as well. Looking at her cute, young face, you find that she's beautiful in her own way."
            , " If things didn't happen for her the way they did, it'd be hard to imagine her even throwing a look down your way, if you'd happen to pass her by in the street."
            ]
        convo = talk 1 "I believe in you, Cal."


nova ∷ DreamnetCharacter
nova = newCharacter "Annabelle" "Jenkins" "Nova" desc facCarla convo
    where
        desc = "<DESCRIPTION MISSING>"
        convo = talk 1 "I believe in you, Cal."


cobra ∷ DreamnetCharacter
cobra = newCharacter "Eduarda" "Ribeiro" "Cobra" desc facCarla convo
    where
        desc = "<DESCRIPTION MISSING>"
        convo = talk 1 "I believe in you, Cal."


kman ∷ DreamnetCharacter
kman = newCharacter "Kelly" "Lafleur" "K-man" desc facCarla convo
    where
        desc = "<DESCRIPTION MISSING>"
        convo = talk 1 "I believe in you, Cal."

--------------------------------------------------------------------------------

johnny ∷ DreamnetCharacter
johnny = newCharacter "Johnny" "M." "Jockey" desc facGenpop convo
    where
        desc =
            "If he'd have asian facial features, he'd look like a textbook sarariman."
        convo = do
            talk 1 "Yea?"
            name 0 >>= reply . ("Hey dude, I'm " <>)
            reply "Who gives a shit? Now would you mind, we're in the middle of something."


sally ∷ DreamnetCharacter
sally = newCharacter "Sally" "S." "M" desc facGenpop convo
    where
        desc =
            "Slim, well-built and gorgeous, this woman looks like she's packing serious hardware. Her scrawny short black hair falls just short of shiny mirrors where her eyes are supposed to be. Probably HUD augments."
        convo = do
            talk 1 "Mmmm, hello _there_ hot stuff."
            reply "Hi, I am..."
            reply "Yeah, listen, not tonight sugar puffs, I'm in the middle of something, OK?"


moe ∷ DreamnetCharacter
moe = newCharacter "Moe" "Sarlac" "Trigger" desc facGenpop convo
    where
        desc = intercalate "\n"
            [ "You never met anyone whose name fit them better. Black side hair and shiny dome, white sleeveless shirt covered with beer and fat stains, covering his giant belly - somehow, you are *sure* Moe would present a formidable opponent in close quarters."
            , "Its common knowledge that he used to run, but Devin told you bits and pieces of his past that make you feel tremendous respect towards this relic of the old age."
            ]
        convo = do
            talk 0 "Hey Moe, how's it going?"
            reply "Hey Cal, pissed and shitty, in other words - nothing new. What can I get you?"
            mainBranch
        mainBranch = choice_
            [ "Ask about Devin" |=> devinBranch
            , "News"            |=> bizBranch
            , "Smalltalk"       |=> smalltalkBranch
            , "Trade"           |=> tradeBranch
            , "Leave"           |=> talk 0 "I'm actually in a bit of a hurry, so I gotta go. Later Moe..."
            ]
        devinBranch = do
            talk 0 "Hey, um, was Devin passing by?" 
            reply "Devin, huh? Maybe... why?"
            reply "Just looking for him, he said he had a job..."
            reply "Free piece of advice, Cal, information is a commodity here. Just this once, I'll shoot this one for free: yes, the kid passed by, he left 2 hours ago, though."
            reply "Hm, thanks, I guess..."
            mainBranch -- TODO willr ecurse infinitely when monad is ran!
        bizBranch = do
            talk 0 "Heard anything new regarding the biz?" 
            reply "No, I haven't heard anything new regarding 'the biz'. What are you, a moron? Don't walk into my bar talking shit you'll get us both arrested."
            reply "Sorry I didn't mean to..."
            reply "Yeah yeah yeah spare me... if you want some icecream, the credit scanner is right there."
            choice_ [ "Ask about ice cream" |=> do
                                                talk 0 "Ice cream? You sell ice cream?"
                                                reply "Jesus you're green!"
                                                reply "..."
                                                reply "Listen, I'm going to go on a limb here: you want to try chocolate. That'll be 200c. Use the credit scanner over there."
                                                reply "Right."
                                                mainBranch -- TODO pressure on and provoke him?
                    , "<back>"              |=> mainBranch -- TODO WILL RECURSE INFINITELY!
                    ]
        smalltalkBranch = do
            talk 0 "So... what's new with you?"
            reply "Absolutely fucking nothing! Why?"
            describe "While you'd love to know intimate details about his life, Moe looks like he might pull a shotgun on you if you keep pressing"
            choice_ [ "Back off" |=> do
                                     talk 0 "Uh, nothing, nothing, sorry..."
                                     reply "Yeah, figured so myself. Liz."
                                     mainBranch
                    , "Press on" |=> do
                                     talk 0 "I'm interested in your private life!"
                                     reply "Now kid... you'll drop this, and I won't blow a gaping hole between your tits, that'll be much larger than your brain. Right?"
                                     reply "Right."
                                     reply "Good. Was there something else? If not, I'm kinda busy, if you get my drift."
                                     mainBranch
                    ]
                            
        tradeBranch = do
            talk 0 "Listen, can you get me one..."
            choice_ [ "Chocolate icecream" |=> icecream
                    , "Beer"               |=> beer
                    ]
        icecream = do
            talk 0 "I'd like some of your chocolate icecream, please."
            reply "Sure. 200c gets you two scoops. Now what do you want?"
            choice_ [ "Icecream" |=> do
                                     talk 0 "Well... icecream, no?"
                                     describe "<Moe facepalms and sighs deeply>"
                                     reply "Let me draw it to you, lizzie. People. Contacts. Jobs, or 'biz' if you'd like. Chocolate, you get to ask about people. Per scoop."
                                     reply "Oh."
                                     reply "Yeah, 'oh'. Now, what will it be? I'm busy here."
                                     chocolateIcecream
                    , "Choices?" |=> do
                                     talk 0 "What are my choices?"
                                     reply "Three people you see in the bar." 
                                     chocolateIcecream
                    ]
        chocolateIcecream = do
            choice_ [ "Dude on the bar" |=> do
                                            talk 0 "Who's the dude at the bar?"
                                            reply "That's Delgado. He's almost a part of the inventory."
                                            reply "Some local drunk?"
                                            reply "Watch your mouth! He's done jobs that'd make your skin crawl, and he's done more than you'll ever live to do. He is a vet. Got fucked up. Buy him few drinks, ask him. Once he starts about that old story, he never stops."
                                            chocolateIcecream
                    , "Dude at the table with a girl" |=> do
                                                          talk 0 "Who is that dude over there at the table?"
                                                          reply "Augmented information trader from up the sprawl. Word is he took a job and ended up carrying some encrypted yak crap up in that shitbox. Now, yaks are after their property - destruction of it, if you get my drift. He's laying low, looking for some mythical hacker who can take that shit out of his head. tldr - stay away from him, unless you want to end up sliced like a deli."
                                                          chocolateIcecream
                    , "Girl at the table with the dude" |=> do
                                                            talk 0 "Who is the gal?"
                                                            reply "Street muscle. She's wired up to the teeth, though, and top grade hardware, not some backstreet workshop shit. Your buddy Dev tried running a bioscan on her from the bar and failed a smoke test. Said 'er wetware fried his probe."
                                                            reply "No kidding!"
                                                            reply "No kidding."
                                                            reply "Although, knowing Devin, it just ticked him off. He has a boner for peeking under the skirt. All more interesting when its not a five minute job."
                                                            reply "Backed off this one, though. Gal walked over to the bar, ordered him a drink. Said her metal can pin ice breakers to an inch within 5 mile radius. Asked Dev politely to have a drink and asked if he likes olives. I put them in just to piss him, so the kid's looking at me and saying 'Um, no, I do not'."
                                                            reply "So our gal over there says 'Well, please allow me', then slides some fred krueger slash wolverine steel from her finger. Both me and the kid gaping eyes, she pricks the olive and eats it, then walks away."
                                                            reply "Shit!"
                                                            reply "Her price range must be through the roof, but the jacko over there can probably handle it."
                                                            chocolateIcecream
                    , "Not right now" |=> do
                                          talk 0  "Actually, not right now. I wanted to ask..."
                                          mainBranch
                    ]

        beer = do
            talk 0 "Can I get a beer?"
            reply "Sure. Skinny, fat or extra fat?"
            choice_ [ "Skinny" |=> do
                                   talk 0 "Skinny, please."
                                   reply "Retro vibes today, eh? Sure, sure, skinny it is."
                                   mainBranch
                    , "Fat" |=> do
                                talk 0 "Fat, please"
                                reply "One fat draft, coming right up."
                                mainBranch
                    , "Extra fat" |=> do
                                      talk 0 "Extra fat, please"
                                      reply "Er, you sure? Its not even noon. Aiming to booze away until weekend?"
                                      choice_ [ "Yes" |=> do
                                                          talk 0 "Yeah, give me that extra fat."
                                                          reply "Sure, sure. Extra fat coming right up."
                                                          mainBranch
                                              , "No" |=> do
                                                         talk 0 "Actually, you're right, I changed my mind."
                                                         stillWantBeer

                                              ]
                    , "What?" |=> do
                                  talk 0 "Skinny? Fat? What do you mean?"
                                  reply "Laced beer, Cal! Synthemesc. You've been living under the rock?"
                                  reply "Yeah... sorry... I don't know what's up with me today..."
                                  stillWantBeer
                    , "Changed my mind" |=> mainBranch
                    ]
        stillWantBeer = do
            talk 1 "Still want beer?"
            choice_ [ "Yeah" |=> beer
                    , "Nope" |=> mainBranch
                    ]

