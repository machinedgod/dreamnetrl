{-# LANGUAGE UnicodeSyntax #-}

module Design.GameCharacters
( randomCharacter

, characters
, characterForName
, characterDictionary

-- DEBUG
, randomName
, randomOrientation

, carla
)
where


import Safe                 (at)
import Control.Lens         (view)
import Control.Monad.Random (MonadRandom, getRandom, getRandomR)
import Data.List            (intercalate)
import Data.Maybe           (fromMaybe)
import Data.Bool            (bool)
import Data.Semigroup       ((<>))

import qualified Data.Map as M (Map, fromList, lookup)

import Dreamnet.Engine.Conversation
import Dreamnet.Engine.Character

import Design.DesignAPI
import Design.Items

--------------------------------------------------------------------------------

facGenpop ∷ Faction
facGenpop = Faction "genpop"


facCarla ∷ Faction
facCarla = Faction "carla"


--------------------------------------------------------------------------------

randomName ∷ (MonadRandom r) ⇒ r (String, String)
randomName = pure (,) <*> rndname <*> rndlname
    where
        rndname = at englishNames <$> getRandomR (0, length englishNames - 1)
        rndlname = at englishSurnames <$> getRandomR (0, length englishSurnames - 1)
        englishNames =
            [ "Aaron", "Abbott", "Abel", "Abner", "Abraham", "Adam", "Addison"
            , "Adler", "Adley", "Adrian", "Aedan", "Aiken", "Alan"
            , "Alastair", "Albern", "Albert", "Albion", "Alden", "Aldis"
            , "Aldrich", "Alexander", "Alfie", "Alfred", "Algernon"
            , "Alston", "Alton", "Alvin", "Ambrose", "Amery", "Amos"
            , "Andrew", "Angus", "Ansel", "Anthony", "Archer", "Archibald"
            , "Arlen", "Arnold", "Arthur", "Arvel", "Atwater", "Atwood"
            , "Aubrey", "Austin", "Avery", "Axel", "Baird", "Baldwin"
            , "Barclay", "Barnaby", "Baron", "Barrett", "Barry"
            , "Bartholomew", "Basil", "Benedict", "Benjamin", "Benton"
            , "Bernard", "Bert", "Bevis", "Blaine", "Blair", "Blake"
            , "Bond", "Boris", "Bowen", "Braden", "Bradley", "Brandan"
            , "Brent", "Bret", "Brian", "Brice", "Brigham", "Brock"
            , "Broderick", "Brooke", "Bruce", "Bruno", "Bryant", "Buck"
            , "Bud", "Burgess", "Burton", "Byron", "Cadman", "Calvert"
            , "Caldwell", "Caleb", "Calvin", "Carrick", "Carl", "Carlton"
            , "Carney", "Carroll", "Carter", "Carver", "Cary", "Casey"
            , "Casper", "Cecil", "Cedric", "Chad", "Chalmers", "Chandler"
            , "Channing", "Chapman", "Charles", "Chatwin", "Chester"
            , "Christian", "Christopher", "Clarence", "Claude", "Clayton"
            , "Clifford", "Clive", "Clyde", "Coleman", "Colin", "Collier"
            , "Conan", "Connell", "Connor", "Conrad", "Conroy", "Conway"
            , "Corwin", "Crispin", "Crosby", "Culbert", "Culver", "Curt"
            , "Curtis", "Cuthbert", "Craig", "Cyril"
            ]
        englishSurnames =
            [ "Smith", "Mitchell", "Jones", "Kelly", "Williams", "Cook"
            , "Taylor", "Carter", "Brown", "Richardson", "Davies", "Bailey"
            , "Evans", "Collins", "Wilson", "Bell", "Thomas", "Shaw"
            , "Johnson", "Murphy", "Roberts", "Miller", "Robinson", "Cox"
            , "Thompson", "Richards", "Wright", "Khan", "Walker", "Marshall"
            , "White", "Anderson", "Edwards", "Simpson", "Hughes", "Ellis"
            , "Green", "Adams", "Hall", "Singh", "Lewis", "Begum"
            , "Harris", "Wilkinson", "Clarke", "Foster", "Patel", "Chapman"
            , "Jackson", "Powell", "Wood", "Webb", "Turner", "Rogers"
            , "Martin", "Gray", "Cooper", "Mason", "Hill", "Ali", "Ward"
            , "Hunt", "Morris", "Hussain", "Moore", "Campbell", "Clark"
            , "Matthews", "Lee", "Owen", "King", "Palmer", "Baker"
            , "Holmes", "Harrison", "Mills", "Morgan", "Barnes", "Allen"
            , "Knight", "James", "Lloyd", "Scott", "Butler", "Phillips"
            , "Russell", "Watson", "Barker", "Davis", "Fisher", "Parker"
            , "Stevens", "Price", "Jenkins", "Bennett", "Murray", "Young"
            , "Dixon", "Griffiths", "Harvey"
            ]


randomNickname ∷ (MonadRandom r) ⇒ r String
randomNickname = at nouns <$> getRandomR (0, length nouns - 1)
    where
        nouns =
            [ "Galadriel", "Zen", "Saint", "Strange", "Prophet", "Binary"
            , "Storm", "Doctor", "Chaos", "Cyber", "God", "Zombie"
            , "Daemon", "Neon", "Wizard", "Pirate", "Wicked", "Comrade"
            , "Radical", "God", "Psycho", "Phantom", "Strange", "Blade"
            , "Lord", "Tempest", "Dark", "Alias", "Shogun", "Edward"
            , "Teach", "Machiavelli", "Perfect", "Demon"
            ]

--------------------------------------------------------------------------------

randomOrientation ∷ (MonadRandom r) ⇒ r Orientation
randomOrientation = bool LeftSide RightSide <$> getRandom

--------------------------------------------------------------------------------

em ∷ Slot o t i
em = Slot Nothing


randomEquipment ∷ (MonadRandom r) ⇒ r (Equipment i)
randomEquipment = pure $ Equipment em em em em em em em em em em em em em em

--------------------------------------------------------------------------------

randomCharacter ∷ (MonadRandom r) ⇒ r DreamnetCharacter
randomCharacter = do
    (n, ln) ← randomName
    nn      ← randomNickname
    hnd     ← randomOrientation
    equ     ← generateEquipment
    msk     ← pure (MeleeCombatSkills 0 0 0 0 0 0)
    rsk     ← pure (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
    tsk     ← pure (ThrowingSkills 0 0 0 0 0)
    esk     ← pure (EngineeringSkills 0 0 0 0 0 0)
    csk     ← pure (CommunicationSkills 0 0 0 0 0 0 0)
    isk     ← pure (InfiltrationSkills 0 0 0 0 0)
    desc    ← generateDescription msk rsk tsk esk csk isk
    convo   ← pure generateConvo

    pure $ newCharacter
        n ln
        nn
        hnd
        desc
        equ
        facGenpop
        convo
        10
        msk rsk tsk esk csk isk


generateDescription ∷ (MonadRandom r)
                    ⇒ MeleeCombatSkills
                    → RangedCombatSkills
                    → ThrowingSkills
                    → EngineeringSkills
                    → CommunicationSkills
                    → InfiltrationSkills
                    → r String
generateDescription _ _ _ _ _ _ = pure "You never saw this person in your life."


generateConvo ∷ (ConversationAPI o c) ⇒ c () -- TODO I should be able to mix conversation monad with monad random!
generateConvo = talk 1 "Beat it, lizzie!"


generateEquipment ∷ (MonadRandom r) ⇒ r (Equipment States)
generateEquipment = randomEquipment

--------------------------------------------------------------------------------

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
carla = newCharacter
            "Carla" "D'Addario"
            "La Piovra"
            RightSide
            desc
            equ
            facCarla
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc = intercalate "\n"
            [ "Scrawny, beautiful, capable. You know you are. You also hope no one figures out quickly enough that you are."
            , "In this business, being considered green has certain advantages."
            ]
        equ = Equipment
                (Slot Nothing) -- lhand
                (Slot Nothing) -- rhand
                (Slot . pure . Clothes $ headband) -- head
                (Slot . pure . Clothes $ armourPiece Torso Kevlar) -- torso
                (Slot . pure . Clothes $ backpack [ Weapon laserjet ] ) -- back
                (Slot . pure . Clothes $ clipBelt [ Throwable fragmentGrenade, Ammo laserjetClip, Ammo laserjetClip ]) -- belt
                (Slot . pure . Clothes $ armourPiece Arm Kevlar) -- larm
                (Slot . pure . Clothes $ armourPiece Arm Kevlar) -- rarm
                (Slot . pure . Clothes $ armourPiece Thigh Kevlar) -- lthigh
                (Slot . pure . Clothes $ armourPiece Thigh Kevlar) -- rthigh
                (Slot . pure . Clothes $ armourPiece Shin Kevlar) -- lshin
                (Slot . pure . Clothes $ armourPiece Shin Kevlar) -- rshin
                (Slot . pure . Clothes $ boot Kevlar) -- lfoot
                (Slot . pure . Clothes $ boot Kevlar) -- rfoot
        -- TODO Carla talks to herself in the mirror and gets stat boost?
        convo = do
            talk 0 "Hi."
            reply "Hi."
            reply "We'll get through this, will we?"
            reply "We will. You *can* do this. You *CAN*! No one can tell you otherwise. Stop the doubt, brace yourself and just keep on pushing."


hideo ∷ DreamnetCharacter
hideo = newCharacter
            "Hideo" "Hattori"
            "Tetsuo"
            RightSide
            desc
            equ
            facCarla
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc = "<DESCRIPTION MISSING>"
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."


devin ∷ DreamnetCharacter
devin = newCharacter
            "Devin" "Dorsett"
            "570rm"
            RightSide
            desc
            equ
            facCarla
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc = "<DESCRIPTION MISSING>"
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."


delgado ∷ DreamnetCharacter
delgado = newCharacter
            "Phillipe" "Delgado"
            "Sarge"
            RightSide
            desc
            equ
            facCarla
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc = intercalate "\n"
            [ "Scrawny, scarred and tough as nails."
            , "When you look in his eyes without giving a tell, you see that there's unspeakable depth and pain, but as soon as he catches your eyes, an imaginary door comes slamming down, the depth disappears and and Delgado's face takes that of the one everyone knows the best: mean SOB."
            , "You wonder if this is a conscious effort to maintain illusion of superiority, or a subconscious defense mechanism. Either way, there's certain type of abstract beauty in there, and somewhere very, very far away in the depths of your own brain - you find yourself thinking about how does Major Phillipe Delgado look naked."
            ]
        equ = Equipment em em em em em em em em em em em em em em
        convo = do
            describe "As you approach this pile of muscles and scars, your eyes perceive this tiniest twitch, as if the bones were re-settling in joints. You realize that he spotted you the moment you entered, even if his back were turned to you, this is his probably decades of training and even more decades of experience doing their work: his body preparing to jump and twist your neck at the slightest wrong move."
            talk 0 "Hey.. hi... you're Sargeant Delgado, right?"
            describe "He acts as if you don't exist, starring somewhere in the distance across the bar. He tips his beer and drinks of a silent gulp."
            choice_ [ "Press on" |=> pressOn
                    , "Back off" |=> pure ()
                    ]
        pressOn = do
            talk 0 "Look, I have a run to do, and I need someone to watch my back. Word is, you're the best, so I want to do business."
            reply "I work alone."
            reply "Okay... okay... look, this is a two-person job. We split 50-50"
            reply "80-20. What's the job?"
            choice_ [ "Haggle"           |=> haggle
                    , "Describe the job" |=> describeTheJob
                    , "Seal the deal"    |=> sealTheDeal
                    ]
        haggle = do
            talk 0 "I could go as far as 60-40, but that's it."
            reply "80-20. Non negotiable. I don't run with lizards so consider this a special favor to you."
            choice_ [ "Describe the job" |=> describeTheJob
                    , "Seal the deal"    |=> sealTheDeal
                    ]
        describeTheJob = do
            talk 0 "So, its basically in-and-out, industrial espionage type of thing. We go in, crack the safe, grab microdisk for the client and leave."
            reply "And who is your client?"
            reply "That's confidental and on a strict need-to-know basis."
            choice_ [ "Haggle"           |=> haggle
                    , "Seal the deal"    |=> sealTheDeal
                    ]
        sealTheDeal = do
            talk 0 "So, you in?"
            describe "He eyes you, as if he's weighing a giant decision in his head."
            reply "I bring my own hardware. Subdermal or dental comm implant is a must. Do you have access to a hack?"
            reply "Er, my contact is currently on vacation."
            reply "Go to 5th and Randall, get behind the building, say Delgado sent you. His name is Tom Sawyer... Tom Saw-yer, get it?"
            describe "He laughs heartily at this"
            talk 1 "Anyway, bring Cs and your own stims."
            describe "You see him scratching something on the napkin, then he hands it to you."
            receiveItem 0 (Prop "Delgado's napkin")
            talk 1 "Contact node. For when you're ready."
            reply "Dude, thanks. We're in business!"
            reply "Yea, yea, yea. Hack, node, job. Go."


raj ∷ DreamnetCharacter
raj = newCharacter
        "Qaayenaat" "Rajan"
        "Raj"
        RightSide
        desc
        equ
        facCarla
        convo
        10
        (MeleeCombatSkills 0 0 0 0 0 0)
        (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
        (ThrowingSkills 0 0 0 0 0)
        (EngineeringSkills 0 0 0 0 0 0)
        (CommunicationSkills 0 0 0 0 0 0 0)
        (InfiltrationSkills 0 0 0 0 0)
    where
        desc = intercalate "\n"
            [ "Tiny, skinny and looking like she'd bolt in a second if she could."
            , "Poor Raj, you think, wondering if she understands how much you feel out of place as well. Looking at her cute, young face, you find that she's beautiful in her own way."
            , " If things didn't happen for her the way they did, it'd be hard to imagine her even throwing a look down your way, if you'd happen to pass her by in the street."
            ]
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."


nova ∷ DreamnetCharacter
nova = newCharacter
        "Annabelle" "Jenkins"
        "Nova"
        RightSide
        desc
        equ
        facCarla
        convo
        10
        (MeleeCombatSkills 0 0 0 0 0 0)
        (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
        (ThrowingSkills 0 0 0 0 0)
        (EngineeringSkills 0 0 0 0 0 0)
        (CommunicationSkills 0 0 0 0 0 0 0)
        (InfiltrationSkills 0 0 0 0 0)
    where
        desc = "<DESCRIPTION MISSING>"
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."


cobra ∷ DreamnetCharacter
cobra = newCharacter
            "Eduarda" "Ribeiro"
            "Cobra"
            RightSide
            desc
            equ
            facCarla
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc = "<DESCRIPTION MISSING>"
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."


kman ∷ DreamnetCharacter
kman = newCharacter
        "Kelly" "Lafleur"
        "K-man"
        RightSide
        desc
        equ
        facCarla
        convo
        10
        (MeleeCombatSkills 0 0 0 0 0 0)
        (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
        (ThrowingSkills 0 0 0 0 0)
        (EngineeringSkills 0 0 0 0 0 0)
        (CommunicationSkills 0 0 0 0 0 0 0)
        (InfiltrationSkills 0 0 0 0 0)
    where
        desc = "<DESCRIPTION MISSING>"
        equ = Equipment em em em em em em em em em em em em em em
        convo = talk 1 "I believe in you, Cal."

--------------------------------------------------------------------------------

johnny ∷ DreamnetCharacter
johnny = newCharacter
            "Johnny" "M."
            "Jockey"
            RightSide
            desc
            equ
            facGenpop
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc =
            "If he'd have asian facial features, he'd look like a textbook sarariman."
        equ = Equipment em em em em em em em em em em em em em em
        convo = do
            talk 1 "Yea?"
            name 0 >>= reply . ("Hey dude, I'm " <>)
            reply "Who gives a shit? Now would you mind, we're in the middle of something."


sally ∷ DreamnetCharacter
sally = newCharacter
            "Sally" "S."
            "M"
            RightSide
            desc
            equ
            facGenpop
            convo
            10
            (MeleeCombatSkills 0 0 0 0 0 0)
            (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
            (ThrowingSkills 0 0 0 0 0)
            (EngineeringSkills 0 0 0 0 0 0)
            (CommunicationSkills 0 0 0 0 0 0 0)
            (InfiltrationSkills 0 0 0 0 0)
    where
        desc =
            "Slim, well-built and gorgeous, this woman looks like she's packing serious hardware. Her scrawny short black hair falls just short of shiny mirrors where her eyes are supposed to be. Probably HUD augments."
        equ = Equipment em em em em em em em em em em em em em em
        convo = do
            talk 1 "Mmmm, hello _there_ hot stuff."
            reply "Hi, I am..."
            reply "Yeah, listen, not tonight sugar puffs, I'm in the middle of something, OK?"


moe ∷ DreamnetCharacter
moe = newCharacter
        "Moe" "Sarlac"
        "Trigger"
        RightSide
        desc
        equ
        facGenpop
        convo
        10
        (MeleeCombatSkills 0 0 0 0 0 0)
        (RangedCombatSkills 0 0 0 0 0 0 0 0 0 0)
        (ThrowingSkills 0 0 0 0 0)
        (EngineeringSkills 0 0 0 0 0 0)
        (CommunicationSkills 0 0 0 0 0 0 0)
        (InfiltrationSkills 0 0 0 0 0)
    where
        desc = intercalate "\n"
            [ "You never met anyone whose name fit them better. Black side hair and shiny dome, white sleeveless shirt covered with beer and fat stains, covering his giant belly - somehow, you are *sure* Moe would present a formidable opponent in close quarters."
            , "Its common knowledge that he used to run, but Devin told you bits and pieces of his past that make you feel tremendous respect towards this relic of the old age."
            ]
        equ = Equipment em em em em em em em em em em em em em em
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

