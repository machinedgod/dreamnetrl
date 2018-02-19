{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Dreamnet.DesignData
( DesignData
, dd_characters
, dd_defaultRedshirt
, dd_startingMap

, defaultDesignData
) where


import Control.Lens
import qualified Data.Map as M

import Dreamnet.Conversation
import Dreamnet.Character

--------------------------------------------------------------------------------

data DesignData = DesignData {
      _dd_characters      ∷ M.Map String (Character Item ConversationNode) 
    , _dd_defaultRedshirt ∷ Character Item ConversationNode
    , _dd_startingMap     ∷ String
    }

makeLenses ''DesignData


defaultDesignData ∷ DesignData
defaultDesignData =
    DesignData {
      _dd_characters      = M.fromList $ toNamedTuple <$> characters
    , _dd_defaultRedshirt = newCharacter "?" redshirtConvo
    , _dd_startingMap     = "res/job"
    }
    where
        toNamedTuple = (,) <$> view ch_name <*> id


characters ∷ [Character Item ConversationNode]
characters =
    [ newCharacter "Carla"   End
    , newCharacter "Raj"     rajConvo
    , newCharacter "Delgado" delgadoConvo

    , newCharacter "Moe"    moeConvo
    , newCharacter "Johnny" johnnyConvo
    , newCharacter "Sally"  sallyConvo
    ]

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
