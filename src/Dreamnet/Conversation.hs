{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Dreamnet.Conversation
( ConversationNode(..)
, pick
, advance

, ConversationF
, runConversationF_temp
, name
, talk
, continue
, reply
, choice_
, describe
, (|=>)
) where


import Safe                (at)
import Control.Monad.Free  (Free(Pure, Free))
import Data.Monoid         ((<>))
import Data.Bool           (bool)

--------------------------------------------------------------------------------

-- Note: NEVER slap equality on recursive data structures
data ConversationNode = TalkNode   String Word [String] ConversationNode
                      | ChoiceNode [String] [ConversationNode]
                      | DescriptionNode String ConversationNode
                      | End


-- TODO I wonder if this equality will bite me in the ass later on?
instance Eq ConversationNode where
    (TalkNode _ _ _ _)    == (TalkNode _ _ _ _)    = True
    (ChoiceNode _ _)      == (ChoiceNode _ _)      = True
    (DescriptionNode _ _) == (DescriptionNode _ _) = True
    End                   == End                   = True
    _                     == _                     = False


instance Show ConversationNode where
    show (TalkNode t _ _ _)    = "TalkNode " <> t
    show (ChoiceNode cs _)     = "ChoiceNode " <> show cs
    show (DescriptionNode s _) = "DescriptionNode " <> show s
    show End                   = "End"

 
prepend ∷ ConversationNode → ConversationNode → ConversationNode
prepend End (TalkNode s i ps n)   = TalkNode s i ps (prepend End n)
prepend End (DescriptionNode s n) = DescriptionNode s (prepend End n)
prepend cn (TalkNode s i ps n)    = TalkNode s i ps (prepend n cn)
prepend cn (DescriptionNode s n)  = DescriptionNode s (prepend n cn)
prepend cn _                      = cn

--instance Monoid ConversationNode where
--    mempty = End
--    (TalkNode s i ps End)   `mappend` cn' = TalkNode s i ps (cn' <> End)
--    (TalkNode s i ps cn)    `mappend` cn' = TalkNode s i ps (cn <> cn')
--    (ChoiceNode opts cns)   `mappend` cn' = ChoiceNode (opts <> ["UNKNOWN_OPTION"]) (cns <> [cn'])
--    (DescriptionNode s End) `mappend` cn' = DescriptionNode s (cn' <> End)
--    (DescriptionNode s cn)  `mappend` cn' = DescriptionNode s (cn <> cn')
--    End                     `mappend` _   = End


pick ∷ ConversationNode → Word → ConversationNode -- Should really wrap this Int with something that won't backfire with OOB
pick (TalkNode _ _ _ n)    _ = n
pick (ChoiceNode _ ns)     i = ns `at` fromIntegral i
pick (DescriptionNode _ n) _ = n
pick End                   _ = End


advance ∷ ConversationNode → ConversationNode
advance n@(ChoiceNode _ _)    = pick n 0
advance (TalkNode _ _ _ n)    = n
advance (DescriptionNode _ n) = n
advance End                   = End

--------------------------------------------------------------------------------

data ConversationF a = CName      Word (String → a)
                     | CTalk      Word String a
                     | CContinue  String a
                     | CReply     String a
                     -- | CChoice    [String] (Int → a)
                     | CChoice_   [(String, Free ConversationF ())] a
                     | CDescribe  String a
                     deriving (Functor)


runConversationF_temp ∷ Free ConversationF a → ConversationNode
runConversationF_temp = runConversationF ["Carla", "Whoeverelse"] 0 1 End

-- TODO I should get away with ConversationNode alltogether and use free monads to render conversations
--      in realtime, adjusting parameters as necessary
runConversationF ∷ [String] → Word → Word → ConversationNode → Free ConversationF a → ConversationNode
runConversationF ps curr prev cn (Free (CName i fn)) =
    runConversationF ps curr prev cn (fn $ at ps (fromIntegral i))

runConversationF ps curr prev cn (Free (CTalk i s n)) =
    runConversationF ps i (bool curr prev $ i == curr) (prepend cn $ TalkNode s i ps End) n

runConversationF ps curr prev cn (Free (CContinue s n)) =
    runConversationF ps curr prev (prepend cn $ TalkNode s curr ps End) n

runConversationF ps curr prev cn (Free (CReply s n)) =
    runConversationF ps prev curr (prepend cn $ TalkNode s prev ps End) n

--runConversationF ps curr prev cn (Free (CChoice opts fn)) =
--    runConversationF ps curr prev (cn <> ChoiceNode opts) (fn 0) 

runConversationF ps curr prev cn (Free (CChoice_ opts n)) =
    let (ss, prgs) = unzip opts
        nodes      = fmap (runConversationF ps curr prev cn) prgs
    in  runConversationF ps curr prev (prepend cn $ ChoiceNode ss nodes) n

runConversationF ps curr prev cn (Free (CDescribe s n)) =
    runConversationF ps curr prev (prepend cn $ DescriptionNode s End) n

runConversationF _ _ _ cn (Pure _) =
    cn

--------------------------------------------------------------------------------

name ∷ Word → Free ConversationF String
name i = Free $ CName i Pure


talk ∷ Word → String → Free ConversationF ()
talk i s = Free $ CTalk i s (Pure ())


continue ∷ String → Free ConversationF ()
continue s = Free $ CContinue s (Pure ())


reply ∷ String → Free ConversationF ()
reply s = Free $ CReply s (Pure ())


--choice ∷ [String] → Free ConversationF Int
--choice opts = Free $ CChoice opts Pure


choice_ ∷ [(String, Free ConversationF ())] → Free ConversationF ()
choice_ opts = Free $ CChoice_ opts (Pure ())


describe ∷ String → Free ConversationF ()
describe s = Free $ CDescribe s (Pure ())


(|=>) ∷ String → Free ConversationF () → (String, Free ConversationF ())
(|=>) = (,)
