{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | APIs for building and using computers
module Dreamnet.ComputerModel
( ComputerAPI(..)
, ComputerM

, ComputerData
, cd_input

, computerOutput
, computerData

, newComputer
) where

import Prelude hiding (read)

import Control.Lens hiding (List)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char  (toLower)
import Data.List  (intercalate)
import Data.Maybe (fromMaybe)


--------------------------------------------------------------------------------

class ComputerAPI c where
    input ∷ Char → c ()
    backspace ∷ c ()
    commitInput ∷ c String

--------------------------------------------------------------------------------

data ComputerData = ComputerData {
      _cd_input ∷ String
    }

makeLenses ''ComputerData

--------------------------------------------------------------------------------

newtype ComputerM a = ComputerM { runComputerM ∷ State ComputerData a }
                    deriving (Functor, Applicative, Monad, MonadState ComputerData) 


computerOutput ∷ ComputerM String → String
computerOutput cm = runComputerM cm `evalState` bootData 


computerData ∷ ComputerM () → ComputerData
computerData cm = runComputerM cm `execState` bootData 


instance ComputerAPI ComputerM where
    input c = cd_input %= (++[toLower c])
    backspace = cd_input %= (take <$> subtract 1 . length <*> id)
    commitInput = do
        o ← fmap (fromMaybe "Syntax error.") $ runMaybeT $ do
              c ← MaybeT (uses cd_input commandForString)
              lift (processCommand c)
        cd_input .= ""
        return o


data Command = Help   (Maybe String)
             | Login  (Maybe String)
             | List   (Maybe String)
             | Read   (Maybe String)
             | Load   (Maybe String)
             | Run
             | Poke   (Maybe String)
             | Peek   (Maybe String)
             deriving (Read)


commandForString ∷ String → Maybe Command
commandForString (words → [])  = Nothing
commandForString (words → [c]) = command c Nothing
commandForString (words → l)   = command (head l) (Just $ head $ drop 1 l)


command ∷ String → Maybe String → Maybe Command
command "help"  p = Just $ Help p
command "login" p = Just $ Login p
command "list"  p = Just $ List p
command "read"  p = Just $ Read p
command "load"  p = Just $ Load p
command "run"   _ = Just Run
command "poke"  p = Just $ Poke p
command "peek"  p = Just $ Peek p
command _       _ = Nothing


processCommand ∷ Command → ComputerM String
processCommand (Help mp) = return $ maybe generalHelp specificHelp mp
    where
        generalHelp          = "Commands: login, list, read, load, run, poke, peek"
        specificHelp "help"  = "Use help with help to get help about help."
        specificHelp "login" = "Login to genpop node as a specific user."
        specificHelp "list"  = "List directories and files. Use without parameters to list root."
        specificHelp "read"  = "Read files, news, mail. Requires filename parameter."
        specificHelp "load"  = "Load a program into memory, given there's enough RAM."
        specificHelp "run"   = "Run currently loaded program."
        specificHelp "poke"  = "Change contents of a register or a memory address."
        specificHelp "peek"  = "Print contents of a register or a memory address."
        specificHelp c       = "No help for: " ++ c
processCommand (Login mp) = return $ maybe err login mp
    where
        err     = "Command error."
        login l = "Successful login: " ++ l
processCommand (List mp) = return $ maybe listRoot listDir mp 
    where
        listRoot            = intercalate ", " ["news", "mail", "bank", "cartridge"]
        listDir "news"      = intercalate ", " ["weather", "politics", "finance"]
        listDir "mail"      = intercalate ", " ["S70rm: 'Hey!'", "<SPAM>: 'Grieving?'", "BuildingBot: 'Rent due'"]
        listDir "bank"      = intercalate ", " ["balance", "credit"]
        listDir "cartridge" = "segmentation fault: 0x7473 <no cartridge hardware installed>"
        listDir d           = "List error: No directory: " ++ d
processCommand (Read mp) = return $ maybe err read mp
    where
        err                = "Read error: need a name."
        read "weather"     = "The rain continues to fall, as we approach one of the coldest winters to be predicted in the last 50 years."
        read "politics"    = "Major Thompson continues the campaign for legal distinction between gender and race. Scientists are divided."
        read "finance"     = "Yet another positive quarter for Zeronix Medical, garnering just short of 80 billion credits in revenue."
        read "s70rm"       = intercalate "\n" [ "Hey girl!"
                                              , "I'll be down at Moe's today around noon, there's an uptown prick wanting to buy some custom hardware."
                                              , "Sold the gimp a calculator made of old arcade machine parts and put clean 2000c's in my chit."
                                              , "\n"
                                              , "Drop by, I have good news for you regarding the job!"
                                              , "\n"
                                              , "S70rm"
                                              , "-----"
                                              , "'All that it takes for evil to happen is for good people to stand by and do nothing.'"
                                              ]
        read "<spam>"      = intercalate "\n" [ "Dearest Ms. Carla D'Addario,"
                                              , "are you feeling low due to recent death in the family?"
                                              , "Difficulty getting up and going to earn that paycheck?"
                                              , "Friends not returning calls because they have more interest in partying, than lending an ear?"
                                              , "Thoughts of just ending it all?"
                                              , "\n"
                                              , "Fret not Ms. Carla D'Addario! Our clinic has a monthly special on drugs and psycho surgeries that suits your needs just *perfectly*!"
                                              , "Call us *immediately* at 1-8888-75843-DEPRESSED and use this incredible offer!"
                                              , "And remember - if our medication can't help you - its because you're terminal!"
                                              ]
        read "buildingbot" = intercalate "\n" [ "Tenant A4931, building block 5, please be noted that you are now 1 day 07 hours late on your rent payment."
                                              , "Inability to meet the requirements in the next 17 hours shall result in immediate eviction, as per civil code 'PROPERTY LEASE' section 483 paragraph 4 bullet A."
                                              , "Additionally, all belongings found in the apartment shall be disposed of in the section's incinerator, as per civil code 'PROPERTY OWNERSHIP TRANSFER' section 13 paragraph 1 bullet A."
                                              , "Have a pleasant, safe and productive day, Ms. D'Addario."
                                              ]
        read "balance"     = "You have 900c."
        read "credit"      = "You have no credit."
        read f             = "No such file: " ++ f
processCommand (Load _) = return $ "segmentation fault: 0x7473 <no cartridge hardware installed>"
processCommand Run      = return $ "Crack-boot segment empty."
processCommand (Poke _) = return $ "Privilege escalation required. This command is used only for routine maintenance of your node, and should not exist on your hardware. Please report this error immediately to the ministry of online communications at 9-1-1-MINIONLINE."
processCommand (Peek _) = return $ "Privilege escalation required. This command is used only for routine maintenance of your node, and should not exist on your hardware. Please report this error immediately to the ministry of online communications at 9-1-1-MINIONLINE."

--------------------------------------------------------------------------------

bootData ∷ ComputerData
bootData = ComputerData ""


newComputer ∷ ComputerM ()
newComputer = return ()


