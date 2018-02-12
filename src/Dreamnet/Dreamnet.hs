{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Dreamnet.Dreamnet
where

import Control.Lens              (makeLenses, use, uses, view, (.=), (%=))
import Control.Monad             (void, when)
import Control.Monad.Free        (Free)
import Control.Monad.State       (StateT, lift, execStateT)
import Data.Semigroup            ((<>))
import Data.Functor              (($>))
import Data.Bool                 (bool)
import Linear                    (V2(V2))

import qualified UI.NCurses  as C
import qualified Config.Dyre as Dyre

import Dreamnet.DesignData
import Dreamnet.GameState
import qualified Dreamnet.Input as Input
import Dreamnet.World
import Dreamnet.Conversation

import Dreamnet.CoordVector
import Dreamnet.TileData         (ttype, readBoolProperty, readStringProperty)
import Dreamnet.TileMap
import Dreamnet.WorldMap
import Dreamnet.Entity
import Dreamnet.ScrollWindow
import Dreamnet.ChoiceWindow
import Dreamnet.ComputerModel
import Dreamnet.Renderer
import Dreamnet.Visibility
import Dreamnet.Character
import Dreamnet.ObjectMonad

--------------------------------------------------------------------------------

data Game = Game {
      _g_world        ∷ World Visibility (Character Item ConversationNode)
    , _g_gameState    ∷ GameState
    , _g_keepRunning  ∷ Bool
    , _g_rendererData ∷ RendererEnvironment

    -- This is a correct place to put it for now,
    -- because later on there'll be multiple 'update' places
    , _g_conversation ∷ ConversationNode
    , _g_conversationWindow ∷ C.Window
    , _g_scrollWindow ∷ ScrollData
    , _g_choiceWindow ∷ ChoiceData

    -- TODO take this out, eventually
    , _g_carlasComputer ∷ ComputerM ()
    , _g_carlasFramebuffer ∷ String
    }

makeLenses ''Game


newGame ∷ DesignData → C.Curses Game
newGame dd = do
    rdf ← initRenderer
    m   ← loadTileMap (view dd_startingMap dd)
    cvw ← createConversationWindow
    sw  ← createScrollData
    cw  ← createChoiceData
    pure Game {
        _g_world        = newWorld
                              (fromTileMap m objectFromTile)
                              [ newCharacter "Carla"   End
                              , newCharacter "Raj"     End
                              , newCharacter "Delgado" End
                              ]
      , _g_gameState    = Normal 
      , _g_keepRunning  = True
      , _g_rendererData = rdf

      , _g_conversation       = End
      , _g_conversationWindow = cvw
      , _g_scrollWindow       = sw
      , _g_choiceWindow       = cw

      , _g_carlasComputer    = newComputer
      , _g_carlasFramebuffer = "Ready."
    }
    where -- TODO Set materials!
        objectFromTile t@(ttype → "Base")     = Object "Base"     (view t_char t) "concrete"                 (1 `readBoolProperty` t) (2 `readBoolProperty` t)   "<base>"
        objectFromTile t@(ttype → "Door")     = Object "Door"     (view t_char t) "wood"                     (1 `readBoolProperty` t) (1 `readBoolProperty` t) $ "Just a common door. They're " <> bool "closed." "opened." (1 `readBoolProperty` t)
        objectFromTile t@(ttype → "Stairs")   = Object "Stairs"   (view t_char t) "wood"                     (1 `readBoolProperty` t)  True                    $ "If map changing would've been coded in, you would use these to go " <> bool "down." "up." (1 `readBoolProperty` t)
        objectFromTile t@(ttype → "Prop")     = Object "Prop"     (view t_char t) (4 `readStringProperty` t) (2 `readBoolProperty` t) (3 `readBoolProperty` t) $ "A " <> (4 `readStringProperty` t) <> "."
        objectFromTile t@(ttype → "Person")   = Object "Person"    '@'            "blue"                      False                    True                    $ "Its " <> (1 `readStringProperty` t) <> "."
        objectFromTile   (ttype → "Spawn")    = Object "Spawn"     '.'            "concrete"                  True                     True                    $ "Spawn point. You really should not be able to examine this?" -- TODO shitty hardcoding, spawns should probably be generalized somehow!
        objectFromTile t@(ttype → "Camera")   = Object "Camera"   (view t_char t) "green light"               True                     True                      "A camera, its eye lazily scanning the environment. Its unaware of you, or it doesn't care." --                                        "A camera is frantically following your motion as you move around the room and blinking a little red LED. You pessimistically assume you must've been detected!"
        objectFromTile t@(ttype → "Computer") = Object "Computer" (view t_char t) "metal"                     False                    True                      "Your machine. You wonder if Devin mailed you about the job."
        objectFromTile t@(ttype → "Item")     = Object "Item"     (view t_char t) "blue plastic"              True                     True                    $ "A " <> (1 `readStringProperty` t) <> "."
        objectFromTile t                      = error $ "Can't convert Tile type into Object: " <> show t
-- TODO Errrrrr, this should be done through the tileset???
        --objectFromTile dd t@(ttype → "Person")   = let name      = 1 `readStringProperty` t
        --                                               maybeChar = M.lookup name (dd^.dd_characters)
        --                                           in  (fromMaybe (dd^.dd_defaultRedshirt) maybeChar)


--------------------------------------------------------------------------------

launchDreamnet ∷ DesignData → IO ()
launchDreamnet = Dyre.wrapMain Dyre.defaultParams {
                                 Dyre.projectName = "DreamnetRL"
                               , Dyre.realMain    = dreamnet
                               , Dyre.showError   = \_ m → error m
                               }

--------------------------------------------------------------------------------

-- TODO This'll die when renderer gets refactored a bit
doRender ∷ RendererF () → StateT Game C.Curses ()
doRender r = use g_rendererData >>=
    lift . (`runRenderer` r) >>=
    (g_rendererData .=) . snd
        
--------------------------------------------------------------------------------

dreamnet ∷ DesignData → IO ()
dreamnet dd = C.runCurses $ do
    -- Init curses
    C.setRaw     True
    C.setEcho    False
    C.defaultWindow >>= (`C.setKeypad` True)
    void $ C.setCursorMode C.CursorInvisible
    g ← newGame dd
    void $ flip execStateT g $ do
        g_world %= snd . runWorld (updateVisible $> Normal)
        renderNormal
        loopTheLoop


loopTheLoop ∷ StateT Game C.Curses ()
loopTheLoop = do
    r ← use g_keepRunning
    when r $ do
        e ← use g_gameState >>= lift . Input.nextEvent
        
        when (e == Input.Quit) $
            g_keepRunning .= False

        -- States can morph into other states
        -- We need an 'init' function to be called when state is first changed
        --
        -- If state switch would fire an event type instead of just
        -- being returned by the world, then the pipeline would still hold!
        --
        -- Problem is that update (maybe even render) components would need
        -- access to the input pipeline then, which means World has to be aware
        -- of at least certain portions of the Game.

        s ← use g_gameState
        case s of
            Normal →
                case e of
                    (Input.WorldEv (Input.Move v)) → do
                        g_world %= snd . runWorld (moveSelected v >> updateVisible $> Normal)
                        renderNormal

                    (Input.WorldEv Input.Wait) → do
                        g_world %= snd . runWorld (setStatus "Waiting..." >> updateVisible $> Normal)
                        renderNormal

                    (Input.WorldEv (Input.SelectTeamMember 0)) → do
                        g_world %= snd . runWorld (selectCharacter (byName "Carla") $> Normal)
                        renderNormal

                    (Input.WorldEv (Input.SelectTeamMember 1)) → do
                        g_world %= snd . runWorld (selectCharacter (byName "Raj") $> Normal)
                        renderNormal

                    (Input.WorldEv (Input.SelectTeamMember 2)) → do
                        g_world %= snd . runWorld (selectCharacter (byName "Delgado") $> Normal)
                        renderNormal

                    (Input.WorldEv Input.Get) → do
                        obtainTarget >>= \case
                            Just (v, o) → renderMessage $ "Trying to pick up: " <> show o <> " at " <> show v
                            Nothing     → renderMessage "There's nothing here."

                    (Input.WorldEv Input.UseHeld) → do
                        obtainTarget >>= \case
                            Just (v, o) → do
                                g_world %= snd . runWorld (setStatus ("You hit the " <> show o <> " at " <> show v <> " for ! damage.") >> deleteObject v o $> Normal)
                                renderNormal
                            Nothing     → renderMessage "Nothing there."

                    (Input.WorldEv Input.Examine) → do
                        -- TODO run program!
                        examineText ← obtainTarget >>= \case
                            Nothing → uses (g_world.w_map) desc
                            Just t  → pure $ view o_description (snd t)
                        g_scrollWindow %= setText examineText
                        use g_scrollWindow >>= lift . renderScrollWindow
                        g_gameState .= Examination

                    (Input.WorldEv Input.Operate) → do
                        obtainTarget >>= \case
                            Nothing → do
                                renderMessage "There's nothing here."
                            Just (v, o) → do
                                (gs, w') ← uses g_world (runWorld (runProgram v o))
                                g_world .= w'
                                g_gameState .= gs -- TODO if this isn't "NORMAL" then renderNormal that follows isn't correct
                                renderNormal

                    (Input.WorldEv Input.Talk) → do
                        obtainTarget >>= \case
                            Nothing → renderMessage "Trying to talk to someone, but there's no one there."
                            -- TODO ONLY IF O IS A PERSON - ALSO REMEMBER TO ACTUALLY GET THE CONVERSATION NODE
                            Just _  → do
                                g_gameState .= Conversation
                                use g_conversation >>= renderConversation -- >> renderNormal

                    (Input.WorldEv Input.InventorySheet) → do
                        is ← uses (g_world.w_active.e_object) (fmap show . equippedContainers)
                        g_scrollWindow %= setLines is
                        use g_scrollWindow >>= lift . renderScrollWindow
                        g_gameState .= InventoryUI

                    (Input.WorldEv Input.CharacterSheet) → do
                        g_scrollWindow %= setText "Character sheet"
                        use g_scrollWindow >>= lift .  renderScrollWindow
                        g_gameState .= CharacterUI

                    _ → pure ()  -- TODO Invalid event in invalid state. Oups!

            Examination →
                case e of
                    (Input.UIEv Input.MoveUp) → do
                        g_scrollWindow %= scrollUp
                        use g_scrollWindow >>= lift . renderScrollWindow
                    (Input.UIEv Input.MoveDown) → do
                        g_scrollWindow %= scrollDown
                        use g_scrollWindow >>= lift . renderScrollWindow
                    _ → do
                        use g_scrollWindow >>= lift . clearScrollWindow
                        g_gameState .= Normal
                        renderNormal


            Conversation → do
                c ← use g_conversation
                case c of
                    End → do
                        g_gameState .= Normal
                        use g_conversationWindow >>= lift . clearConversationWindow
                        renderNormal

                    --(ListenNode l) → do
                    --    g_choiceWindow %= setOptions l
                    --    updateConversationChoice uie
                    --    use g_conversation >>= renderConversation
                    ChoiceNode l _ → do
                        let (Input.UIEv uie) = e
                        g_choiceWindow %= setOptions l
                        updateConversationChoice uie
                        use g_conversation >>= renderConversation
                    _   → do
                        g_conversation %= advance
                        use g_conversation >>= renderConversation

            InventoryUI →
                case e of
                    (Input.UIEv Input.MoveUp) → do
                        g_scrollWindow %= scrollUp
                        use g_scrollWindow >>= lift . renderScrollWindow
                    (Input.UIEv Input.MoveDown) → do
                        g_scrollWindow %= scrollDown
                        use g_scrollWindow >>= lift . renderScrollWindow
                    _ → do
                        g_gameState .= Normal

            CharacterUI →
                case e of
                    (Input.UIEv Input.MoveUp) → do
                        g_scrollWindow %= scrollUp
                        use g_scrollWindow >>= lift . renderScrollWindow
                    (Input.UIEv Input.MoveDown) → do
                        g_scrollWindow %= scrollDown
                        use g_scrollWindow >>= lift . renderScrollWindow
                    _ → do
                        g_gameState .= Normal
                    

            Operation → do
                let (Input.PassThrough c) = e
                updateComputer c
                qr ← uses g_carlasComputer (view cd_requestedQuit . computerData)
                if qr
                  then g_gameState .= Normal
                  else do
                       comp ← use g_carlasComputer
                       fbr  ← use g_carlasFramebuffer
                       doRender (renderComputer fbr (computerData comp))
            _ → pure ()  --- TODO Target selection shouldn't happen in this case branch, no?


        lift C.render
        loopTheLoop 


runProgram ∷ (Monad w, WorldAPI v c w) ⇒ V2 Int → Object → w GameState
runProgram v o = snd <$> runObjectMonadWorld (operationProgramForName (view o_name o)) v o
    where
        operationProgramForName ∷ String → Free ObjectF ()
        operationProgramForName "Door"     = door     Operate
        operationProgramForName "Computer" = computer Operate
        operationProgramForName "Person"   = person   Operate
        operationProgramForName _          = generic  Operate

-- TODO reuse code for aiming weapons
--switchAim ∷ Maybe (Object → Bool) → StateT Game C.Curses ()
--switchAim (Just nof) = do
--    pp ← use  (g_world.w_active.e_position)
--    os ← uses (g_world.w_map) (interestingObjects pp 2 nof)
--    ca ← use  g_aim
--    case ca of
--        Just a → g_aim .= headMay (drop 1 $ dropWhile (/=a) $ concat (replicate 2 os))
--        _      → g_aim .= headMay os
--switchAim Nothing = g_aim .= Nothing


-- TODO reuse code for aiming weapons
--allButTheBase ∷ Object → Bool
--allButTheBase o
--    | view o_symbol o == '.' = False
--    | otherwise              = True


-- TODO lets get stuck here until commit?
updateConversationChoice ∷ Input.UIEvent → StateT Game C.Curses ()
updateConversationChoice Input.MoveUp = g_choiceWindow %= selectPrevious
updateConversationChoice Input.MoveDown = g_choiceWindow %= selectNext
updateConversationChoice Input.SelectChoice = do
    pf ← uses g_choiceWindow (pick . commit)
    g_conversation %= pf
updateConversationChoice _ = pure ()


obtainTarget ∷ StateT Game C.Curses (Maybe (V2 Int, Object))
obtainTarget = do
    renderMessage "Select direction:"
    lift C.render

    ap ← use (g_world.w_active.e_position)
    t ← lift (Input.nextEvent TargetSelection) >>= \case
            Input.TargetEv (Input.Aim v) → pure v
            -- TODO Should never receive anything except target events
            _ → pure (V2 0 0)
    case t of
        (V2 0 0) → pure Nothing
        v        → do
            os ← uses (g_world.w_map) (drop 1 . valuesAt (ap + v))
            case os of
                [] → pure Nothing
                l  → pure (Just (ap + v, last l))  -- TODO find a way to deal with noninteresting objects (<base>)

--------------------------------------------------------------------------------

byName ∷ String → (Character a b) → Bool
byName n = (==n) . view ch_name


updateComputer ∷ Char → StateT Game C.Curses ()
updateComputer '\n' = do
    o ← uses g_carlasComputer (*> commitInput)
    g_carlasFramebuffer .= computerOutput o
    g_carlasComputer %= (\l → l *> commitInput $> ())
updateComputer '\b' = g_carlasComputer %= (*> backspace)
updateComputer c    = g_carlasComputer %= (*> input c)


--withAimOrElse ∷ (V2 Int → [Object] → StateT Game C.Curses a) → StateT Game C.Curses a → StateT Game C.Curses a
--withAimOrElse f e = fromMaybe e <=< runMaybeT $ do
--    v  ← MaybeT (use g_aim)
--    os ← uses (g_world.w_map) (valuesAt v)
--    pure (f v os)
--
--
--withAim ∷ (V2 Int → [Object] → StateT Game C.Curses ()) → StateT Game C.Curses ()
--withAim f = withAimOrElse f (pure ())

--------------------------------------------------------------------------------

renderNormal ∷ StateT Game C.Curses ()
renderNormal = do
    w  ← uses (g_world.w_map) width
    d  ← uses (g_world.w_map.wm_data) (fmap last)
    v  ← use (g_world.w_vis)
    p  ← use (g_world.w_active.e_position)
    t  ← uses (g_world.w_team) (fmap (view e_position))
    s  ← use (g_world.w_status)
    
    doRender $ do
        sequence [ drawMap (view o_symbol) (view o_material) w d v
                 , drawPlayer p
                 , drawTeam t
                 --, maybe (pure (pure ())) drawAim ma
                 ]
                 >>= updateMain . foldl1 (>>)
        drawHud s >>= updateHud


renderMessage ∷ String → StateT Game C.Curses ()
renderMessage msg = doRender (drawHud msg >>= updateHud)


renderConversation ∷ ConversationNode → StateT Game C.Curses ()
renderConversation (TalkNode s _)   = do
    w ← use g_conversationWindow
    lift $ do
        clearConversationWindow w 
        drawConversationWindow 0 "Carla" s w
renderConversation (ListenNode s _) = do
    w ← use g_conversationWindow
    lift $ do
        clearConversationWindow w
        drawConversationWindow 1 "<NAME CODE INCOMPL>" s w
renderConversation (ChoiceNode _ _) = do
    w ← use g_conversationWindow
    cw ← use g_choiceWindow
    lift $ do
        clearConversationWindow w
        drawChoiceWindow cw   -- TODO MOVE this to Actual choice node, to use the fucking model!
renderConversation _ = pure () -- We'll never end up here


renderComputer ∷ (MonadRender r) ⇒ String → ComputerData → r ()
renderComputer a cd  = updateInteraction $ do
    drawAnswer
    drawPrompt

    C.moveCursor 2 3
    C.drawString (view cd_input cd <> "                                                          ") 
    where
        drawAnswer  = C.moveCursor 1 1 *> C.drawString (a <> "                                        ")
        drawPrompt  = C.moveCursor 2 1 *> C.drawString "> "


