{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Renderer
( MonadRender(..)
, RendererF
, RendererData
, re_world

, initRenderer
, runRenderer

, drawMap
, drawPlayer
, drawAim
, drawHud
, drawExamination
, drawConversationWindow
, drawInteraction
) where

import Control.Lens
import Control.Monad.Reader
import Linear

import UI.NCurses.Class
import qualified UI.NCurses       as Curses
import qualified Data.Vector      as Vec

import qualified Dreamnet.TileMap as TMap
import Dreamnet.World
import Dreamnet.Character

--------------------------------------------------------------------------------

data Styles = Styles {
      _s_mapUnknown ∷ [Curses.Attribute]
    , _s_mapKnown   ∷ [Curses.Attribute]
    , _s_mapVisible ∷ [Curses.Attribute]

    , _s_aim ∷ [Curses.Attribute]
    , _s_playerCharacters ∷ [Curses.Attribute]
    }

makeLenses ''Styles

-- TODO maybe world shouldn't be mixed into init block?
data RendererData = RendererData {
      _re_styles ∷ Styles
    , _re_mainWindow          ∷ Curses.Window
    , _re_hudWindow           ∷ Curses.Window
    , _re_examineWindow       ∷ Curses.Window
    , _re_interactionWindow   ∷ Curses.Window
    , _re_conversationWindow0 ∷ Curses.Window
    , _re_conversationWindow1 ∷ Curses.Window
    -- TODO add up to eight conversation windows
    , _re_world ∷ World
    }

makeLenses ''RendererData

--------------------------------------------------------------------------------

class (MonadReader RendererData r) ⇒ MonadRender r where
    updateWindow ∷ Curses.Window → Curses.Update () → r ()
    swap         ∷ r ()
    

newtype RendererF a = RendererF { runRendererF ∷ ReaderT RendererData Curses.Curses a }
                    deriving (Functor, Applicative, Monad, MonadReader RendererData, MonadCurses)


instance MonadRender RendererF where
    updateWindow w = liftCurses . Curses.updateWindow w
    swap = liftCurses Curses.render


initRenderer ∷ Curses.Curses (World → RendererData)
initRenderer = do
    -- TODO respond to resize events and resize all the windows!
    --      this should happen automatically and be inacessible by API
    (rows, columns) ← Curses.screenSize

    let hudWidth   = columns
        hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight

    mainW          ← Curses.newWindow mainHeight mainWidth 0 0
    hudW           ← Curses.newWindow hudHeight hudWidth mainHeight 0

    examineW       ← let w = 50
                         h = 10
                         x = (mainWidth - w) `div` 2
                         y = (mainHeight - h) `div` 2
                     in  Curses.newWindow h w y x
    conversationW0 ← let w = mainWidth `div` 3
                         h = mainHeight `div` 3
                         x = 0
                         y = mainHeight `div` 3 * 2
                     in  Curses.newWindow h w y x
    conversationW1 ← let w = mainWidth `div` 3
                         h = mainHeight `div` 3
                         x = mainWidth `div` 3 * 2
                         y = 0
                     in  Curses.newWindow h w y x
    interactionW   ← Curses.newWindow (rows - 4) (columns - 4) 2 2

    styles ← createStyles 

    return $ RendererData styles mainW hudW examineW interactionW conversationW0 conversationW1
        where
            createStyles = do
                c1 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  1
                c2 ← Curses.newColorID  Curses.ColorWhite  Curses.ColorBlack  2
                c3 ← Curses.newColorID  Curses.ColorRed    Curses.ColorBlack  3
                --c3 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  3
                return $ Styles {
                           _s_mapUnknown       = [Curses.AttributeColor c1, Curses.AttributeDim]
                         , _s_mapKnown         = [Curses.AttributeColor c1, Curses.AttributeDim]
                         , _s_mapVisible       = [Curses.AttributeColor c2, Curses.AttributeBold]
                         , _s_aim              = [Curses.AttributeColor c3]
                         , _s_playerCharacters = [Curses.AttributeColor c1, Curses.AttributeBold]
                         }


runRenderer ∷ RendererData → RendererF () → Curses.Curses ()
runRenderer rd = flip runReaderT rd . runRendererF
    
--------------------------------------------------------------------------------

drawCharAt ∷ (Integral a) ⇒ V2 a → Char → [Curses.Attribute] → Curses.Update ()
drawCharAt (V2 x y) c s = do
    Curses.moveCursor (fromIntegral y) (fromIntegral x)
    Curses.drawGlyph (Curses.Glyph c s)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    w   ← view (re_mainWindow)
    m   ← view (re_world.w_map)
    vis ← view (re_world.w_visible)

    unknown ← view (re_styles.s_mapUnknown)
    known   ← view (re_styles.s_mapKnown)
    visible ← view (re_styles.s_mapVisible)
    updateWindow w $
        Vec.imapM_ (drawTile unknown known visible m) $ Vec.zip (m^.TMap.m_data) vis
    where
        drawTile us ks vs m i (c, v) = do
            uncurry (drawCharAt $ TMap.coordLin m i) $ case v of
                 Unknown → (' ', us)
                 Known   → (c,   ks)
                 Visible → (c,   vs)


--drawObject ∷ (MonadRender r) ⇒ V2 Int → Object → r ()
--drawObject v Computer      = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v Person        = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Door True)   = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Door False)  = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Container t) = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Dispenser t) = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Stairs t)    = drawCharAt v 'o' [ Curses.AttributeBlink ]
--drawObject v (Prop t)      = drawCharAt v 'o' [ Curses.AttributeBlink ]


drawPlayer ∷ (MonadRender r) ⇒ r ()
drawPlayer = do
    w ← view (re_mainWindow)
    s ← view (re_styles.s_playerCharacters)
    v ← view (re_world.w_playerPos)
    updateWindow w $ drawCharAt v '@' s


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    w  ← view (re_mainWindow)
    s  ← view (re_styles.s_aim)
    ma ← view (re_world.w_aim)
    case ma of
        Just v → updateWindow w $ drawCharAt v '×' s
        _      → return ()
         

drawHud ∷ (MonadRender r) ⇒ r ()
drawHud = do
    w ← view (re_hudWindow)
    s ← view (re_world.w_status)
    updateWindow w $ do
        Curses.moveCursor 2 2
        Curses.drawString $ "Status: " ++ s


-- TODO add title & shit
drawExamination ∷ (MonadRender r) ⇒ String → r ()
drawExamination s = do
    w ← view re_examineWindow
    updateWindow w $ do
        Curses.clear
        Curses.drawBorder (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '╭' [])
                          (Just $ Curses.Glyph '╮' [])
                          (Just $ Curses.Glyph '╰' [])
                          (Just $ Curses.Glyph '╯' [])
        Curses.moveCursor 2 2

        -- TODO TRIM and draw lines wiht 'words' to fit
        Curses.drawString s


-- TODO pass rendering to specific item
drawInteraction ∷ (MonadRender r) ⇒ r ()
drawInteraction = do
    w ← view re_interactionWindow
    updateWindow w $ do
        Curses.clear
        Curses.drawBox (Just $ Curses.Glyph 'i' [])
                       (Just $ Curses.Glyph 'i' [])


-- TODO REDO as multiple windows
drawConversationWindow ∷ (MonadRender r) ⇒ Int → Character → String → r ()
drawConversationWindow i c s = do
    w ← view $ case i of
                   0 → re_conversationWindow0
                   1 → re_conversationWindow1
                   2 → re_conversationWindow0

    updateWindow w (drawWindow c s)
    where
        drawWindow c s = do
            Curses.clear
            Curses.drawBorder (Just $ Curses.Glyph '│' [])
                              (Just $ Curses.Glyph '│' [])
                              (Just $ Curses.Glyph '─' [])
                              (Just $ Curses.Glyph '─' [])
                              (Just $ Curses.Glyph '╭' [])
                              (Just $ Curses.Glyph '╮' [])
                              (Just $ Curses.Glyph '╰' [])
                              (Just $ Curses.Glyph '╯' [])

            (rows, columns) ← Curses.windowSize

            drawNameBox columns (c^.ch_name)
            drawText s
            drawWidgets rows columns
        drawNameBox cols n = do
            let trimmedName  = take (fromIntegral cols - 7) n
            Curses.moveCursor 2 0
            Curses.drawGlyph (Curses.Glyph '├' [])
            Curses.moveCursor 1 2
            Curses.drawString trimmedName
            Curses.moveCursor 0 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '┬' [])
            Curses.moveCursor 2 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '╯' [])
            Curses.moveCursor 2 1
            Curses.drawLineH (Just $ Curses.Glyph '─' []) (fromIntegral (length trimmedName) + 2)
            Curses.moveCursor 1 (fromIntegral (length trimmedName) + 3)
            Curses.drawGlyph (Curses.Glyph '│' [])
        drawText s = do
            Curses.moveCursor 3 2
            Curses.drawString s
        drawWidgets rows cols = do
            Curses.moveCursor 1 (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▲' [])
            Curses.moveCursor (rows - 2) (cols - 3)
            Curses.drawGlyph (Curses.Glyph '▼' [])
            

