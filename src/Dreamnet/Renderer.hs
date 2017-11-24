{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Renderer
( module Control.Lens
, module Control.Monad.Reader

, MonadRender(..)
, RendererF

, RendererData
, rd_styles
, rd_mainWindow
, rd_hudWindow
, rd_examineWindow
, rd_interactionWindow
, rd_choiceWindow
, rd_conversationWindow0
, rd_conversationWindow1

, RendererEnvironment
, re_data
, re_world

, initRenderer
, runRenderer

, drawMap
, drawPlayer
, drawAim
, drawHud
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

data RendererData = RendererData {
      _rd_styles ∷ Styles
    , _rd_mainWindow          ∷ Curses.Window
    , _rd_hudWindow           ∷ Curses.Window
    , _rd_examineWindow       ∷ Curses.Window
    , _rd_interactionWindow   ∷ Curses.Window
    , _rd_choiceWindow        ∷ Curses.Window
    , _rd_conversationWindow0 ∷ Curses.Window
    , _rd_conversationWindow1 ∷ Curses.Window
    }

makeLenses ''RendererData


data RendererEnvironment = RendererEnvironment {
      _re_data ∷ RendererData 
    , _re_world ∷ World
    }

makeLenses ''RendererEnvironment

--------------------------------------------------------------------------------

class (MonadReader RendererEnvironment r) ⇒ MonadRender r where
    updateWindow ∷ Curses.Window → Curses.Update () → r ()
    swap         ∷ r ()
    

newtype RendererF a = RendererF { runRendererF ∷ ReaderT RendererEnvironment Curses.Curses a }
                    deriving (Functor, Applicative, Monad, MonadReader RendererEnvironment, MonadCurses)


instance MonadRender RendererF where
    updateWindow w = liftCurses . Curses.updateWindow w
    swap = liftCurses Curses.render


initRenderer ∷ Curses.Curses RendererData
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

    let lowLeftW = mainWidth `div` 3
        lowLeftH = mainHeight `div` 3
        lowLeftX = 0
        lowLeftY = mainHeight `div` 3 * 2

        topRightW = mainWidth `div` 3
        topRightH = mainHeight `div` 3
        topRightX = mainWidth `div` 3 * 2
        topRightY = 0

    examineW       ← let w = 50
                         h = 10
                         x = (mainWidth - w) `div` 2
                         y = (mainHeight - h) `div` 2
                     in  Curses.newWindow h w y x
    choiceW        ← Curses.newWindow lowLeftH lowLeftW lowLeftX lowLeftY
    conversationW0 ← Curses.newWindow lowLeftH lowLeftW lowLeftX lowLeftY
    conversationW1 ← Curses.newWindow topRightH topRightW topRightY topRightX
    interactionW   ← Curses.newWindow (rows - 4) (columns - 4) 2 2

    styles ← createStyles 

    return $ RendererData styles mainW hudW examineW interactionW choiceW conversationW0 conversationW1
        where
            createStyles = do
                c1 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  1
                c2 ← Curses.newColorID  Curses.ColorWhite  Curses.ColorBlack  2
                c3 ← Curses.newColorID  Curses.ColorRed    Curses.ColorBlack  3
                --c3 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  3
                return Styles {
                         _s_mapUnknown       = [Curses.AttributeColor c1, Curses.AttributeDim]
                       , _s_mapKnown         = [Curses.AttributeColor c1, Curses.AttributeDim]
                       , _s_mapVisible       = [Curses.AttributeColor c2, Curses.AttributeBold]
                       , _s_aim              = [Curses.AttributeColor c3]
                       , _s_playerCharacters = [Curses.AttributeColor c1, Curses.AttributeBold]
                       }


runRenderer ∷ RendererData → World → RendererF () → Curses.Curses ()
runRenderer rd w f = let re = RendererEnvironment rd w
                     in  runReaderT (runRendererF f) re 
    
--------------------------------------------------------------------------------

drawCharAt ∷ (Integral a) ⇒ V2 a → Char → [Curses.Attribute] → Curses.Update ()
drawCharAt (V2 x y) c s = do
    Curses.moveCursor (fromIntegral y) (fromIntegral x)
    Curses.drawGlyph (Curses.Glyph c s)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    w   ← view (re_data.rd_mainWindow)
    m   ← view (re_world.w_map)
    vis ← view (re_world.w_visible)

    unknown ← view (re_data.rd_styles.s_mapUnknown)
    known   ← view (re_data.rd_styles.s_mapKnown)
    visible ← view (re_data.rd_styles.s_mapVisible)
    updateWindow w $
        Vec.imapM_ (drawTile unknown known visible m) $ Vec.zip (m^.TMap.m_data) vis
    where
        drawTile us ks vs m i (c, v) = uncurry (drawCharAt $ TMap.coordLin m i) $
            case v of
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
    w ← view (re_data.rd_mainWindow)
    s ← view (re_data.rd_styles.s_playerCharacters)
    v ← view (re_world.w_playerPos)
    updateWindow w $ drawCharAt v '@' s


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    w  ← view (re_data.rd_mainWindow)
    s  ← view (re_data.rd_styles.s_aim)
    ma ← view (re_world.w_aim)
    case ma of
        Just v → updateWindow w $ drawCharAt v '×' s
        _      → return ()
         

drawHud ∷ (MonadRender r) ⇒ r ()
drawHud = do
    w ← view (re_data.rd_hudWindow)
    s ← view (re_world.w_status)
    updateWindow w $ do
        Curses.moveCursor 2 2
        Curses.drawString $ "Status: " ++ s

