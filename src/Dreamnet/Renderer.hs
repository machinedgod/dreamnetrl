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
, messagePrint
) where

import Control.Lens
import Control.Monad.Reader
import Linear

import UI.NCurses.Class
import qualified UI.NCurses       as Curses
import qualified Data.Vector      as Vec

import qualified Dreamnet.TileMap as TMap
import Dreamnet.World

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
    , _re_world ∷ World
    }

makeLenses ''RendererData

--------------------------------------------------------------------------------

class (MonadReader RendererData r) ⇒ MonadRender r where
    drawCharAt   ∷ (Integral a) ⇒ V2 a → Char → [Curses.Attribute] → r ()
    drawStringAt ∷ (Integral a) ⇒ V2 a → String → r ()
    swap         ∷ r ()
    

newtype RendererF a = RendererF { runRendererF ∷ ReaderT RendererData Curses.Curses a }
                    deriving (Functor, Applicative, Monad, MonadReader RendererData, MonadCurses)


instance MonadRender RendererF where
    drawCharAt (V2 x y) c s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawGlyph (Curses.Glyph c s)
    drawStringAt (V2 x y) s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawString s
    swap = liftCurses Curses.render


initRenderer ∷ Curses.Curses (World → RendererData)
initRenderer = do
    c1 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  1
    c2 ← Curses.newColorID  Curses.ColorWhite  Curses.ColorBlack  2
    c3 ← Curses.newColorID  Curses.ColorRed    Curses.ColorBlack  3
    --c3 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  3
    return $ RendererData $ Styles {
          _s_mapUnknown       = [Curses.AttributeColor c1, Curses.AttributeDim]
        , _s_mapKnown         = [Curses.AttributeColor c1, Curses.AttributeDim]
        , _s_mapVisible       = [Curses.AttributeColor c2, Curses.AttributeBold]
        , _s_aim              = [Curses.AttributeColor c3]
        , _s_playerCharacters = [Curses.AttributeColor c1, Curses.AttributeBold]
        }


runRenderer ∷ RendererData → RendererF () → Curses.Curses ()
runRenderer rd = flip runReaderT rd . runRendererF
    
--------------------------------------------------------------------------------

drawTile ∷ (MonadRender r) ⇒ TMap.TileMap → Int → (Char, Visibility) → r ()
drawTile m i (c, v) = do
    unknown ← view (re_styles.s_mapUnknown)
    known   ← view (re_styles.s_mapKnown)
    visible ← view (re_styles.s_mapVisible)
    uncurry (drawCharAt $ TMap.coordLin m i) $ case v of
         Unknown → (' ', unknown)
         Known   → (c,   known)
         Visible → (c,   visible)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    m   ← view (re_world.w_map)
    vis ← view (re_world.w_visible)
    Vec.imapM_ (drawTile m) $ Vec.zip (m^.TMap.m_data) vis


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
    s ← view (re_styles.s_playerCharacters)
    v ← view (re_world.w_playerPos)
    drawCharAt v '@' s


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    s  ← view (re_styles.s_aim)
    ma ← view (re_world.w_aim)
    case ma of
        Just v → drawCharAt v '×' s
        _      → return ()
         

messagePrint ∷ (MonadRender r) ⇒ String → r ()
messagePrint = drawStringAt (V2 2 40)

