{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Renderer
( MonadRender(..)
, RendererF
, RendererData
, initRenderer
, runRenderer

, drawMap
, drawPlayer
, drawAim
) where

import Control.Lens
import Control.Monad.Reader
import Linear

import UI.NCurses.Class
import qualified UI.NCurses       as Curses
import qualified Data.Vector      as Vec

import Dreamnet.World

--------------------------------------------------------------------------------

data Styles = Styles {
      _s_mapUnknown ∷ [Curses.Attribute]
    , _s_mapKnown   ∷ [Curses.Attribute]
    , _s_mapVisible ∷ [Curses.Attribute]

    , _s_aim ∷ [Curses.Attribute]
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
    drawCharAt   ∷ (Integral a) ⇒ a → a → Char → [Curses.Attribute] → r ()
    drawStringAt ∷ (Integral a) ⇒ a → a → String → r ()
    swap         ∷ r ()
    

newtype RendererF a = RendererF { runRendererF ∷ ReaderT RendererData Curses.Curses a }
                    deriving (Functor, Applicative, Monad, MonadReader RendererData, MonadCurses)


instance MonadRender RendererF where
    drawCharAt x y c s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawGlyph (Curses.Glyph c s)
    drawStringAt x y s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawString s
    swap = liftCurses Curses.render


initRenderer ∷ Curses.Curses (World → RendererData)
initRenderer = do
    c1 ← Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  1
    c2 ← Curses.newColorID  Curses.ColorWhite  Curses.ColorBlack  2
    c3 ← Curses.newColorID  Curses.ColorGreen  Curses.ColorBlack  3
    return $ RendererData $ Styles {
          _s_mapUnknown = [Curses.AttributeColor c1, Curses.AttributeDim]
        , _s_mapKnown   = [Curses.AttributeColor c1, Curses.AttributeDim]
        , _s_mapVisible = [Curses.AttributeColor c2]
        , _s_aim        = [Curses.AttributeColor Curses.defaultColorID]
        }


runRenderer ∷ RendererData → RendererF () → Curses.Curses ()
runRenderer rd = flip runReaderT rd . runRendererF
    
--------------------------------------------------------------------------------

drawTile ∷ (MonadRender r) ⇒ Map → Int → (Char, Visibility) → r ()
drawTile m i (c, v) = do
    let (V2 x y) = coordLin i m
    unknown ← view (re_styles.s_mapUnknown)
    known   ← view (re_styles.s_mapKnown)
    visible ← view (re_styles.s_mapVisible)
    uncurry (drawCharAt x y) $ case v of
         Unknown → (' ', unknown)
         Known   → (c,   known)
         Visible → (c,   visible)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    m   ← view (re_world.w_map)
    vis ← view (re_world.w_visible)
    Vec.imapM_ (drawTile m) $ Vec.zip (m^.m_data) vis
    where


drawPlayer ∷ (MonadRender r) ⇒ r ()
drawPlayer = do
    (V2 x y) ← view (re_world.w_playerPos)
    drawCharAt x y '@' []


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    s  ← view (re_styles.s_aim)
    ma ← view (re_world.w_aim)
    case ma of
        Just (V2 x y) → drawCharAt x y '╋' s
        _             → return ()
         


debugPrint ∷ (MonadRender r) ⇒ String → r ()
debugPrint = drawStringAt 41 2


messagePrint ∷ (MonadRender r) ⇒ String → r ()
messagePrint = drawStringAt 40 2
