{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Dreamnet.Renderer
( MonadRender(..)
, RendererF
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

class (MonadReader World r) ⇒ MonadRender r where
    getColors    ∷ r [Curses.ColorID]
    drawCharAt   ∷ (Integral a) ⇒ a → a → Char → [Curses.Attribute] → r ()
    drawStringAt ∷ (Integral a) ⇒ a → a → String → r ()
    swap         ∷ r ()
    

newtype RendererF a = RendererF { runRendererF ∷ ReaderT World Curses.Curses a }
                    deriving (Functor, Applicative, Monad, MonadReader World, MonadCurses)


instance MonadRender RendererF where
    getColors = liftCurses $
        sequence [ Curses.newColorID  Curses.ColorWhite  Curses.ColorBlack  1
                 , Curses.newColorID  Curses.ColorBlue   Curses.ColorBlack  2
                 ] 
    drawCharAt x y c s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawGlyph (Curses.Glyph c s)
    drawStringAt x y s = liftCurses $ Curses.defaultWindow >>= \w → Curses.updateWindow w $ do
        Curses.moveCursor (fromIntegral y) (fromIntegral x)
        Curses.drawString s
    swap = liftCurses Curses.render


runRenderer ∷ World → RendererF () → Curses.Curses ()
runRenderer w = flip runReaderT w . runRendererF
    
--------------------------------------------------------------------------------

drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    m   ← view w_map
    vis ← view w_visible
    Vec.imapM_ (drawTile m) $ Vec.zip (m^.m_data) vis
    where
        drawTile ∷ (MonadRender r) ⇒ Map → Int → (Char, Visibility) → r ()
        drawTile m i (c, v) = do
            let (V2 x y) = coordLin i m
            cs ← getColors
            uncurry (drawCharAt x y) $ case v of
                 Unknown → (' ', [Curses.AttributeColor (cs !! 0)])
                 Known   → (c,   [Curses.AttributeColor (cs !! 1), Curses.AttributeDim])
                 Visible → (c,   [Curses.AttributeColor (cs !! 0)])


drawPlayer ∷ (MonadRender r) ⇒ r ()
drawPlayer = do
    (V2 x y) ← view w_playerPos
    drawCharAt x y '@' []


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    (V2 x y) ← view w_aim
    drawCharAt x y '╋' []


debugPrint ∷ (MonadRender r) ⇒ String → r ()
debugPrint = drawStringAt 41 2


messagePrint ∷ (MonadRender r) ⇒ String → r ()
messagePrint = drawStringAt 40 2
