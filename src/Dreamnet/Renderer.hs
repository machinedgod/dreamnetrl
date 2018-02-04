{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Renderer
( MonadRender(..)
, RendererF

, Styles
, s_materials
, s_visibilityVisible

, RendererEnvironment
, rd_styles
, rd_mainWindow
, rd_hudWindow
, rd_interactionWindow

, initRenderer
, runRenderer

, drawMap
, drawPlayer
, drawAim
, drawHud
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Linear               (V2(V2))
import Data.Semigroup       ((<>))

import UI.NCurses.Class
import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.Visibility

--------------------------------------------------------------------------------

data Styles = Styles {
      _s_materials ∷ M.Map String [C.Attribute]
    , _s_unknown   ∷ [C.Attribute]
    , _s_playerAim ∷ [C.Attribute]

    , _s_visibilityUnknown ∷ [C.Attribute]
    , _s_visibilityKnown   ∷ [C.Attribute]
    , _s_visibilityVisible ∷ [C.Attribute]

    --, _s_colorRed     ∷ C.ColorID
    --, _s_colorGreen   ∷ C.ColorID
    --, _s_colorYellow  ∷ C.ColorID
    --, _s_colorBlue    ∷ C.ColorID
    , _s_colorMagenta ∷ C.ColorID
    --, _s_colorCyan    ∷ C.ColorID
    --, _s_colorWhite   ∷ C.ColorID
    }

makeLenses ''Styles

data RendererEnvironment = RendererEnvironment {
      _rd_styles              ∷ Styles

    , _rd_mainWindow          ∷ C.Window
    , _rd_hudWindow           ∷ C.Window
    , _rd_interactionWindow   ∷ C.Window
    }

makeLenses ''RendererEnvironment

--------------------------------------------------------------------------------

class (MonadState RendererEnvironment r) ⇒ MonadRender r where
    updateWindow ∷ C.Window → C.Update () → r ()
    

-- TODO double buffering
newtype RendererF a = RendererF { runRendererF ∷ StateT RendererEnvironment C.Curses a }
                    deriving (Functor, Applicative, Monad, MonadState RendererEnvironment, MonadCurses)


instance MonadCurses (StateT RendererEnvironment C.Curses) where
    liftCurses = lift


instance MonadRender RendererF where
    updateWindow w = liftCurses . C.updateWindow w


initRenderer ∷ C.Curses RendererEnvironment
initRenderer = do
    -- TODO respond to resize events and resize all the windows!
    --      this should happen automatically and be inacessible by API
    (rows, columns) ← C.screenSize

    let hudWidth   = columns
        hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight
    mainWin ← C.newWindow mainHeight mainWidth 0 0
    hudWin  ← C.newWindow hudHeight hudWidth mainHeight 0

    let interactW = columns - 4
        interactH = rows - 4
        interactX = 2
        interactY = 2

    interactionWin   ← C.newWindow interactH interactW interactY interactX

    styles ← C.maxColor >>= createStyles 

    return $ RendererEnvironment styles mainWin hudWin interactionWin
        where
            createStyles mc
                | mc > 0 && mc <= 8    = createStyles8Colors    -- (And disable lighting)
                | mc > 8 && mc <= 255  = createStyles8Colors   -- (And disable lighting)
                | mc >= 255            = createStyles8Colors  -- (And enable lighting!)
                | otherwise            = error "Your terminal doesn't support color! I haven't had time to make things render without colors yet, sorry :-("
            createStyles8Colors = do 
                cRed     ←  C.newColorID  C.ColorRed      C.ColorBlack  1
                cGreen   ←  C.newColorID  C.ColorGreen    C.ColorBlack  2
                cYellow  ←  C.newColorID  C.ColorYellow   C.ColorBlack  3
                cBlue    ←  C.newColorID  C.ColorBlue     C.ColorBlack  4
                cMagenta ←  C.newColorID  C.ColorMagenta  C.ColorBlack  5
                cCyan    ←  C.newColorID  C.ColorCyan     C.ColorBlack  6
                cWhite   ←  C.newColorID  C.ColorWhite    C.ColorBlack  7

                let materials = M.fromList
                        [ ("wood"           , [ C.AttributeColor cYellow, C.AttributeDim  ])
                        , ("metal"          , [ C.AttributeColor cCyan,   C.AttributeBold ])
                        , ("blue plastic"   , [ C.AttributeColor cCyan,   C.AttributeDim  ])
                        , ("red plastic"    , [ C.AttributeColor cRed,    C.AttributeDim  ])
                        , ("green plastic"  , [ C.AttributeColor cGreen,  C.AttributeDim  ])
                        , ("ceramics"       , [ C.AttributeColor cWhite,  C.AttributeDim  ])
                        , ("green light"    , [ C.AttributeColor cGreen,  C.AttributeBold ])
                        , ("yellow light"   , [ C.AttributeColor cYellow, C.AttributeBold ])
                        , ("red light"      , [ C.AttributeColor cRed,    C.AttributeBold ])
                        ]
                    matUnknown = [ C.AttributeColor cMagenta, C.AttributeBold, C.AttributeBlink ]
                return Styles {
                         _s_materials = materials
                       , _s_unknown   = matUnknown
                       , _s_playerAim = [ C.AttributeColor cGreen, C.AttributeBold]

                       , _s_visibilityUnknown = []
                       , _s_visibilityKnown   = [ C.AttributeColor cBlue,  C.AttributeDim ]
                       , _s_visibilityVisible = [ C.AttributeColor cWhite ]
                       --, _s_visibilityVisible = [ C.AttributeColor cWhite, C.AttributeDim ]

                       --, _s_colorRed     = cRed    
                       --, _s_colorGreen   = cGreen  
                       --, _s_colorYellow  = cYellow 
                       --, _s_colorBlue    = cBlue   
                       , _s_colorMagenta = cMagenta
                       --, _s_colorCyan    = cCyan   
                       --, _s_colorWhite   = cWhite  
                       }


runRenderer ∷ RendererEnvironment → RendererF a → C.Curses (a, RendererEnvironment)
runRenderer rd f = runStateT (runRendererF f) rd
    
--------------------------------------------------------------------------------

drawCharAt ∷ (Integral a) ⇒ V2 a → Char → [C.Attribute] → C.Update ()
drawCharAt (V2 x y) c s = do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c s)


drawMap ∷ (MonadRender r) ⇒ (a → Char) → (a → [C.Attribute]) → WorldMap a Visibility → r ()
drawMap chf matf m = do
    u ← use (rd_styles.s_visibilityUnknown)
    k ← use (rd_styles.s_visibilityKnown)
    w ← use rd_mainWindow 
    updateWindow w $
        V.imapM_ (drawTile u k) $ V.zip (m^.wm_data) (m^.wm_visible)
    where
        -- TODO I wonder if I can somehow reimplement this without relying on
        -- pattern matching the Visibility (using Ord, perhaps?)
        drawTile u k i (os, v) = uncurry (drawCharAt $ coordLin m i) $
                                     case v of
                                         Unknown → (' ', u)
                                         Known   → (chf (last os), k)
                                         Visible → (chf (last os), matf (last os))


drawPlayer ∷ (MonadRender r) ⇒ V2 Int → r ()
drawPlayer v = do
    w ← use rd_mainWindow
    s ← uses (rd_styles.s_colorMagenta) C.AttributeColor
    updateWindow w $ drawCharAt v '@' [s]


drawAim ∷ (MonadRender r) ⇒ V2 Int → r ()
drawAim v = do
    w  ← use rd_mainWindow
    s  ← use (rd_styles.s_playerAim)
    updateWindow w $ drawCharAt v '×' s
         

drawHud ∷ (MonadRender r) ⇒ String → r ()
drawHud s = do
    w ← use rd_hudWindow
    updateWindow w $ do
        C.drawBorder (Just $ C.Glyph '│' [])
                          (Just $ C.Glyph '│' [])
                          (Just $ C.Glyph '─' [])
                          (Just $ C.Glyph '─' [])
                          (Just $ C.Glyph '╭' [])
                          (Just $ C.Glyph '╮' [])
                          (Just $ C.Glyph '╰' [])
                          (Just $ C.Glyph '╯' [])

        C.moveCursor 2 2
        let st = take 80 $ s <> repeat '.'
        C.drawString $ "Status: " <> st

