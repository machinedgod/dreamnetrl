{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Renderer
( Material
, Styles
, s_materials
, s_visibilityVisible

, RenderAction
, MonadRender(..)
, RendererF

, RendererEnvironment
, rd_styles

, initRenderer
, runRenderer

, lookupMaterial
, drawMap
, drawPlayer
, drawTeam
, drawAim
, drawHud
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Linear               (V2(V2))
import Data.Semigroup       ((<>))
import Data.Maybe           (fromMaybe)

import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.CoordVector
import Dreamnet.Visibility

--------------------------------------------------------------------------------

type Material = [C.Attribute]

data Styles = Styles {
      _s_materials ∷ M.Map String Material
    , _s_unknown   ∷ Material
    , _s_playerAim ∷ Material

    , _s_visibilityUnknown ∷ Material
    , _s_visibilityKnown   ∷ Material
    , _s_visibilityVisible ∷ Material

    --, _s_colorRed     ∷ C.ColorID
    , _s_colorGreen   ∷ C.ColorID
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

type RenderAction = C.Update ()

class (MonadState RendererEnvironment r) ⇒ MonadRender r where
    updateMain ∷ RenderAction → r ()
    updateHud ∷ RenderAction → r ()
    updateInteraction ∷ RenderAction → r ()
    

-- TODO double buffering
newtype RendererF a = RendererF { runRendererF ∷ StateT RendererEnvironment C.Curses a }
                    deriving (Functor, Applicative, Monad, MonadState RendererEnvironment)


instance MonadRender RendererF where
    updateMain ac = use rd_mainWindow >>=  RendererF . lift . (`C.updateWindow` ac)

    updateHud ac = use rd_hudWindow >>= RendererF . lift . (`C.updateWindow` ac)

    updateInteraction ac = use rd_interactionWindow >>= RendererF . lift  . (`C.updateWindow` ac)


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
                cRed     ← C.newColorID  C.ColorRed      C.ColorBlack  1
                cGreen   ← C.newColorID  C.ColorGreen    C.ColorBlack  2
                cYellow  ← C.newColorID  C.ColorYellow   C.ColorBlack  3
                cBlue    ← C.newColorID  C.ColorBlue     C.ColorBlack  4
                cMagenta ← C.newColorID  C.ColorMagenta  C.ColorBlack  5
                cCyan    ← C.newColorID  C.ColorCyan     C.ColorBlack  6
                cWhite   ← C.newColorID  C.ColorWhite    C.ColorBlack  7

                return Styles {
                         _s_materials = M.fromList
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
                       , _s_unknown   = [ C.AttributeColor cMagenta, C.AttributeBold, C.AttributeBlink ]
                       , _s_playerAim = [ C.AttributeColor cGreen, C.AttributeBold]

                       , _s_visibilityUnknown = []
                       , _s_visibilityKnown   = [ C.AttributeColor cBlue,  C.AttributeDim ]
                       , _s_visibilityVisible = [ C.AttributeColor cWhite ]
                       --, _s_visibilityVisible = [ C.AttributeColor cWhite, C.AttributeDim ]

                       --, _s_colorRed     = cRed    
                       , _s_colorGreen   = cGreen
                       --, _s_colorYellow  = cYellow 
                       --, _s_colorBlue    = cBlue   
                       , _s_colorMagenta = cMagenta
                       --, _s_colorCyan    = cCyan   
                       --, _s_colorWhite   = cWhite  
                       }


runRenderer ∷ RendererEnvironment → RendererF a → C.Curses (a, RendererEnvironment)
runRenderer rd f = runStateT (runRendererF f) rd
    
--------------------------------------------------------------------------------

lookupMaterial ∷ (MonadRender r) ⇒ String → r Material
lookupMaterial n = use (rd_styles.s_unknown) >>= \umat →
    uses (rd_styles.s_materials) (fromMaybe umat . M.lookup n)


draw ∷ (Integral a) ⇒ V2 a → Char → Material → RenderAction
draw (V2 x y) c m = do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c m)


drawMap ∷ (MonadRender r) ⇒ (a → Char) → (a → Material) → Width → V.Vector a → V.Vector Visibility → r RenderAction
drawMap chf matf w dat vis = do
    u ← use (rd_styles.s_visibilityUnknown)
    k ← use (rd_styles.s_visibilityKnown)
    pure $ V.imapM_ (drawTile u k) $ V.zip dat vis
    where
        -- TODO I wonder if I can somehow reimplement this without relying on
        -- pattern matching the Visibility (using Ord, perhaps?)
        --drawTile _ k i ([], _) = uncurry (draw $ coordLin w i) $ ('?', k)
        drawTile u k i (o, v) = uncurry (draw $ coordLin' (fromIntegral w) i) $
                                     case v of
                                         Unknown → (' ', u)
                                         Known   → (chf o, k)
                                         Visible → (chf o, matf o)


drawPlayer ∷ (MonadRender r) ⇒ V2 Int → r RenderAction
drawPlayer v = use (rd_styles.s_colorMagenta) >>= drawCharacter v


drawTeam ∷ (Applicative r, MonadRender r) ⇒ [V2 Int] → r RenderAction
drawTeam vs = do
    s ← use (rd_styles.s_colorGreen)
    fmap sequence_ $ traverse (flip drawCharacter s) vs


drawCharacter ∷ (MonadRender r) ⇒ V2 Int → C.ColorID → r RenderAction
drawCharacter v cid = do
    pure $ draw v '@' [C.AttributeColor cid]


drawAim ∷ (MonadRender r) ⇒ V2 Int → r RenderAction
drawAim v = do
    s  ← use (rd_styles.s_playerAim)
    pure $ draw v '×' s
         

drawHud ∷ (MonadRender r) ⇒ String → r RenderAction
drawHud s = do
    pure $ do
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

--------------------------------------------------------------------------------
