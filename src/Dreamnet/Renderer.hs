{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Renderer
( RenderAction
, MonadRender(..)
, RendererF

, RendererEnvironment

, initRenderer
, runRenderer

, drawMap
, drawHud
) where

import Safe                 (atDef)
import Control.Lens         (makeLenses, use, uses)
import Control.Monad.Trans  (lift)
import Control.Monad.State  (MonadState, StateT, runStateT)
import Linear               (V2(V2))
import Data.Semigroup       ((<>))
import Data.Maybe           (fromMaybe)
import Data.Foldable        (traverse_)
import Data.Char            (digitToInt)
import Data.List            (intercalate)

import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.GameState
import Dreamnet.Utils       (lines')
import Dreamnet.CoordVector
import Dreamnet.Visibility

--------------------------------------------------------------------------------

type Material = [C.Attribute]

data Styles = Styles {
      _s_materials ∷ M.Map String Material
    , _s_unknown   ∷ Material
    --, _s_playerAim ∷ Material

    , _s_visibilityUnknown ∷ Material
    , _s_visibilityKnown   ∷ Material
    --, _s_visibilityVisible ∷ Material

    , _s_colorBlack   ∷ C.ColorID
    , _s_colorRed     ∷ C.ColorID
    , _s_colorGreen   ∷ C.ColorID
    , _s_colorYellow  ∷ C.ColorID
    , _s_colorBlue    ∷ C.ColorID
    , _s_colorMagenta ∷ C.ColorID
    , _s_colorCyan    ∷ C.ColorID
    , _s_colorWhite   ∷ C.ColorID
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

newtype RenderAction a = RenderAction { runAction ∷ C.Update a }
                       deriving (Functor, Applicative, Monad)


class (MonadState RendererEnvironment r) ⇒ MonadRender r where
    updateMain ∷ RenderAction () → r ()
    updateHud ∷ RenderAction () → r ()
    updateInteraction ∷ RenderAction () → r ()
    

-- TODO double buffering
newtype RendererF a = RendererF { runRendererF ∷ StateT RendererEnvironment C.Curses a }
                    deriving (Functor, Applicative, Monad, MonadState RendererEnvironment)


instance MonadRender RendererF where
    updateMain ac = use rd_mainWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)

    updateHud ac = use rd_hudWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)

    updateInteraction ac = use rd_interactionWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)


initRenderer ∷ C.Curses RendererEnvironment
initRenderer = do
    -- TODO respond to resize events and resize all the windows!
    --      this should happen automatically and be inacessible by API
    (rows, columns) ← C.screenSize

    let hudWidth   = columns
        hudHeight  = 13
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
                cBlack   ← C.newColorID C.ColorBlack   C.ColorBlack 1
                cRed     ← C.newColorID C.ColorRed     C.ColorBlack 2
                cGreen   ← C.newColorID C.ColorGreen   C.ColorBlack 3
                cYellow  ← C.newColorID C.ColorYellow  C.ColorBlack 4
                cBlue    ← C.newColorID C.ColorBlue    C.ColorBlack 5
                cMagenta ← C.newColorID C.ColorMagenta C.ColorBlack 6
                cCyan    ← C.newColorID C.ColorCyan    C.ColorBlack 7
                cWhite   ← C.newColorID C.ColorWhite   C.ColorBlack 8

                return Styles {
                         _s_materials = M.fromList
                            [ ("concrete"       , [ C.AttributeColor cWhite ])
                            , ("grass"          , [ C.AttributeColor cGreen,  C.AttributeDim  ])
                            , ("wood"           , [ C.AttributeColor cYellow, C.AttributeDim  ])
                            , ("metal"          , [ C.AttributeColor cCyan,   C.AttributeBold ])
                            , ("blue plastic"   , [ C.AttributeColor cCyan,   C.AttributeDim  ])
                            , ("red plastic"    , [ C.AttributeColor cRed,    C.AttributeDim  ])
                            , ("green plastic"  , [ C.AttributeColor cGreen,  C.AttributeDim  ])
                            , ("ceramics"       , [ C.AttributeColor cWhite,  C.AttributeDim  ])
                            , ("green light"    , [ C.AttributeColor cGreen,  C.AttributeBold ])
                            , ("yellow light"   , [ C.AttributeColor cYellow, C.AttributeBold ])
                            , ("red light"      , [ C.AttributeColor cRed,    C.AttributeBold ])

                            , ("blue"           , [ C.AttributeColor cBlue ])
                            ]
                       , _s_unknown   = [ C.AttributeColor cMagenta, C.AttributeBold, C.AttributeBlink ]
                       --, _s_playerAim = [ C.AttributeColor cGreen, C.AttributeBold]

                       , _s_visibilityUnknown = []
                       , _s_visibilityKnown   = [ C.AttributeColor cBlue,  C.AttributeDim ]
                       --, _s_visibilityVisible = [ C.AttributeColor cWhite ]
                       --, _s_visibilityVisible = [ C.AttributeColor cWhite, C.AttributeDim ]

                       , _s_colorBlack   = cBlack
                       , _s_colorRed     = cRed
                       , _s_colorGreen   = cGreen
                       , _s_colorYellow  = cYellow
                       , _s_colorBlue    = cBlue
                       , _s_colorMagenta = cMagenta
                       , _s_colorCyan    = cCyan
                       , _s_colorWhite   = cWhite
                       }


runRenderer ∷ RendererEnvironment → RendererF a → C.Curses (a, RendererEnvironment)
runRenderer rd f = runStateT (runRendererF f) rd
    
--------------------------------------------------------------------------------

lookupMaterial ∷ (MonadRender r) ⇒ String → r Material
lookupMaterial n = use (rd_styles.s_unknown) >>= \umat →
    uses (rd_styles.s_materials) (fromMaybe umat . M.lookup n)


draw ∷ (Integral a) ⇒ V2 a → Char → Material → RenderAction ()
draw (V2 x y) c m = RenderAction $ do 
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c m)


drawMap ∷ (MonadRender r) ⇒ (a → Char) → (a → String) → Width → V.Vector a → V.Vector Visibility → r (RenderAction ())
drawMap chf matf w dat vis = do
    u ← use (rd_styles.s_visibilityUnknown)
    k ← use (rd_styles.s_visibilityKnown)

    mats ← V.mapM (lookupMaterial . matf) dat
    pure $ V.imapM_ (drawTile u k) $ V.zip3 (chf <$> dat) mats vis
    where
        -- TODO I wonder if I can somehow reimplement this without relying on
        -- pattern matching the Visibility (using Ord, perhaps?)
        drawTile ∷ [C.Attribute] → [C.Attribute] → Int → (Char, Material, Visibility) → RenderAction ()
        drawTile u k i (c, m, v) = uncurry (draw $ coordLin' (fromIntegral w) i) $
                                     case v of
                                         Unknown → (' ', u)
                                         Known   → (c, k)
                                         Visible → (c, m)


drawHud ∷ (MonadRender r) ⇒ GameState → Int → Int → Word → Int → String → r (RenderAction ())
drawHud gs hms am turns button msg = do
    white   ← use (rd_styles.s_colorWhite)
    green   ← use (rd_styles.s_colorGreen)
    blue ← use (rd_styles.s_colorBlue)
    pure $ RenderAction $ do
        let watchLength = 42
            --watchLength = 34
        C.setColor white
        ox ← subtract watchLength . snd <$> C.windowSize

        drawBorders watchLength
        drawList 0 1 teamBoxes

        setDataColor gs 0 white green blue
        drawData (0 * 17 + 2) (0 * 5 + 2) "Carla"   ("Handgun",  8, 10) (10, 15)
        setDataColor gs 1 white green blue
        drawData (1 * 17 + 2) (0 * 5 + 2) "Delgado" ("Rifle",   21, 21) (2, 20)
        setDataColor gs 2 white green blue
        drawList (2 * 17 + 1) (0 * 5 + 2) emptyMember
        setDataColor gs 3 white green blue
        drawData (0 * 17 + 2) (1 * 5 + 2) "Raj"     ("Railgun",  7,  8) (8, 12)
        setDataColor gs 4 white green blue
        drawData (1 * 17 + 2) (1 * 5 + 2) "570rm"   ("P.Blas.", 11, 11) (8, 16)
        setDataColor gs 5 white green blue
        drawList (2 * 17 + 1) (1 * 5 + 2) emptyMember

        C.setColor $ if gs == HudMessages
                        then green
                        else white
        drawStatus watchLength

        C.setColor white
        drawList ox 0 watch

        C.setColor $ if gs == HudWatch
                        then green
                        else white
        let s = turns `mod` 60
            m = turns `div` 60 `mod` 60
            h = m `div` 60 `mod` 60
            
        drawTime watchLength
            (fromIntegral . digitToInt . flip (atDef '0') 1 . reverse . show $ h)
            (fromIntegral . digitToInt . flip (atDef '0') 0 . reverse . show $ h)
            (fromIntegral . digitToInt . flip (atDef '0') 1 . reverse . show $ m)
            (fromIntegral . digitToInt . flip (atDef '0') 0 . reverse . show $ m)
            (fromIntegral . digitToInt . flip (atDef '0') 1 . reverse . show $ s)
            (fromIntegral . digitToInt . flip (atDef '0') 0 . reverse . show $ s)
    where
        setDataColor ∷ GameState → Int → C.ColorID → C.ColorID → C.ColorID → C.Update ()
        setDataColor gs i none hud active
            | gs == HudTeam && hms == i = C.setColor hud
            |                   am == i = C.setColor active
            | otherwise                 = C.setColor none

        drawData ∷ Word → Word → String → (String, Int, Int) → (Int, Int) → C.Update ()
        drawData ox oy n (wn, cl, mcl) (hp, mhp) = do
            let boxWidth = 14
            -- Name
            C.moveCursor (fromIntegral oy) (fromIntegral ox)
            C.drawString n
            -- Stance
            C.moveCursor (fromIntegral oy) (fromIntegral ox + boxWidth - 1)
            C.drawGlyph (C.Glyph '_' [])
            -- Weapon
            C.moveCursor (fromIntegral oy + 1) (fromIntegral ox)
            C.drawString wn
            -- Clip
            let clipStr = show cl <> "/" <> show mcl
            C.moveCursor (fromIntegral oy + 1) (fromIntegral ox + boxWidth - fromIntegral (length clipStr))
            C.drawString clipStr
            -- Health bar
            let hBars = floor ((fromIntegral hp / fromIntegral mhp) * fromIntegral boxWidth)
                hDots = fromIntegral boxWidth - hBars
            C.moveCursor (fromIntegral oy + 3) (fromIntegral ox)
            C.drawString (concat [ replicate hBars '|'
                                 , replicate hDots '.'
                                 ])
            

        --shorten ∷ Word → String → String
        --shorten l = (++".") . take (l-1)

        watch ∷ [String]
        watch = [ "    .-------------------------------.    "
                , "   /                                 \\   "
                , "━━/    .-------------------------.    \\━━"
                , " .    /                           \\    . "
                , "┌|---'                             '---| "
                , "└|   |                             |   |┐"
                , " |   |                             |   |│"
                , "┌|   |                             |   |┘"
                , "└|---.                             .---| "
                , " '    \\                           /    ' "
                , "━━\\    '-------------------------'    /━━"
                , "   \\      o      o     o      o      /   "
                , "    '-------------------------------'    "
                ]

        drawTime ∷ Integer → Word → Word → Word → Word → Word → Word → C.Update ()
        drawTime watchLength h1 h2 m1 m2 s1 s2 = do
            ox ← subtract watchLength . snd <$> C.windowSize
            drawList (ox + 8)  4 (digit h1)
            drawList (ox + 12) 4 (digit h2)
            drawList (ox + 16) 4 dots
            drawList (ox + 18) 4 (digit m1)
            drawList (ox + 22) 4 (digit m2)
            drawList (ox + 27) 6 (smallDigit s1)
            drawList (ox + 30) 6 (smallDigit s2)

        drawList ∷ Integer → Integer → [String] → C.Update ()
        drawList x y =
            traverse_ (\(ix, l) → C.moveCursor ix x >> C.drawString l)
            . zip [y..]

        dots ∷ [String]
        dots = [" "
               ,"'"
               ," "
               ,"'"
               ," "
               ]

        digit ∷ Word → [String]
        digit 0 =
            [ " ━ "
            , "┃ ┃"
            , "   "
            , "┃ ┃"
            , " ━ "
            ]
        digit 1 =
            [ "   "
            , "  ┃"
            , "   "
            , "  ┃"
            , "   "
            ]
        digit 2 =
            [ " ━ "
            , "  ┃"
            , " ━ "
            , "┃  "
            , " ━ "
            ]
        digit 3 =
            [ " ━ "
            , "  ┃"
            , " ━ "
            , "  ┃"
            , " ━ "
            ]
        digit 4 =
            [ "   "
            , "┃ ┃"
            , " ━ "
            , "  ┃"
            , "   "
            ]
        digit 5 =
            [ " ━ "
            , "┃  "
            , " ━ "
            , "  ┃"
            , " ━ "
            ]
        digit 6 =
            [ " ━ "
            , "┃  "
            , " ━ "
            , "┃ ┃"
            , " ━ "
            ]
        digit 7 =
            [ " ━ "
            , "┃ ┃"
            , "   "
            , "  ┃"
            , "   "
            ]
        digit 8 =
            [ " ━ "
            , "┃ ┃"
            , " ━ "
            , "┃ ┃"
            , " ━ "
            ]
        digit 9 =
            [ " ━ "
            , "┃ ┃"
            , " ━ "
            , "  ┃"
            , " ━ "
            ]
        digit _ = digit 8

        smallDigit ∷ Word → [String]
        smallDigit 0 = ["|̅‾|"
                       ,"|̅ |"
                       ,"|_|"
                       ]
        smallDigit 1 = ["  |"
                       ,"  |"
                       ,"  |"
                       ]
        smallDigit 2 = [" ‾|"
                       ," / "
                       ,"|_ "
                       ]
        smallDigit 3 = [" ‾|"
                       ," ─|"
                       ," _|"
                       ]
        smallDigit 4 = ["|̅ |"
                       ," \\|"
                       ,"  |"
                       ]
        smallDigit 5 = ["|̅‾ "
                       ," \\ "
                       ," _|"
                       ]
        smallDigit 6 = ["|̅‾ "
                       ,"|\\ "
                       ,"|_|"
                       ]
        smallDigit 7 = ["|̅‾|"
                       ," / "
                       ,"/  "
                       ]
        smallDigit 8 = ["|̅‾|"
                       ,"|─|"
                       ,"|_|"
                       ]
        smallDigit 9 = ["|̅‾|"
                       ," \\|"
                       ," _|"
                       ]
        smallDigit _ = smallDigit 8


        drawBorders ∷ Integer → C.Update ()
        drawBorders watchLength = do
            len ← fromIntegral . subtract watchLength . snd <$> C.windowSize
            C.moveCursor 2 0
            C.drawString $ replicate len '-'
            C.moveCursor 10 0
            C.drawString $ replicate len '-'

        teamBoxes ∷ [String]
        teamBoxes = [ "┏----------------┳----------------┳----------------┓"
                    , "|                |                |                ┣"
                    , "|                |                |                |"
                    , "|                |                |                |"
                    , "|                |                |                |"
                    , "┣----------------╋----------------╋----------------┫"
                    , "|                |                |                |"
                    , "|                |                |                |"
                    , "|                |                |                |"
                    , "|                |                |                ┣"
                    , "┗----------------┻----------------┻----------------┛"
                    ]
        emptyMember ∷ [String]
        emptyMember = [ "/  //  //  //  /"
                      , "  //  //  //  //"
                      , " //  //  //  // "
                      , "//  //  //  //  "
                      ]

        drawStatus ∷ Integer → C.Update ()
        drawStatus watchLength = do
            let start       = 54 -- Team boxes length
                padding     = 4
            
            C.moveCursor 3 (fromIntegral start)
            C.drawGlyph (C.Glyph '▲' [])
            C.moveCursor 9 (fromIntegral start)
            C.drawGlyph (C.Glyph '▼' [])
            
            -- TODO add lines' here
            len ← subtract (start + watchLength + padding) . fromIntegral . snd <$> C.windowSize
            let lns = lines' (fromIntegral len) length " " (words loremIpsum)
            drawList (fromIntegral start) padding lns

        loremIpsum ∷ String
        loremIpsum = intercalate " "
            [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
            , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
            , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
            , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
            , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
            , "culpa qui officia deserunt mollit anim id est laborum"
            ]

