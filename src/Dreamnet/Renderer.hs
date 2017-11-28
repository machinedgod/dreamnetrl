{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Renderer
( module Control.Lens
, module Control.Monad.Reader

, MonadRender(..)
, RendererF

, RendererEnvironment
, rd_styles
, rd_scrollModel
, rd_choiceModel
, rd_mainWindow
, rd_hudWindow
, rd_examineWindow
, rd_interactionWindow
, rd_choiceWindow
, rd_conversationWindow0
, rd_conversationWindow1

, initRenderer
, runRenderer

, drawMap
, drawObject
, drawObjects
, drawPlayer
, drawAim
, drawHud
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Linear
import Data.Maybe (fromMaybe)
import Data.Bool (bool)

import UI.NCurses.Class
import qualified UI.NCurses  as Curses
import qualified Data.Map    as Map
import qualified Data.Vector as Vec

import qualified Dreamnet.TileMap as TMap
import Dreamnet.World
import Dreamnet.Character
import Dreamnet.ScrollModel
import Dreamnet.ChoiceModel

--------------------------------------------------------------------------------

data Styles = Styles {
      _s_materials ∷ Map.Map String [Curses.Attribute]
    , _s_unknown   ∷ [Curses.Attribute]
    , _s_playerAim ∷ [Curses.Attribute]

    , _s_visibilityUnknown ∷ [Curses.Attribute]
    , _s_visibilityKnown   ∷ [Curses.Attribute]
    , _s_visibilityVisible ∷ [Curses.Attribute]

    , _s_colorRed     ∷ Curses.ColorID
    , _s_colorGreen   ∷ Curses.ColorID
    , _s_colorYellow  ∷ Curses.ColorID
    , _s_colorBlue    ∷ Curses.ColorID
    , _s_colorMagenta ∷ Curses.ColorID
    , _s_colorCyan    ∷ Curses.ColorID
    , _s_colorWhite   ∷ Curses.ColorID
    }

makeLenses ''Styles

data RendererEnvironment = RendererEnvironment {
      _rd_styles              ∷ Styles

    , _rd_scrollModel ∷ ScrollModel
    , _rd_choiceModel ∷ ChoiceModel

    , _rd_mainWindow          ∷ Curses.Window
    , _rd_hudWindow           ∷ Curses.Window
    , _rd_examineWindow       ∷ Curses.Window
    , _rd_interactionWindow   ∷ Curses.Window
    , _rd_choiceWindow        ∷ Curses.Window
    , _rd_conversationWindow0 ∷ Curses.Window
    , _rd_conversationWindow1 ∷ Curses.Window
    }

makeLenses ''RendererEnvironment

--------------------------------------------------------------------------------

class (MonadState RendererEnvironment r, MonadReader World r) ⇒ MonadRender r where
    updateWindow ∷ Curses.Window → Curses.Update () → r ()
    swap         ∷ r ()
    

-- SOME stuff should be just reader, some stuff state
newtype RendererF a = RendererF { runRendererF ∷ ReaderT World (StateT RendererEnvironment Curses.Curses) a }
                    deriving (Functor, Applicative, Monad, MonadReader World, MonadState RendererEnvironment, MonadCurses)


instance MonadCurses (ReaderT World (StateT RendererEnvironment Curses.Curses)) where
    liftCurses = lift . lift


instance MonadRender RendererF where
    updateWindow w = liftCurses . Curses.updateWindow w
    swap = liftCurses Curses.render


initRenderer ∷ Curses.Curses RendererEnvironment
initRenderer = do
    -- TODO respond to resize events and resize all the windows!
    --      this should happen automatically and be inacessible by API
    (rows, columns) ← Curses.screenSize

    let hudWidth   = columns
        hudHeight  = 8
        mainWidth  = columns
        mainHeight = rows - hudHeight
    mainWin ← Curses.newWindow mainHeight mainWidth 0 0
    hudWin  ← Curses.newWindow hudHeight hudWidth mainHeight 0



    let lowLeftW = mainWidth `div` 3
        lowLeftH = mainHeight `div` 3
        lowLeftX = 0
        lowLeftY = mainHeight `div` 3 * 2

        topRightW = mainWidth `div` 3
        topRightH = mainHeight `div` 3
        topRightX = mainWidth `div` 3 * 2
        topRightY = 0

        examineW = 50
        examineH = 10
        examineX = (mainWidth - examineW) `div` 2
        examineY = (mainHeight - examineH) `div` 2

        interactW = (columns - 4)
        interactH = (rows - 4)
        interactX = 2
        interactY = 2

    examineWin       ← Curses.newWindow examineH examineW examineY examineX
    choiceWin        ← Curses.newWindow lowLeftH lowLeftW lowLeftY lowLeftX
    interactionWin   ← Curses.newWindow interactH interactW interactY interactX
    conversationWin0 ← Curses.newWindow lowLeftH lowLeftW lowLeftY lowLeftX
    conversationWin1 ← Curses.newWindow topRightH topRightW topRightY topRightX

    styles ← Curses.maxColor >>= createStyles 
    let scrollW = fromIntegral $ examineW - 6 -- border, padding, arrow widgets
        scrollH = fromIntegral $ examineH - 2

    return $ RendererEnvironment styles
                 (createScrollModel scrollW scrollH)
                 (ChoiceModel Vec.empty 0)
                 mainWin hudWin examineWin interactionWin choiceWin
                 conversationWin0 conversationWin1
        where
            createStyles mc
                | mc > 0 && mc <= 8    = createStyles8Colors    -- (And disable lighting)
                | mc > 8 && mc <= 255  = createStyles8Colors   -- (And disable lighting)
                | mc >= 255            = createStyles8Colors  -- (And enable lighting!)
                | otherwise            = error "Your terminal doesn't support color! I haven't had time to make things render without colors yet, sorry :-("
            createStyles8Colors = do 
                cRed     ←  Curses.newColorID  Curses.ColorRed      Curses.ColorBlack  1
                cGreen   ←  Curses.newColorID  Curses.ColorGreen    Curses.ColorBlack  2
                cYellow  ←  Curses.newColorID  Curses.ColorYellow   Curses.ColorBlack  3
                cBlue    ←  Curses.newColorID  Curses.ColorBlue     Curses.ColorBlack  4
                cMagenta ←  Curses.newColorID  Curses.ColorMagenta  Curses.ColorBlack  5
                cCyan    ←  Curses.newColorID  Curses.ColorCyan     Curses.ColorBlack  6
                cWhite   ←  Curses.newColorID  Curses.ColorWhite    Curses.ColorBlack  7

                let materials = Map.fromList
                        [ ("wood"          , [ Curses.AttributeColor cYellow, Curses.AttributeDim  ])
                        , ("metal"         , [ Curses.AttributeColor cCyan,   Curses.AttributeBold ])
                        , ("blue plastic"  , [ Curses.AttributeColor cCyan,   Curses.AttributeDim  ])
                        , ("red plastic"   , [ Curses.AttributeColor cRed,    Curses.AttributeDim  ])
                        , ("ceramics"      , [ Curses.AttributeColor cWhite,  Curses.AttributeDim  ])
                        ]
                    matUnknown = [ Curses.AttributeColor cMagenta, Curses.AttributeBold, Curses.AttributeBlink ]
                return Styles {
                         _s_materials = materials
                       , _s_unknown   = matUnknown
                       , _s_playerAim = [ Curses.AttributeColor cGreen, Curses.AttributeBold]

                       , _s_visibilityUnknown = []
                       , _s_visibilityKnown   = [ Curses.AttributeColor cBlue,  Curses.AttributeDim ]
                       , _s_visibilityVisible = [ Curses.AttributeColor cWhite, Curses.AttributeDim ]

                       , _s_colorRed     = cRed    
                       , _s_colorGreen   = cGreen  
                       , _s_colorYellow  = cYellow 
                       , _s_colorBlue    = cBlue   
                       , _s_colorMagenta = cMagenta
                       , _s_colorCyan    = cCyan   
                       , _s_colorWhite   = cWhite  
                       }


runRenderer ∷ RendererEnvironment → World → RendererF a → Curses.Curses (a, RendererEnvironment)
runRenderer rd w f = runStateT (runReaderT (runRendererF f) w) rd
    
--------------------------------------------------------------------------------

drawCharAt ∷ (Integral a) ⇒ V2 a → Char → [Curses.Attribute] → Curses.Update ()
drawCharAt (V2 x y) c s = do
    Curses.moveCursor (fromIntegral y) (fromIntegral x)
    Curses.drawGlyph (Curses.Glyph c s)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    w   ← use rd_mainWindow
    m   ← view w_map
    vis ← view w_visible

    unknown ← use (rd_styles.s_visibilityUnknown)
    known   ← use (rd_styles.s_visibilityKnown)
    visible ← use (rd_styles.s_visibilityVisible)
    updateWindow w $
        Vec.imapM_ (drawTile unknown known visible m) $ Vec.zip (m^.TMap.m_data) vis
    where
        drawTile us ks vs m i (c, v) = uncurry (drawCharAt $ TMap.coordLin m i) $
            case v of
                Unknown → (' ', us)
                Known   → (c,   ks)
                Visible → (c,   vs)



drawObject ∷ (MonadRender r) ⇒ V2 Int → Object → r ()
drawObject v o = do
    m   ← view w_map
    vis ← view w_visible

    mats   ← use (rd_styles.s_materials)
    matu   ← use (rd_styles.s_unknown)
    items  ← views (w_items) (maybe [] (const [Curses.AttributeReverse]) . Map.lookup v)
    known  ← use (rd_styles.s_visibilityKnown)

    w  ← use (rd_mainWindow)
    case isVisible vis m of
        Unknown → return ()
        Known   → updateWindow w $ drawCharAt v (objectChar o) known
        Visible → updateWindow w $ drawCharAt v (objectChar o) (objectMat matu mats o)
    where
        isVisible vis m = vis Vec.! TMap.linCoord m v
        objectChar Computer         = '&'
        objectChar (Person n)       = '@'
        objectChar (Door o)         = bool '+' '\'' o
        objectChar (Stairs u)       = bool '<' '>' u
        objectChar (Prop _ _ _ c _) = c
        objectMat matu mats Computer   = fromMaybe matu $ Map.lookup "metal" mats
        objectMat matu mats (Person _) = [] -- If ally, green. Also use red shades to communicate suspicion when sneaking
        objectMat matu mats (Door _)   = fromMaybe matu $ Map.lookup "wood" mats
        objectMat matu mats (Stairs _) = fromMaybe matu $ Map.lookup "wood" mats
        objectMat matu mats (Prop _ _ _ _ m) = fromMaybe matu $ Map.lookup m mats


drawObjects ∷ (MonadRender r) ⇒ r ()
drawObjects = view (w_objects) >>= Map.foldWithKey (\k v p → p >> drawObject k v) (return ())


drawPlayer ∷ (MonadRender r) ⇒ r ()
drawPlayer = do
    w ← use rd_mainWindow
    s ← uses (rd_styles.s_colorMagenta) Curses.AttributeColor
    v ← view w_playerPos
    updateWindow w $ drawCharAt v '@' [s]


drawAim ∷ (MonadRender r) ⇒ r ()
drawAim = do
    w  ← use rd_mainWindow
    s  ← use (rd_styles.s_playerAim)
    ma ← view w_aim
    case ma of
        Just v → updateWindow w $ drawCharAt v '×' s
        _      → return ()
         

drawHud ∷ (MonadRender r) ⇒ r ()
drawHud = do
    w ← use rd_hudWindow
    s ← view w_status
    updateWindow w $ do
        Curses.drawBorder (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '│' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '─' [])
                          (Just $ Curses.Glyph '╭' [])
                          (Just $ Curses.Glyph '╮' [])
                          (Just $ Curses.Glyph '╰' [])
                          (Just $ Curses.Glyph '╯' [])

        Curses.moveCursor 2 2
        Curses.drawString $ "Status: " ++ s

