{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Renderer
( MonadRender(..)
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
, drawPlayer
, drawAim
, drawHud
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Linear
import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.Bool (bool)

import UI.NCurses.Class
import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.CoordVector
import Dreamnet.WorldMap
import Dreamnet.World
import Dreamnet.ScrollModel
import Dreamnet.ChoiceModel

--------------------------------------------------------------------------------

data Styles = Styles {
      _s_materials ∷ M.Map String [C.Attribute]
    , _s_unknown   ∷ [C.Attribute]
    , _s_playerAim ∷ [C.Attribute]

    , _s_visibilityUnknown ∷ [C.Attribute]
    , _s_visibilityKnown   ∷ [C.Attribute]
    , _s_visibilityVisible ∷ [C.Attribute]

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

    , _rd_scrollModel ∷ ScrollModel
    , _rd_choiceModel ∷ ChoiceModel

    , _rd_mainWindow          ∷ C.Window
    , _rd_hudWindow           ∷ C.Window
    , _rd_examineWindow       ∷ C.Window
    , _rd_interactionWindow   ∷ C.Window
    , _rd_choiceWindow        ∷ C.Window
    , _rd_conversationWindow0 ∷ C.Window
    , _rd_conversationWindow1 ∷ C.Window
    }

makeLenses ''RendererEnvironment

--------------------------------------------------------------------------------

class (MonadState RendererEnvironment r, MonadReader World r) ⇒ MonadRender r where
    updateWindow ∷ C.Window → C.Update () → r ()
    swap         ∷ r ()
    

-- SOME stuff should be just reader, some stuff state
newtype RendererF a = RendererF { runRendererF ∷ ReaderT World (StateT RendererEnvironment C.Curses) a }
                    deriving (Functor, Applicative, Monad, MonadReader World, MonadState RendererEnvironment, MonadCurses)


instance MonadCurses (ReaderT World (StateT RendererEnvironment C.Curses)) where
    liftCurses = lift . lift


instance MonadRender RendererF where
    updateWindow w = liftCurses . C.updateWindow w
    swap = liftCurses C.render


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

    examineWin       ← C.newWindow examineH examineW examineY examineX
    choiceWin        ← C.newWindow lowLeftH lowLeftW lowLeftY lowLeftX
    interactionWin   ← C.newWindow interactH interactW interactY interactX
    conversationWin0 ← C.newWindow lowLeftH lowLeftW lowLeftY lowLeftX
    conversationWin1 ← C.newWindow topRightH topRightW topRightY topRightX

    styles ← C.maxColor >>= createStyles 
    let scrollW = fromIntegral $ examineW - 6 -- border, padding, arrow widgets
        scrollH = fromIntegral $ examineH - 2

    return $ RendererEnvironment styles
                 (createScrollModel scrollW scrollH)
                 (ChoiceModel V.empty 0)
                 mainWin hudWin examineWin interactionWin choiceWin
                 conversationWin0 conversationWin1
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
                        [ ("wood"          , [ C.AttributeColor cYellow, C.AttributeDim  ])
                        , ("metal"         , [ C.AttributeColor cCyan,   C.AttributeBold ])
                        , ("blue plastic"  , [ C.AttributeColor cCyan,   C.AttributeDim  ])
                        , ("red plastic"   , [ C.AttributeColor cRed,    C.AttributeDim  ])
                        , ("ceramics"      , [ C.AttributeColor cWhite,  C.AttributeDim  ])
                        ]
                    matUnknown = [ C.AttributeColor cMagenta, C.AttributeBold, C.AttributeBlink ]
                return Styles {
                         _s_materials = materials
                       , _s_unknown   = matUnknown
                       , _s_playerAim = [ C.AttributeColor cGreen, C.AttributeBold]

                       , _s_visibilityUnknown = []
                       , _s_visibilityKnown   = [ C.AttributeColor cBlue,  C.AttributeDim ]
                       , _s_visibilityVisible = [ C.AttributeColor cWhite, C.AttributeDim ]

                       , _s_colorRed     = cRed    
                       , _s_colorGreen   = cGreen  
                       , _s_colorYellow  = cYellow 
                       , _s_colorBlue    = cBlue   
                       , _s_colorMagenta = cMagenta
                       , _s_colorCyan    = cCyan   
                       , _s_colorWhite   = cWhite  
                       }


runRenderer ∷ RendererEnvironment → World → RendererF a → C.Curses (a, RendererEnvironment)
runRenderer rd w f = runStateT (runReaderT (runRendererF f) w) rd
    
--------------------------------------------------------------------------------

drawCharAt ∷ (Integral a) ⇒ V2 a → Char → [C.Attribute] → C.Update ()
drawCharAt (V2 x y) c s = do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c s)


drawMap ∷ (MonadRender r) ⇒ r ()
drawMap = do
    w   ← use rd_mainWindow
    m   ← view w_map
    vis ← view (w_map.wm_visible)

    unknown ← use (rd_styles.s_visibilityUnknown)
    known   ← use (rd_styles.s_visibilityKnown)
    visible ← use (rd_styles.s_visibilityVisible)
    mats    ← use (rd_styles.s_materials)
    let drawTile i (o, v) = uncurry (drawCharAt $ coordLin m i) $
                              case v of
                                  Unknown → (' ', unknown)
                                  Known   → (objectToChar o, known)
                                  Visible → (objectToChar o, fromMaybe visible (objectToMat mats o))
    updateWindow w $
        V.imapM_ drawTile $ V.zip (m^.wm_data) vis


objectToChar ∷ Object → Char
objectToChar (Base c _ _)     = c
objectToChar (Door o)         = bool '+' '\'' o
objectToChar (Stairs u)       = bool '<' '>' u
objectToChar (Prop c _ _ _ _) = c
objectToChar (Person _)       = '@' -- TODO if ally, color green
objectToChar Computer         = '$'
objectToChar (ItemO _)        = '['
objectToChar (Union _ o2)     = objectToChar o2


objectToMat ∷ M.Map String [C.Attribute] → Object → Maybe [C.Attribute]
objectToMat _    (Base _ _ _)     = Nothing
objectToMat mats (Door _)         = "wood" `M.lookup` mats
objectToMat mats (Stairs _)       = "wood" `M.lookup` mats
objectToMat mats (Prop _ _ m _ _) = m `M.lookup` mats
objectToMat _    (Person _)       = Nothing
objectToMat mats  Computer        = "metal" `M.lookup` mats
objectToMat mats (ItemO _)        = "blue plastic" `M.lookup` mats
objectToMat mats (Union _ o2)     = objectToMat mats o2


--drawObject ∷ (MonadRender r) ⇒ V2 Int → Object → r ()
--drawObject v o = do
--    m   ← view w_map
--    vis ← view w_visible
--
--    mats   ← use (rd_styles.s_materials)
--    matu   ← use (rd_styles.s_unknown)
--    items  ← views (w_items) (maybe [] (const [C.AttributeReverse]) . M.lookup v)
--    known  ← use (rd_styles.s_visibilityKnown)
--
--    w  ← use (rd_mainWindow)
--    case isVisible vis m of
--        Unknown → return ()
--        Known   → updateWindow w $ drawCharAt v (objectChar o) known
--        Visible → updateWindow w $ drawCharAt v (objectChar o) (objectMat matu mats o)
--    where
--        isVisible vis m = vis V.! linCoord m v
--        objectChar (Person n)       = '@'
--        objectChar (Door o)         = bool '+' '\'' o
--        objectChar (Stairs u)       = bool '<' '>' u
--        objectChar (Prop _ _ _ c _) = c
--        objectMat matu mats Computer   = fromMaybe matu $ M.lookup "metal" mats
--        objectMat matu mats (Person _) = [] -- If ally, green. Also use red shades to communicate suspicion when sneaking
--        objectMat matu mats (Door _)   = fromMaybe matu $ M.lookup "wood" mats
--        objectMat matu mats (Stairs _) = fromMaybe matu $ M.lookup "wood" mats
--        objectMat matu mats (Prop _ _ _ _ m) = fromMaybe matu $ M.lookup m mats


--drawObjects ∷ (MonadRender r) ⇒ r ()
--drawObjects = view (w_objects) >>= M.foldWithKey (\k v p → p *> drawObject k v) (return ())


drawPlayer ∷ (MonadRender r) ⇒ r ()
drawPlayer = do
    w ← use rd_mainWindow
    s ← uses (rd_styles.s_colorMagenta) C.AttributeColor
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
        C.drawBorder (Just $ C.Glyph '│' [])
                          (Just $ C.Glyph '│' [])
                          (Just $ C.Glyph '─' [])
                          (Just $ C.Glyph '─' [])
                          (Just $ C.Glyph '╭' [])
                          (Just $ C.Glyph '╮' [])
                          (Just $ C.Glyph '╰' [])
                          (Just $ C.Glyph '╯' [])

        C.moveCursor 2 2
        C.drawString $ "Status: " <> s

