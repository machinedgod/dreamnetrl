{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-type-defaults #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Dreamnet.Rendering.Renderer
( newScrollData, newScrollData', scrollUp, scrollDown

, newChoiceData, selectNext, selectPrevious

, sMaterials, sUnknown, sVisibilityUnknown, sVisibilityKnown, sColorBlack
, sColorRed, sColorGreen, sColorYellow, sColorBlue, sColorMagenta
, sColorCyan, sColorWhite

, Camera
, RendererEnvironment, newRenderEnvironment

, RenderAction(RenderAction)
, RenderAPI(..)

, RendererM, runRenderer, evalRenderer, execRenderer

, draw', draw, drawString, drawList

, clear, drawMap, drawTeamHud, clearStatus, drawStatus, statusOrigin, drawWatch
, drawCharacterSheet, drawEquipmentDoll, drawInformation, drawChoice
, drawComputer
) where


import Safe                      (atDef)
import Control.Lens              (Lens', makeLenses, view, views, use, uses,
                                  (%=), (.=))
import Control.Monad.Trans       (MonadTrans, lift)
import Control.Monad.State       (MonadState, StateT, runStateT, evalStateT,
                                  execStateT)
import Linear                    (V2(V2))
import Data.Maybe                (fromMaybe, isJust)
import Data.Bifunctor            (second)
import Data.Foldable             (traverse_, forM_)
import Data.Char                 (digitToInt)
import Data.List                 (genericLength, genericReplicate)
import Data.Bool                 (bool)
import Data.Tuple                (swap)

import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.Engine.Utils (fmt)
import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Visibility

import Dreamnet.Rendering.ScrollData
import Dreamnet.Rendering.ChoiceData

import Dreamnet.ObjectStates hiding (Material, Camera)

--------------------------------------------------------------------------------

pad ∷ (Integral a) ⇒ a → String → String
pad i  = (<>) <$> id <*> flip genericReplicate ' ' . (`subtract` i) . genericLength


padL ∷ (Integral a) ⇒ a → String → String
padL i = (<>) <$> flip genericReplicate ' ' . (`subtract` i) . genericLength <*> id


padC ∷ (Integral a) ⇒ a → String → String
padC i s =
    let l = subtract (genericLength s) i `div` 2
        r = (l + genericLength s) `subtract` i
    in  genericReplicate l ' ' <> s <> genericReplicate r ' '


--------------------------------------------------------------------------------

data WatchData = WatchData {
      _wdHours   ∷ Int
    , _wdMinutes ∷ Int
    , _wdSeconds ∷ Int
    , _wdWeekDay ∷ String
    , _wdDay     ∷ Int
    , _wdMonth   ∷ Int
    , _wdYear    ∷ Int
    }
makeLenses ''WatchData


fromSeconds ∷ Int → WatchData
fromSeconds t =
    let s = t `mod` 60
        m = t `div` 60 `mod` 60
        h = m `div` 60 `mod` 60
    in  WatchData {
          _wdHours   = h
        , _wdMinutes = m
        , _wdSeconds = s
        , _wdWeekDay = "Tue"
        , _wdDay     = 12
        , _wdMonth   = 4
        , _wdYear    = 2183
        }


numberToDigits ∷ Int → (Int, Int)
numberToDigits i =
    let f ix = fromIntegral . digitToInt . flip (atDef '0') ix . reverse . show
    in  (f 1 i, f 0 i)

--------------------------------------------------------------------------------

type Material = [C.Attribute]

data Styles = Styles {
      _sMaterials ∷ M.Map String Material
    , _sUnknown   ∷ Material
    --, _sPlayerAim ∷ Material

    , _sVisibilityUnknown ∷ Material
    , _sVisibilityKnown   ∷ Material
    --, _sVisibilityVisible ∷ Material

    , _sColorBlack   ∷ C.ColorID
    , _sColorRed     ∷ C.ColorID
    , _sColorGreen   ∷ C.ColorID
    , _sColorYellow  ∷ C.ColorID
    , _sColorBlue    ∷ C.ColorID
    , _sColorMagenta ∷ C.ColorID
    , _sColorCyan    ∷ C.ColorID
    , _sColorWhite   ∷ C.ColorID
    }

makeLenses ''Styles

--------------------------------------------------------------------------------

data Camera = Camera {
      _cmPosition ∷ V2 Int
    , _cmViewport ∷ V2 Int
    }
makeLenses ''Camera


data RendererEnvironment = RendererEnvironment {
      _rdStyles     ∷ Styles

    , _rdMainWindow ∷ C.Window
    , _rdHudWindow  ∷ C.Window
    , _rdUiWindow   ∷ C.Window

    , _rdCamera     ∷ Camera

    , _rdChoiceData ∷ ChoiceData
    , _rdScrollData ∷ ScrollData
    }
makeLenses ''RendererEnvironment


mainSize' ∷ C.Curses (Integer, Integer)
mainSize' = do
    hudHeight ← snd <$> hudSize'
    second (subtract hudHeight) . swap <$> C.screenSize


hudSize' ∷ C.Curses (Integer, Integer)
hudSize' = (,13) . snd <$> C.screenSize


newRenderEnvironment ∷ C.Curses RendererEnvironment
newRenderEnvironment = do
    -- TODO respond to resize events and resize all the windows!
    --      this should happen automatically and be inacessible by API
    (rows, columns) ← C.screenSize
    (mw, mh) ← mainSize'
    (hw, hh) ← hudSize'

    mainWin ← C.newWindow mh mw 0 0
    hudWin  ← C.newWindow hh hw mh 0
    uiWin   ← C.newWindow 0 0 (rows - 1) (columns - 1)
    styles  ← C.maxColor >>= createStyles

    pure $
        RendererEnvironment {
          _rdStyles     = styles

        , _rdMainWindow = mainWin
        , _rdHudWindow  = hudWin
        , _rdUiWindow   = uiWin

        , _rdCamera     = Camera (V2 0 0) (V2 (fromIntegral columns) (fromIntegral rows))

        , _rdChoiceData = newChoiceData  (V2 0 0) (V2 10 10) []
        , _rdScrollData = newScrollData' (V2 0 0) (V2 10 10) Nothing []
        }
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
                     _sMaterials = M.fromList
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
                        , ("cloth"          , [ C.AttributeColor cWhite,  C.AttributeDim ])

                        , ("blue"           , [ C.AttributeColor cBlue ])
                        , ("red"            , [ C.AttributeColor cRed ])
                        ]
                   , _sUnknown   = [ C.AttributeColor cMagenta, C.AttributeBold, C.AttributeBlink ]
                   --, _sPlayerAim = [ C.AttributeColor cGreen, C.AttributeBold]

                   , _sVisibilityUnknown = []
                   , _sVisibilityKnown   = [ C.AttributeColor cBlue,  C.AttributeDim ]
                   --, _sVisibilityVisible = [ C.AttributeColor cWhite ]
                   --, _sVisibilityVisible = [ C.AttributeColor cWhite, C.AttributeDim ]

                   , _sColorBlack   = cBlack
                   , _sColorRed     = cRed
                   , _sColorGreen   = cGreen
                   , _sColorYellow  = cYellow
                   , _sColorBlue    = cBlue
                   , _sColorMagenta = cMagenta
                   , _sColorCyan    = cCyan
                   , _sColorWhite   = cWhite
                   }

--------------------------------------------------------------------------------

-- TODO remove dependence on Curses and use them just as an implementation
newtype RenderAction a = RenderAction { runAction ∷ C.Update a }
                       deriving (Functor, Applicative, Monad)


class RenderAPI r where
    updateMain     ∷ RenderAction () → r ()
    updateHud      ∷ RenderAction () → r ()
    updateUi       ∷ RenderAction () → r ()
    screenSize     ∷ r (Integer, Integer)
    mainSize       ∷ r (Integer, Integer)
    hudSize        ∷ r (Integer, Integer)
    setScroll      ∷ ScrollData → r ()
    doScroll       ∷ (ScrollData → ScrollData) → r ()
    withScroll     ∷ (ScrollData → a) → r a
    setChoice      ∷ ChoiceData → r ()
    doChoice       ∷ (ChoiceData → ChoiceData) → r ()
    withChoiceData ∷ (ChoiceData → a) → r a
    currentChoice  ∷ r Int
    moveCamera     ∷ V2 Int → r ()
    camera         ∷ r Camera
    style          ∷ Lens' Styles a → r a
    flush          ∷ r ()

--------------------------------------------------------------------------------

-- TODO double buffering
newtype RendererM m a = RendererM { runRendererM ∷ StateT RendererEnvironment m a }
                      deriving (Functor, Applicative, Monad, MonadState RendererEnvironment)


instance MonadTrans RendererM where
    lift = RendererM . lift


instance RenderAPI (RendererM C.Curses) where
    updateMain ac = use rdMainWindow >>= lift . (`C.updateWindow` runAction ac)

    updateHud ac = use rdHudWindow >>= lift . (`C.updateWindow` runAction ac)

    updateUi ac = use rdUiWindow >>= lift . (`C.updateWindow` (C.clear >> runAction ac))

    screenSize = lift C.screenSize

    mainSize = lift mainSize'

    hudSize = lift hudSize'

    setScroll sd = rdScrollData .= sd

    doScroll f = rdScrollData %= f

    withScroll f = uses rdScrollData f

    setChoice cd = rdChoiceData .= cd

    doChoice f = rdChoiceData %= f

    withChoiceData = uses rdChoiceData

    currentChoice = use (rdChoiceData.cdCurrentSelection)

    moveCamera v = rdCamera.cmPosition %= clip
        where
            clip ∷ V2 Int → V2 Int
            clip op = fmap fromIntegral $ max (V2 0 0) $ (fromIntegral <$> op) - v

    camera = use rdCamera

    style l = use (rdStyles.l)

    flush = lift C.render



runRenderer ∷ (Monad m) ⇒ RendererM m a → RendererEnvironment → m (a, RendererEnvironment)
runRenderer = runStateT . runRendererM


evalRenderer ∷ (Monad m) ⇒ RendererM m a → RendererEnvironment → m a
evalRenderer = evalStateT . runRendererM


execRenderer ∷ (Monad m) ⇒ RendererM m a → RendererEnvironment → m RendererEnvironment
execRenderer = execStateT . runRendererM

--------------------------------------------------------------------------------

clear ∷ RenderAction ()
clear = RenderAction C.clear


drawMap ∷ (RenderAPI r, Monad r) ⇒ (Int → Char) → (Int → String) → (Int → Visibility) → Width → Height → r (RenderAction ())
drawMap chf matf visf w h = do
    u ← style sVisibilityUnknown
    k ← style sVisibilityKnown
    sequence_ <$> mapM (drawTile u k) coordsToDraw
    where
        coordsToDraw ∷ [Int]
        coordsToDraw = fromIntegral <$> [ fromIntegral row * fromIntegral w + fromIntegral col | col ← [0..w - 1], row ← [0..h - 1] ]

        drawTile u k i = do
            m ← lookupMaterial (matf i)
            pure $
                uncurry (draw' (coordLin' w i)) $
                    case visf i of
                        Unknown → (' ', u)
                        Known   → (chf i, k)
                        Visible → (chf i, m)


{-
drawMap ∷ (RenderAPI r, Monad r) ⇒ (a → Char) → (a → String) → Width → V.Vector a → V.Vector Visibility → r (RenderAction ())
drawMap chf matf (fromIntegral → w) dat vis = do
    u ← style sVisibilityUnknown
    k ← style sVisibilityKnown
    c ← camera

    let viewport = V2 (min (c^.cmViewport._x) w)
                      (min (c^.cmViewport._y) (fromIntegral $ V.length dat `div` w))

    let dat' = visibleChunk (c^.cmPosition) viewport w dat
    let vis' = visibleChunk (c^.cmPosition) viewport w vis
    mats ← V.mapM (lookupMaterial . matf) dat'
    pure $ V.imapM_ (drawTile (viewport^._x) u k) $ V.zip3 (chf <$> dat') mats vis'
    where
        -- TODO I wonder if I can somehow reimplement this without relying on
        -- pattern matching the Visibility (using Ord, perhaps?)
        drawTile ∷ Int → [C.Attribute] → [C.Attribute] → Int → (Char, Material, Visibility) → RenderAction ()
        drawTile wh u k i (c, m, v) = uncurry (draw' $ coordLin' (fromIntegral wh) i) $
                                     case v of
                                         Unknown → (' ', u)
                                         Known   → (c, k)
                                         Visible → (c, m)

        visibleChunk ∷ V2 Int → V2 Int → Int → V.Vector a → V.Vector a
        visibleChunk (V2 x' y') (V2 w' h') wh v =
            let oln i = V.drop (fromIntegral $ wh * (y' + i)) v
                ldata = V.drop (fromIntegral $ wh - w' - x') . V.take (fromIntegral w') . V.drop (fromIntegral x')
            in  fold $ ldata . oln <$> [0..h']
            -}



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


watchLength ∷ (Num n) ⇒ n
watchLength = (+1) . genericLength . head $ watch


teamBoxes ∷ [String]
teamBoxes = [ "┏----------------┳----------------┳----------------┓"
            , "|/  //  //  //  /|/  //  //  //  /|/  //  //  //  /┣"
            , "|  //  //  //  //|  //  //  //  //|  //  //  //  //|"
            , "| //  //  //  // | //  //  //  // | //  //  //  // |"
            , "|//  //  //  //  |//  //  //  //  |//  //  //  //  |"
            , "┣----------------╋----------------╋----------------┫"
            , "|/  //  //  //  /|/  //  //  //  /|/  //  //  //  /|"
            , "|  //  //  //  //|  //  //  //  //|  //  //  //  //|"
            , "| //  //  //  // | //  //  //  // | //  //  //  // |"
            , "|//  //  //  //  |//  //  //  //  |//  //  //  //  ┣"
            , "┗----------------┻----------------┻----------------┛"
            ]

teamBoxesLength ∷ (Num n) ⇒ n
teamBoxesLength = genericLength . head $ teamBoxes


drawTeamHud ∷ (RenderAPI r, Monad r, Show i) ⇒ [Character i c f] → Maybe Int → r (RenderAction ())
drawTeamHud team mayix = do
    white ← style sColorWhite
    green ← style sColorGreen
    pure $ RenderAction $ do
        C.setColor white

        drawBorders
        drawList 0 1 teamBoxes

        forM_ (zip [0.. ] (take 6 team)) $ \(ix, ch) → do
            C.setColor $ case mayix of
                Just i → if ix == i
                            then green
                            else white
                Nothing → white
            drawData
                (fromIntegral (ix `mod` 3) * 17 + 1)
                (fromIntegral (ix `div` 3) *  5 + 2)
                ch
    where
        drawData ∷  (Show i) ⇒ Integer → Integer → Character i c f → C.Update ()
        drawData ox oy ch = do
            drawList ox oy [ "                "
                           , "                "
                           , "                "
                           , "                "
                           ]
            -- TODO make it so that it draws strings with spaces
            --      this way, it'll rewrite empty boxes
            let boxWidth = 14
            -- Name
            drawString (ox + 1) oy (view chNickName ch)
            -- Stance
            draw (ox + boxWidth) oy (views chStance stanceChar ch)

            -- Weapon
            drawString (ox + 1) (oy + 1) $
                maybe  "<EMPTY>"
                      (take (fromIntegral boxWidth) . show)
                      (slotWrapperItem . primaryHandSlot $ ch)

            -- Clip
            --let clipStr = show cl <> "/" <> show mcl
            --C.moveCursor (oy + 1) (ox + 1 + boxWidth - fromIntegral (length clipStr))
            --C.drawString clipStr

            -- Health bar
            let hp    = view chHealthPoints ch
                mhp   = view chMaxHealthPoints ch
                hBars = floor ((fromIntegral hp / fromIntegral mhp ∷ Float) * fromIntegral boxWidth)
                hDots = fromIntegral boxWidth - hBars
            drawString (ox + 1) (oy + 3)
                (replicate hBars '|' <> replicate hDots '.')


        --shorten ∷ Word → String → String
        --shorten l = (++".") . take (l-1)

        stanceChar ∷ Stance → Char
        stanceChar Upright = '^'
        stanceChar Crouch  = ':'
        stanceChar Prone   = '_'


        drawBorders ∷ C.Update ()
        drawBorders = do
            len ← subtract watchLength . snd <$> C.windowSize
            drawString 0 2 $ genericReplicate len '-'
            drawString 0 10 $ genericReplicate len '-'


drawWatch ∷ (RenderAPI r, Monad r) ⇒ Bool → Int → r (RenderAction ())
drawWatch sel turns = do
    white ← style sColorWhite
    green ← style sColorGreen
    pure $ RenderAction $ do
        C.setColor white
        C.windowSize >>= \ox → drawList (subtract watchLength . snd $ ox) 0 watch


        C.setColor $ if sel
                        then green
                        else white
        drawTime (fromSeconds turns)
    where
        drawTime ∷ WatchData → C.Update ()
        drawTime wd = do
            ox ← subtract watchLength . snd <$> C.windowSize
            drawList (ox + 8)  4 (digit . fst . numberToDigits . view wdHours $ wd)
            drawList (ox + 12) 4 (digit . snd . numberToDigits . view wdHours $ wd)
            drawList (ox + 16) 4 dots
            drawList (ox + 18) 4 (digit . fst . numberToDigits . view wdMinutes $ wd)
            drawList (ox + 22) 4 (digit . snd . numberToDigits . view wdMinutes $ wd)
            drawList (ox + 27) 6 (smallDigit . fst . numberToDigits . view wdSeconds $ wd)
            drawList (ox + 30) 6 (smallDigit . snd . numberToDigits . view wdSeconds $ wd)
            drawString (ox + 27) 3 (view wdWeekDay wd <> " " <> views wdDay paddedDigit wd)
            drawString (ox + 27) 4 (views wdMonth paddedDigit wd <> "-" <> views wdYear show wd)

        paddedDigit = reverse . take 2 . reverse . ("0"<>) . show

        dots ∷ [String]
        dots = [" "
               ,"'"
               ," "
               ,"'"
               ," "
               ]


clearStatus ∷ RenderAction ()
clearStatus = statusOrigin >>= \(start,padding,len) → RenderAction $ do
    drawList start padding $
        replicate 5 $
            genericReplicate len ' '


drawStatus ∷ (RenderAPI r, Monad r) ⇒ Bool → String → r (RenderAction ())
drawStatus sel msg = do
    green ← style sColorGreen
    white ← style sColorWhite
    pure $ clearStatus *> statusOrigin >>= \(start,padding,len) → RenderAction $ do
        C.setColor $ if sel
                        then green
                        else white

        draw (fromIntegral start + len `div` 2) 3 '▲'
        draw (fromIntegral start + len `div` 2) 9 '▼'

        -- Message
        let lns = if null msg
                    then []
                    else fmt (fromIntegral len) length " " (words msg)
        drawList start padding lns


statusOrigin ∷ RenderAction (Int, Int, Int)
statusOrigin = RenderAction $ do
    let start   = teamBoxesLength
        padding = 4
    len ← subtract (start + watchLength + padding) . fromIntegral . snd <$> C.windowSize
    pure (start, padding, len)



drawCharacterSheet ∷ (RenderAPI r, Monad r, Show f) ⇒ Character i c f → r (RenderAction ())
drawCharacterSheet ch = screenSize >>= \(rows, cols) →
    pure $ RenderAction $ do
        let x = (cols - listDrawingWidth characterSheet) `div` 2
            y = (rows - listDrawingHeight characterSheet - 1) `div` 2
        C.resizeWindow (listDrawingHeight characterSheet + 1) (listDrawingWidth characterSheet)
        C.moveWindow y x
        drawList 0 0 characterSheet

        drawString 4 2  (pad 29 $ view chLastName ch <> ", " <> view chName ch)
        drawString 18 4 (padL 15 $ view chNickName ch)
        drawString 18 5 (padL 15 $ views chHandedness show ch)
        drawString 18 6 (padL 15 $ views chFaction show ch)
        drawString 18 7 (padL 15 $ views chHealthPoints show ch <> "/" <> views chMaxHealthPoints show ch)

        drawThreeDigit 21 11 (view (chMeleeCombat.mcsRemainingPoints) ch)
        drawThreeDigit 21 13 (view (chMeleeCombat.mcsBarehanded) ch)
        drawThreeDigit 21 14 (view (chMeleeCombat.mcsKnives) ch)
        drawThreeDigit 21 15 (view (chMeleeCombat.mcsSwords) ch)
        drawThreeDigit 21 16 (view (chMeleeCombat.mcsStaves) ch)
        drawThreeDigit 21 17 (view (chMeleeCombat.mcsMaces) ch)
        drawThreeDigit 21 19 (views chMeleeCombat sumMelee ch)

        drawThreeDigit 21 22 (view (chThrowing.tsRemainingPoints) ch)
        drawThreeDigit 21 24 (view (chThrowing.tsGrenades) ch)
        drawThreeDigit 21 25 (view (chThrowing.tsKnives) ch)
        drawThreeDigit 21 26 (view (chThrowing.tsShurikens) ch)
        drawThreeDigit 21 27 (view (chThrowing.tsStickies) ch)
        drawThreeDigit 21 29 (views chThrowing sumThrowing ch)

        drawThreeDigit 46 11 (view (chRangedCombat.rcsRemainingPoints) ch)
        drawThreeDigit 46 13 (view (chRangedCombat.rcsGuns) ch)
        drawThreeDigit 46 14 (view (chRangedCombat.rcsSmgs) ch)
        drawThreeDigit 46 15 (view (chRangedCombat.rcsShotguns) ch)
        drawThreeDigit 46 16 (view (chRangedCombat.rcsAssault) ch)
        drawThreeDigit 46 17 (view (chRangedCombat.rcsSniper) ch)
        drawThreeDigit 46 18 (view (chRangedCombat.rcsBows) ch)
        drawThreeDigit 46 19 (view (chRangedCombat.rcsCrossbows) ch)
        drawThreeDigit 46 20 (view (chRangedCombat.rcsPlasma) ch)
        drawThreeDigit 46 21 (view (chRangedCombat.rcsLasers) ch)
        drawThreeDigit 46 23 (views chRangedCombat sumRanged ch)

        drawThreeDigit 46 26 (view (chInfiltration.isRemainingPoints) ch)
        drawThreeDigit 46 28 (view (chInfiltration.isBlendInShadows) ch)
        drawThreeDigit 46 29 (view (chInfiltration.isUseOfCover) ch)
        drawThreeDigit 46 30 (view (chInfiltration.isSilentMovement) ch)
        drawThreeDigit 46 31 (view (chInfiltration.isCoverSwitchManeuver) ch)
        drawThreeDigit 46 33 (views chInfiltration sumInfiltration ch)

        drawThreeDigit 71 11 (view (chEngineering.esRemainingPoints) ch)
        drawThreeDigit 71 13 (view (chEngineering.esAssembly) ch)
        drawThreeDigit 71 14 (view (chEngineering.esModding) ch)
        drawThreeDigit 71 15 (view (chEngineering.esRepair) ch)
        drawThreeDigit 71 16 (view (chEngineering.esAnalysis) ch)
        drawThreeDigit 71 17 (view (chEngineering.esJuryrigging) ch)
        drawThreeDigit 71 19 (views chEngineering sumEngineering ch)

        drawThreeDigit 71 22 (view (chCommunication.ssRemainingPoints) ch)
        drawThreeDigit 71 24 (view (chCommunication.ssSmallTalk) ch)
        drawThreeDigit 71 25 (view (chCommunication.ssBodyLanguage) ch)
        drawThreeDigit 71 26 (view (chCommunication.ssNeurolinguisticProgramming) ch)
        drawThreeDigit 71 27 (view (chCommunication.ssHaggle) ch)
        drawThreeDigit 71 28 (view (chCommunication.ssInterrogation) ch)
        drawThreeDigit 71 29 (view (chCommunication.ssSeduction) ch)
        drawThreeDigit 71 31 (views chCommunication sumCommunication ch)
    where
        drawThreeDigit x y = drawString x y . padL 3 . show


listDrawingWidth ∷ (Num n) ⇒ [String] → n
listDrawingWidth = fromIntegral . length . head


listDrawingHeight ∷ (Num n) ⇒ [String] → n
listDrawingHeight = fromIntegral . length


characterSheet ∷ [String]
characterSheet =
    [ "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓  "
    , "┃ ┌───────────────────────────────┐                                          ┃─┐"
    , "┃ │ Carla D'Addario               │                                          ┃/│"
    , "┃ ├───────────────────────────────┤                                          ┃/│"
    , "┃ │ Nickname            La Piovra │                                          ┃/│"
    , "┃ │ Handedness               Left │                                          ┃/│"
    , "┃ │ Faction                 Carla │                                          ┃/│"
    , "┃ │ HP/Max                  10/10 │                                          ┃/│"
    , "┃ └───────────────────────────────┘                                          ┃/│"
    , "┃╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴┨/│"
    , "┃ ┌──────────────────────┐ ┌──────────────────────┐ ┌──────────────────────┐ ┃/│"
    , "┃ │ Melee combat   ╵ 000 │ │ Ranged combat  ╵ 000 │ │ Engineering    ╵ 000 │ ┃/│"
    , "┃ ├────────────────┴─────┤ ├────────────────┴─────┤ ├────────────────┴─────┤ ┃/│"
    , "┃ │ Barehanded     ╵ 000 │ │ Guns           ╵ 000 │ │ Assembly       ╵ 000 │ ┃/│"
    , "┃ │ Knives         ╵ 000 │ │ SMGs           ╵ 000 │ │ Modding        ╵ 000 │ ┃/│"
    , "┃ │ Swords         ╵ 000 │ │ Shotguns       ╵ 000 │ │ Repair         ╵ 000 │ ┃/│"
    , "┃ │ Staves         ╵ 000 │ │ Assault        ╵ 000 │ │ Analysis       ╵ 000 │ ┃/│"
    , "┃ │ Maces          ╵ 000 │ │ Sniper         ╵ 000 │ │ Juryrigging    ╵ 000 │ ┃/│"
    , "┃ ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ │ Bows           ╵ 000 │ ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ ┃/│"
    , "┃ │ Total          ╵ 000 │ │ Crossbows      ╵ 000 │ │ Total          ╵ 000 │ ┃/│"
    , "┃ └────────────────┴─────┘ │ Plasma         ╵ 000 │ └────────────────┴─────┘ ┃/│"
    , "┃ ┌──────────────────────┐ │ Lasers         ╵ 000 │ ┌──────────────────────┐ ┃/│"
    , "┃ │ Throwing       ╵ 000 │ ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ │ Communication  ╵ 000 │ ┃/│"
    , "┃ ├────────────────┴─────┤ │ Total          ╵ 000 │ ├────────────────┴─────┤ ┃/│"
    , "┃ │ Grenades       ╵ 000 │ └────────────────┴─────┘ │ Small talk     ╵ 000 │ ┃/│"
    , "┃ │ Knives         ╵ 000 │ ┌──────────────────────┐ │ Body language  ╵ 000 │ ┃/│"
    , "┃ │ Shurikens      ╵ 000 │ │ Infiltration   ╵ 000 │ │ Neurolin.prog. ╵ 000 │ ┃/│"
    , "┃ │ Stickies       ╵ 000 │ ├────────────────┴─────┤ │ Haggle         ╵ 000 │ ┃/│"
    , "┃ ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ │ Blend in shad. ╵ 000 │ │ Interrogation  ╵ 000 │ ┃/│"
    , "┃ │ Total          ╵ 000 │ │ Cover          ╵ 000 │ │ Seduction      ╵ 000 │ ┃/│"
    , "┃ └────────────────┴─────┘ │ Silent moveme. ╵ 000 │ ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ ┃/│"
    , "┃                          │ Maneuvers      ╵ 000 │ │ Total          ╵ 000 │ ┃/│"
    , "┃                          ├╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╵╴╴╴╴╴┤ └────────────────┴─────┘ ┃/│"
    , "┃                          │ Total          ╵ 000 │                          ┃/│"
    , "┃                          └────────────────┴─────┘                          ┃/│"
    , "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛/│"
    , " └─────────────────────────────────────────────────────────────────────────────┘"
    ]


drawEquipmentDoll ∷ (RenderAPI r, Monad r, Show i) ⇒ Character i c f → r (RenderAction ())
drawEquipmentDoll ch = screenSize >>= \(rows, cols) →
    pure $ RenderAction $ do
        let x = (cols - listDrawingWidth equipmentDoll) `div` 2
            y = (rows - listDrawingHeight equipmentDoll - 1) `div` 2
        C.resizeWindow (listDrawingHeight equipmentDoll + 1) (listDrawingWidth equipmentDoll)
        C.moveWindow y x
        drawList 0 0 equipmentDoll

        drawString  2  1 (pad  18 $ view chLastName ch <> ", " <> view chName ch)
        drawString 30  4 (padC 17 $ views (chEquipment.eqHead)       showItem ch)
        drawString 30  9 (padC 17 $ views (chEquipment.eqTorso)      showItem ch)
        drawString 30 13 (padC 17 $ views (chEquipment.eqBack)       showItem ch)
        drawString 30 21 (padC 17 $ views (chEquipment.eqBelt)       showItem ch)
        drawString 11 11 (padC 17 $ views (chEquipment.eqRightArm)   showItem ch)
        drawString  9 25 (padC 17 $ views (chEquipment.eqRightHand)  showItem ch)
        drawString 49 11 (padC 17 $ views (chEquipment.eqLeftArm)    showItem ch)
        drawString 51 25 (padC 17 $ views (chEquipment.eqLeftHand)   showItem ch)
        drawString 20 29 (padC 17 $ views (chEquipment.eqRightThigh) showItem ch)
        drawString 40 29 (padC 17 $ views (chEquipment.eqLeftThigh)  showItem ch)
        drawString 20 36 (padC 17 $ views (chEquipment.eqRightShin)  showItem ch)
        drawString 40 36 (padC 17 $ views (chEquipment.eqLeftShin)   showItem ch)
        drawString 20 43 (padC 17 $ views (chEquipment.eqRightFoot)  showItem ch)
        drawString 40 43 (padC 17 $ views (chEquipment.eqLeftFoot)   showItem ch)
    where
        showItem ∷ (Show s) ⇒ Slot o t s → String
        showItem = maybe "<EMPTY>" show . view sItem


equipmentDoll ∷ [String]
equipmentDoll =
    [ "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓  "
    , "┃ Carla D'Addario    ╵                                                       ┃─┐"
    , "┃╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴┘       ┌─────────────────┐                             ┃/│"
    , "┃                            │       Head      │                             ┃/│"
    , "┃                            │      <GEAR>     │                             ┃/│"
    , "┃                            └─────────────────┘                             ┃/│"
    , "┃                                     |                                      ┃/│"
    , "┃                    +-------┌─────────────────┐-------+                     ┃/│"
    , "┃                   /        │      Torso      │        \\                    ┃/│"
    , "┃         ┌─────────────────┐│      <GEAR>     │┌─────────────────┐          ┃/│"
    , "┃         │      R. arm     │└─────────────────┘│      L. arm     │          ┃/│"
    , "┃         │      <GEAR>     │┌─────────────────┐│      <GEAR>     │          ┃/│"
    , "┃         └─────────────────┘│       Back      │└─────────────────┘          ┃/│"
    , "┃                   |        │      <GEAR>     │        |                    ┃/│"
    , "┃                   |        └─────────────────┘        |                    ┃/│"
    , "┃                   |                 |                 |                    ┃/│"
    , "┃                   |                 |                 |                    ┃/│"
    , "┃                   |                 |                 |                    ┃/│"
    , "┃                   |                 |                 |                    ┃/│"
    , "┃                   |        ┌─────────────────┐        |                    ┃/│"
    , "┃                   |        │       Belt      │        |                    ┃/│"
    , "┃                   |        │      <GEAR>     │        |                    ┃/│"
    , "┃                   |        └─────────────────┘        |                    ┃/│"
    , "┃       ┌─────────────────┐           |           ┌─────────────────┐        ┃/│"
    , "┃       │     R. hand     │      +----+----+      │     L. hand     │        ┃/│"
    , "┃       │     <GEAR>      │     /           \\     │     <GEAR>      │        ┃/│"
    , "┃       └─────────────────┘    /             \\    └─────────────────┘        ┃/│"
    , "┃                  ┌─────────────────┐ ┌─────────────────┐                   ┃/│"
    , "┃                  │    R. thigh     │ │     L. thigh    │                   ┃/│"
    , "┃                  │     <GEAR>      │ │     <GEAR>      │                   ┃/│"
    , "┃                  └─────────────────┘ └─────────────────┘                   ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                  ┌─────────────────┐ ┌─────────────────┐                   ┃/│"
    , "┃                  │    R. shin      │ │     L. shin     │                   ┃/│"
    , "┃                  │     <GEAR>      │ │     <GEAR>      │                   ┃/│"
    , "┃                  └─────────────────┘ └─────────────────┘                   ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                              |             |                               ┃/│"
    , "┃                  ┌─────────────────┐ ┌─────────────────┐                   ┃/│"
    , "┃                  │    R. foot      │ │     L. foot     │                   ┃/│"
    , "┃                  │     <GEAR>      │ │     <GEAR>      │                   ┃/│"
    , "┃                  └─────────────────┘ └─────────────────┘                   ┃/│"
    , "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛/│"
    , " └─────────────────────────────────────────────────────────────────────────────┘"
    ]


drawInformation ∷ (RenderAPI r, Monad r) ⇒ r (RenderAction ())
drawInformation = do
    (V2 x y)← withScroll (view sdPosition)
    (V2 w h)← withScroll (view sdSize)
    mt      ← withScroll (view sdTitle)
    lw      ← withScroll lineWidth
    vl      ← withScroll visibleLines
    iat     ← withScroll isAtTop
    hml     ← withScroll hasMoreLines
    pure $ RenderAction $ do
        C.resizeWindow h w
        C.moveWindow y x
        C.drawBorder (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '│' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '─' [])
                     (Just $ C.Glyph '╭' [])
                     (Just $ C.Glyph '╮' [])
                     (Just $ C.Glyph '╰' [])
                     (Just $ C.Glyph '╯' [])
        (rows, cols) ← C.windowSize
        forM_ mt $
            drawTitle cols
        V.imapM_ (\i → drawString 2 (i + bool 1 4 (isJust mt)) . pad lw) vl
        draw (cols - 3) 1          (bool ' ' '▲' iat)
        draw (cols - 3) (rows - 2) (bool ' ' '▼' hml)
    where
        drawTitle ∷ Integer → String → C.Update ()
        drawTitle cols tn = do
                let trimmed    = take (fromIntegral cols - 7) tn
                    trimmedLen = fromIntegral (length trimmed)
                draw 0 2 '├'
                drawString 2 1 trimmed
                draw (trimmedLen + 3) 0 '┬'
                draw (trimmedLen + 3) 2 '╯'
                C.moveCursor 2 1
                C.drawLineH (Just $ C.Glyph '─' []) (trimmedLen + 2)
                draw (trimmedLen + 3) 1 '│'


drawChoice ∷ (RenderAPI r, Monad r) ⇒ r (RenderAction ())
drawChoice = do
    (V2 x y) ← withChoiceData (view cdPosition)
    (V2 w h) ← withChoiceData (view cdSize)
    os       ← withChoiceData (view cdOptions)
    cs       ← withChoiceData (view cdCurrentSelection)
    pure $ RenderAction $ do
        C.resizeWindow h w
        C.moveWindow y x
        C.drawBorder (Just $ C.Glyph '╷' [])
                     (Just $ C.Glyph '╷' [])
                     (Just $ C.Glyph '╶' [])
                     (Just $ C.Glyph '╶' [])
                     (Just $ C.Glyph '╭' [])
                     (Just $ C.Glyph '╮' [])
                     (Just $ C.Glyph '╰' [])
                     (Just $ C.Glyph '╯' [])
        V.imapM_ drawChoiceLine os
        drawSelectionWidget cs
    where
        drawChoiceLine i l = do
            (_, columns) ← C.windowSize
            let lineStart     = 5
            let maxLineLength = columns - lineStart - 2
            C.moveCursor (fromIntegral i + 1) lineStart
            C.drawString (take (fromIntegral maxLineLength) l)
        drawSelectionWidget i = do
            let widgetStart = 2
            C.moveCursor (fromIntegral i + 1) widgetStart
            C.drawGlyph (C.Glyph '»' [])


drawComputer ∷ ComputerData → RenderAction ()
drawComputer cd = RenderAction $ do
    C.resizeWindow 30 60
    C.moveWindow 1 1
    C.drawBorder (Just $ C.Glyph '╷' [])
                 (Just $ C.Glyph '╷' [])
                 (Just $ C.Glyph '╶' [])
                 (Just $ C.Glyph '╶' [])
                 (Just $ C.Glyph '╭' [])
                 (Just $ C.Glyph '╮' [])
                 (Just $ C.Glyph '╰' [])
                 (Just $ C.Glyph '╯' [])
    -- TODO need fmt here
    drawList 2 1 (view cdFrameBuffer cd)
    drawString 2 28 (pad 58 ("> " <> view cdInputBuffer cd))

--------------------------------------------------------------------------------

lookupMaterial ∷ (RenderAPI r, Monad r) ⇒ String → r Material
lookupMaterial n = do
    umat ← style sUnknown
    fromMaybe umat . M.lookup n <$> style sMaterials


draw' ∷ (Integral a) ⇒ V2 a → Char → Material → RenderAction ()
draw' (V2 x y) c m = RenderAction $ do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c m)


draw ∷ (Integral a) ⇒ a → a → Char → C.Update ()
draw x y c = do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawGlyph (C.Glyph c [])


drawString ∷ (Integral a) ⇒ a → a → String → C.Update ()
drawString x y s = do
    C.moveCursor (fromIntegral y) (fromIntegral x)
    C.drawString s


drawList ∷ (Integral a) ⇒ a → a → [String] → C.Update ()
drawList x y =
    traverse_ (uncurry (drawString x))
    . zip [y..]


digit ∷ Int → [String]
digit 0 = [ " ━ "
          , "┃ ┃"
          , "   "
          , "┃ ┃"
          , " ━ "
          ]
digit 1 = [ "   "
          , "  ┃"
          , "   "
          , "  ┃"
          , "   "
          ]
digit 2 = [ " ━ "
          , "  ┃"
          , " ━ "
          , "┃  "
          , " ━ "
          ]
digit 3 = [ " ━ "
          , "  ┃"
          , " ━ "
          , "  ┃"
          , " ━ "
          ]
digit 4 = [ "   "
          , "┃ ┃"
          , " ━ "
          , "  ┃"
          , "   "
          ]
digit 5 = [ " ━ "
          , "┃  "
          , " ━ "
          , "  ┃"
          , " ━ "
          ]
digit 6 = [ " ━ "
          , "┃  "
          , " ━ "
          , "┃ ┃"
          , " ━ "
          ]
digit 7 = [ " ━ "
          , "┃ ┃"
          , "   "
          , "  ┃"
          , "   "
          ]
digit 8 = [ " ━ "
          , "┃ ┃"
          , " ━ "
          , "┃ ┃"
          , " ━ "
          ]
digit 9 = [ " ━ "
          , "┃ ┃"
          , " ━ "
          , "  ┃"
          , " ━ "
          ]
digit _ = digit 8


smallDigit ∷ Int → [String]
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


loremIpsum ∷ String
loremIpsum = unwords
    [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
    , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
    , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
    , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
    , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
    , "culpa qui officia deserunt mollit anim id est laborum"
    ]

