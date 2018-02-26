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

, clear
, drawMap
, drawHud
, drawStatus
, drawCharacterSheet
) where

import Safe                      (atDef)
import Control.Lens              (makeLenses, use, uses, view, views)
import Control.Monad.Trans       (lift)
import Control.Monad.State       (MonadState, StateT, runStateT)
import Linear                    (V2(V2))
import Data.Semigroup            ((<>))
import Data.Maybe                (fromMaybe)
import Data.Foldable             (traverse_, forM_)
import Data.Char                 (digitToInt)
import Data.List                 (intercalate)

import qualified UI.NCurses  as C
import qualified Data.Map    as M
import qualified Data.Vector as V

import Dreamnet.World       (o_state)
import Dreamnet.Utils       (lines')
import Dreamnet.Character   
import Dreamnet.CoordVector
import Dreamnet.Visibility

import Design.DesignAPI     (GameState(..), DreamnetCharacter)

--------------------------------------------------------------------------------

data WatchData = WatchData {
      _wd_hours   ∷ Word
    , _wd_minutes ∷ Word
    , _wd_seconds ∷ Word
    , _wd_weekDay ∷ String
    , _wd_day     ∷ Word
    , _wd_month   ∷ Word
    , _wd_year    ∷ Word
    }
makeLenses ''WatchData


fromSeconds ∷ Word → WatchData
fromSeconds t =
    let s = t `mod` 60
        m = t `div` 60 `mod` 60
        h = m `div` 60 `mod` 60
    in  WatchData {
          _wd_hours   = h
        , _wd_minutes = m
        , _wd_seconds = s
        , _wd_weekDay = "Tue"
        , _wd_day     = 12
        , _wd_month   = 4
        , _wd_year    = 2183
        }


numberToDigits ∷ Word → (Word, Word)
numberToDigits i =
    let f ix = fromIntegral . digitToInt . flip (atDef '0') ix . reverse . show
    in  (f 1 i, f 0 i)

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
      _rd_styles     ∷ Styles

    , _rd_mainWindow ∷ C.Window
    , _rd_hudWindow  ∷ C.Window
    , _rd_uiWindow   ∷ C.Window
    }

makeLenses ''RendererEnvironment

--------------------------------------------------------------------------------

newtype RenderAction a = RenderAction { runAction ∷ C.Update a }
                       deriving (Functor, Applicative, Monad)


class (MonadState RendererEnvironment r) ⇒ MonadRender r where
    updateMain ∷ RenderAction () → r ()
    updateHud  ∷ RenderAction () → r ()
    updateUi   ∷ RenderAction () → r ()
    screenSize ∷ r (Integer, Integer)
    

-- TODO double buffering
newtype RendererF a = RendererF { runRendererF ∷ StateT RendererEnvironment C.Curses a }
                    deriving (Functor, Applicative, Monad, MonadState RendererEnvironment)


instance MonadRender RendererF where
    updateMain ac = use rd_mainWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)

    updateHud ac = use rd_hudWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)

    updateUi ac = use rd_uiWindow >>= RendererF . lift . (`C.updateWindow` runAction ac)

    screenSize = RendererF $ lift C.screenSize


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

clear ∷ RenderAction ()
clear = RenderAction $ C.clear


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
        drawTile u k i (c, m, v) = uncurry (draw' $ coordLin' (fromIntegral w) i) $
                                     case v of
                                         Unknown → (' ', u)
                                         Known   → (c, k)
                                         Visible → (c, m)

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
watchLength = fromIntegral . (+1) . length . head $ watch


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
teamBoxesLength = fromIntegral . length . head $ teamBoxes


drawHud ∷ (MonadRender r) ⇒ GameState → Int → Int → [DreamnetCharacter] → Word → Int → r (RenderAction ())
drawHud gs hms am team turns button = do
    white   ← use (rd_styles.s_colorWhite)
    green   ← use (rd_styles.s_colorGreen)
    blue ← use (rd_styles.s_colorBlue)
    pure $ RenderAction $ do
        C.setColor white
        ox ← subtract watchLength . snd <$> C.windowSize

        drawBorders
        drawList 0 1 teamBoxes

        forM_ (zip [0.. ] (take 6 team)) $ \(ix, ch) → do
            setDataColor ix white green blue
            drawData
                (fromIntegral (ix `mod` 3) * 17 + 1)
                (fromIntegral (ix `div` 3) *  5 + 2)
                ch
                


        C.setColor white
        drawList ox 0 watch

        C.setColor $ if gs == HudWatch
                        then green
                        else white
            
        drawTime (fromSeconds turns)
    where
        setDataColor ∷ Int → C.ColorID → C.ColorID → C.ColorID → C.Update ()
        setDataColor i none hud active
            | gs == HudTeam && hms == i = C.setColor hud
            |                   am == i = C.setColor active
            | otherwise                 = C.setColor none

        drawData ∷ Integer → Integer → DreamnetCharacter → C.Update ()
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
            drawString (ox + 1) oy (view ch_nickName ch)
            -- Stance
            draw (ox + boxWidth) oy (views ch_stance stanceChar ch)

            -- Weapon
            drawString (ox + 1) (oy + 1) $ 
                maybe ("<EMPTY>")
                      (take (fromIntegral boxWidth) . show . view o_state)
                      (slottedItem . view (ch_primaryHand ch) $ ch)
            
            -- Clip
            --let clipStr = show cl <> "/" <> show mcl
            --C.moveCursor (oy + 1) (ox + 1 + boxWidth - fromIntegral (length clipStr))
            --C.drawString clipStr

            -- Health bar
            let hp    = view ch_healthPoints ch
                mhp   = view ch_maxHealthPoints ch
                hBars = floor ((fromIntegral hp / fromIntegral mhp ∷ Float) * fromIntegral boxWidth)
                hDots = fromIntegral boxWidth - hBars
            drawString (ox + 1) (fromIntegral oy + 3)
                (concat [ replicate hBars '|'
                        , replicate hDots '.'
                        ])
            

        --shorten ∷ Word → String → String
        --shorten l = (++".") . take (l-1)

        drawTime ∷ WatchData → C.Update ()
        drawTime wd = do
            ox ← subtract watchLength . snd <$> C.windowSize
            drawList (ox + 8)  4 (digit . fst . numberToDigits . view wd_hours $ wd)
            drawList (ox + 12) 4 (digit . snd . numberToDigits . view wd_hours $ wd)
            drawList (ox + 16) 4 dots
            drawList (ox + 18) 4 (digit . fst . numberToDigits . view wd_minutes $ wd)
            drawList (ox + 22) 4 (digit . snd . numberToDigits . view wd_minutes $ wd)
            drawList (ox + 27) 6 (smallDigit . fst . numberToDigits . view wd_seconds $ wd)
            drawList (ox + 30) 6 (smallDigit . snd . numberToDigits . view wd_seconds $ wd)
            drawString (ox + 27) 3 (view wd_weekDay wd <> " " <> views wd_day show wd)
            drawString (ox + 27) 4 (views wd_month show wd <> "-" <> views wd_year show wd)

        dots ∷ [String]
        dots = [" "
               ,"'"
               ," "
               ,"'"
               ," "
               ]

        stanceChar ∷ Stance → Char
        stanceChar Upright = '^'
        stanceChar Crouch  = '~'
        stanceChar Prone   = '_'


        drawBorders ∷ C.Update ()
        drawBorders = do
            len ← fromIntegral . subtract watchLength . snd <$> C.windowSize
            drawString 0 2 $ replicate len '-'
            drawString 0 10 $ replicate len '-'


drawStatus ∷ (MonadRender r) ⇒ GameState → String → r (RenderAction ())
drawStatus gs msg = do
    green ← use (rd_styles.s_colorGreen)
    white ← use (rd_styles.s_colorWhite)
    pure $ RenderAction $ do
        let start       = teamBoxesLength
            padding     = 4
        len ← subtract (start + watchLength + padding) . fromIntegral . snd <$> C.windowSize

        C.setColor $ if gs == HudMessages
                        then green
                        else white

        C.moveCursor 3 (fromIntegral start + len `div` 2)
        C.drawGlyph (C.Glyph '▲' [])
        C.moveCursor 9 (fromIntegral start + len `div` 2)
        C.drawGlyph (C.Glyph '▼' [])
        

        -- Clear
        drawList (fromIntegral start) padding $
            replicate 5 $
                replicate (fromIntegral len) ' '

        -- Message
        let lns = if null msg
                    then []
                    else lines' (fromIntegral len) length " " (words msg)
        drawList (fromIntegral start) padding lns


drawCharacterSheet ∷ (MonadRender r) ⇒ DreamnetCharacter → r (RenderAction ())
drawCharacterSheet ch = screenSize >>= \(rows, cols) →
    pure $ RenderAction $ do
        let x = (cols - characterSheetWidth) `div` 2
            y = (rows - characterSheetHeight) `div` 2
        C.resizeWindow characterSheetHeight characterSheetWidth
        C.moveWindow y x
        drawList 0 0 characterSheet

        drawString 4 2  (pad 29 $ view ch_name ch <> " " <> view ch_lastName ch) 
        drawString 18 4 (padL 15 $ view ch_nickName ch)
        drawString 18 5 (padL 15 $ views ch_handedness show ch)
        drawString 18 6 (padL 15 $ views ch_faction show ch)
        drawString 18 7 (padL 15 $ views ch_healthPoints show ch <> "/" <> views ch_maxHealthPoints show ch)

        drawThreeDigit 21 11 (view (ch_meleeCombat.mcs_remainingPoints) ch)
        drawThreeDigit 21 13 (view (ch_meleeCombat.mcs_barehanded) ch)
        drawThreeDigit 21 14 (view (ch_meleeCombat.mcs_knives) ch)
        drawThreeDigit 21 15 (view (ch_meleeCombat.mcs_swords) ch)
        drawThreeDigit 21 16 (view (ch_meleeCombat.mcs_staves) ch)
        drawThreeDigit 21 17 (view (ch_meleeCombat.mcs_maces) ch)
        drawThreeDigit 21 19 (views ch_meleeCombat sumMelee ch)

        drawThreeDigit 21 22 (view (ch_throwing.ts_remainingPoints) ch)
        drawThreeDigit 21 24 (view (ch_throwing.ts_grenades) ch)
        drawThreeDigit 21 25 (view (ch_throwing.ts_knives) ch)
        drawThreeDigit 21 26 (view (ch_throwing.ts_shurikens) ch)
        drawThreeDigit 21 27 (view (ch_throwing.ts_stickies) ch)
        drawThreeDigit 21 29 (views ch_throwing sumThrowing ch)

        drawThreeDigit 46 11 (view (ch_rangedCombat.rcs_remainingPoints) ch)
        drawThreeDigit 46 13 (view (ch_rangedCombat.rcs_guns) ch)
        drawThreeDigit 46 14 (view (ch_rangedCombat.rcs_smgs) ch)
        drawThreeDigit 46 15 (view (ch_rangedCombat.rcs_shotguns) ch)
        drawThreeDigit 46 16 (view (ch_rangedCombat.rcs_assault) ch)
        drawThreeDigit 46 17 (view (ch_rangedCombat.rcs_sniper) ch)
        drawThreeDigit 46 18 (view (ch_rangedCombat.rcs_bows) ch)
        drawThreeDigit 46 19 (view (ch_rangedCombat.rcs_crossbows) ch)
        drawThreeDigit 46 20 (view (ch_rangedCombat.rcs_plasma) ch)
        drawThreeDigit 46 21 (view (ch_rangedCombat.rcs_lasers) ch)
        drawThreeDigit 46 23 (views ch_rangedCombat sumRanged ch)

        drawThreeDigit 46 26 (view (ch_infiltration.is_remainingPoints) ch)
        drawThreeDigit 46 28 (view (ch_infiltration.is_blendInShadows) ch)
        drawThreeDigit 46 29 (view (ch_infiltration.is_useOfCover) ch)
        drawThreeDigit 46 30 (view (ch_infiltration.is_silentMovement) ch)
        drawThreeDigit 46 31 (view (ch_infiltration.is_coverSwitchManeuver) ch)
        drawThreeDigit 46 33 (views ch_infiltration sumInfiltration ch)

        drawThreeDigit 71 11 (view (ch_engineering.es_remainingPoints) ch)
        drawThreeDigit 71 13 (view (ch_engineering.es_assembly) ch)
        drawThreeDigit 71 14 (view (ch_engineering.es_modding) ch)
        drawThreeDigit 71 15 (view (ch_engineering.es_repair) ch)
        drawThreeDigit 71 16 (view (ch_engineering.es_analysis) ch)
        drawThreeDigit 71 17 (view (ch_engineering.es_juryrigging) ch)
        drawThreeDigit 71 19 (views ch_engineering sumEngineering ch)

        drawThreeDigit 71 22 (view (ch_communication.ss_remainingPoints) ch)
        drawThreeDigit 71 24 (view (ch_communication.ss_smallTalk) ch)
        drawThreeDigit 71 25 (view (ch_communication.ss_bodyLanguage) ch)
        drawThreeDigit 71 26 (view (ch_communication.ss_neurolinguisticProgramming) ch)
        drawThreeDigit 71 27 (view (ch_communication.ss_haggle) ch)
        drawThreeDigit 71 28 (view (ch_communication.ss_interrogation) ch)
        drawThreeDigit 71 29 (view (ch_communication.ss_seduction) ch)
        drawThreeDigit 71 31 (views ch_communication sumCommunication ch)
    where
        pad i  = (<>) <$> id <*> flip replicate ' ' . (i -) . length
        padL i = (<>) <$> flip replicate ' ' . (i -) . length <*> id
        drawThreeDigit x y = drawString x y . padL 3 . show
            


characterSheetWidth ∷ (Num n) ⇒ n
characterSheetWidth = fromIntegral . length . head $ characterSheet


characterSheetHeight ∷ (Num n) ⇒ n
characterSheetHeight = fromIntegral . (+1) . length $ characterSheet


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

--------------------------------------------------------------------------------

lookupMaterial ∷ (MonadRender r) ⇒ String → r Material
lookupMaterial n = use (rd_styles.s_unknown) >>= \umat →
    uses (rd_styles.s_materials) (fromMaybe umat . M.lookup n)


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


drawList ∷ Integer → Integer → [String] → C.Update ()
drawList x y =
    traverse_ (\(ix, l) → drawString x ix l)
    . zip [y..]


digit ∷ Word → [String]
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


loremIpsum ∷ String
loremIpsum = intercalate " "
    [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
    , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
    , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
    , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
    , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
    , "culpa qui officia deserunt mollit anim id est laborum"
    ]

