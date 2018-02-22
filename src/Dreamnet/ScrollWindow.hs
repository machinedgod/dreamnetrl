{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.ScrollWindow
( ScrollData

, createScrollData
, setTitle
, setText
, setLines
, scrollUp
, scrollDown

, clearScrollWindow
, moveWindow
, resizeWindow
, renderScrollWindow
) where


import Control.Lens   (makeLenses, view, views, (%~), (+~), (.~))
import Data.List      (intercalate)
import Data.Bool      (bool)
import Linear         (V2(V2), _x, _y)

import qualified Data.Vector as V (fromList, imapM_)
import qualified UI.NCurses  as C

import Dreamnet.Utils (lines')

--------------------------------------------------------------------------------

-- In case I forget: Data types here are INT and not WORD because WORD
-- wraps around instead of going into negative numbers or clipping.
-- Bit me in the ass nicely...
data ScrollData = ScrollData {
      _sd_lines     ∷ [String]
    , _sd_position  ∷ V2 Integer
    , _sd_size      ∷ V2 Integer
    , _sd_startLine ∷ Int
    , _sd_title     ∷ Maybe String

    , _sd_window    ∷ C.Window
    }

makeLenses ''ScrollData


createScrollData ∷ C.Curses ScrollData
createScrollData = do
    --(rows, columns) ← C.screenSize
    --let mainWidth  = columns
    --    mainHeight = rows

    let examineW = 40
        examineH = 10
        --examineX = (mainWidth - examineW) `div` 2
        --examineY = (mainHeight - examineH) `div` 2
        examineX = 2
        examineY = 1
    win ← C.newWindow examineH examineW examineY examineX

    --let lineWidth = fromIntegral $ examineW - 6 -- border, padding, arrow widgets
    --    maxLines  = fromIntegral $ examineH - 2
    pure $ ScrollData [] (V2 examineX examineY) (V2 examineW examineH) 0 Nothing win

--------------------------------------------------------------------------------

setTitle ∷ String → ScrollData → ScrollData
setTitle t = sd_title .~ Just t


setText ∷ String → ScrollData → ScrollData
setText s sd =
    let ls = intercalate [""] $ lines' (fromIntegral $ lineWidth sd) length " " . words <$> lines s
    in  setLines ls sd


setLines ∷ [String] → ScrollData → ScrollData
setLines ls = (sd_startLine .~ 0) . (sd_lines .~ ls)


scrollUp ∷ ScrollData → ScrollData
scrollUp = sd_startLine %~ (\i → max 0 (i - 1))


scrollDown ∷ ScrollData → ScrollData
scrollDown sd = let nsl = views sd_startLine (+1) sd -- <------ Where are these +1's coming from???
                    tlc = views sd_lines length sd
                in  if nsl <= tlc - (fromIntegral $ maxLines sd)
                        then sd_startLine +~ 1 $ sd
                        else sd


moveWindow ∷ V2 Integer → ScrollData → ScrollData
moveWindow v = sd_position .~ v


resizeWindow ∷ V2 Integer → ScrollData → ScrollData
resizeWindow v = sd_size .~ v

--------------------------------------------------------------------------------

clearScrollWindow ∷ ScrollData → C.Curses ()
clearScrollWindow sd = C.updateWindow (view sd_window sd) C.clear


renderScrollWindow ∷ ScrollData → C.Curses ()
renderScrollWindow sd =
    let win          = view sd_window sd
        startLine    = view sd_startLine sd
        visibleLines = views sd_lines (V.fromList . take (fromIntegral $ maxLines sd) . drop startLine) sd
        isAtTop      = views sd_startLine (==0) sd
        hasMoreLines = views sd_lines (\ls → length ls >= (startLine + 1) + (fromIntegral $ maxLines sd)) sd -- <-------------------------------- Like this one here too!
        (V2 x y)     = view sd_position sd
        (V2 w h)     = view sd_size sd
    in  C.updateWindow win $ do
            -- C.clear -- TODO only clear scroll lines
            C.moveWindow y x
            C.resizeWindow h w
            C.drawBorder (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '│' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '─' [])
                         (Just $ C.Glyph '╭' [])
                         (Just $ C.Glyph '╮' [])
                         (Just $ C.Glyph '╰' [])
                         (Just $ C.Glyph '╯' [])
            (rows, columns) ← C.windowSize
            drawTitle columns
            V.imapM_ drawLine visibleLines
            drawUpWidget columns (not isAtTop)
            drawDownWidget rows columns hasMoreLines
    where
        drawLine ∷ Int → String → C.Update ()
        drawLine i s = case view sd_title sd of
            Nothing → do
                C.moveCursor (fromIntegral i + 1) 2
                C.drawString s
            Just _ → do
                C.moveCursor (fromIntegral i + 4) 2 -- +4 to account for the title
                C.drawString s

        drawUpWidget ∷ Integer → Bool → C.Update ()
        drawUpWidget cols b = do
            C.moveCursor 1 (cols - 3)
            C.drawGlyph (C.Glyph (bool ' ' '▲' b) [])

        drawDownWidget ∷ Integer → Integer → Bool → C.Update ()
        drawDownWidget rows cols b = do
            C.moveCursor (rows - 2) (cols - 3)
            C.drawGlyph (C.Glyph (bool ' ' '▼' b) [])

        drawTitle ∷ Integer → C.Update ()
        drawTitle cols = do
            let mtn = take (fromIntegral cols - 7) <$> view sd_title sd
            case mtn of
                Nothing → pure ()
                Just tn → do
                    C.moveCursor 2 0
                    C.drawGlyph (C.Glyph '├' [])
                    C.moveCursor 1 2
                    C.drawString tn
                    C.moveCursor 0 (fromIntegral (length tn) + 3)
                    C.drawGlyph (C.Glyph '┬' [])
                    C.moveCursor 2 (fromIntegral (length tn) + 3)
                    C.drawGlyph (C.Glyph '╯' [])
                    C.moveCursor 2 1
                    C.drawLineH (Just $ C.Glyph '─' []) (fromIntegral (length tn) + 2)
                    C.moveCursor 1 (fromIntegral (length tn) + 3)
                    C.drawGlyph (C.Glyph '│' [])

--------------------------------------------------------------------------------

lineWidth ∷ ScrollData → Integer
lineWidth = subtract 6 . view (sd_size._x) -- (-6) border, padding, arrow widgets


-- TODO should depend on the title being present!
maxLines ∷ ScrollData → Integer
maxLines = subtract 2 . view (sd_size._y)

