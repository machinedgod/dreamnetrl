{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BlockArguments #-}

module Main
where

import Control.Monad.Random  (evalRandT, mkStdGen)
import Dreamnet.Dreamnet

-- WorldMap3D DEBUG
--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad
import Data.Either
import Data.Bool
import Linear hiding (column)

import qualified Data.Vector as V
import qualified UI.NCurses  as C

import Dreamnet.Engine.Visibility hiding (height)
import Dreamnet.Engine.TileMap
import Dreamnet.Engine.WorldMap
import Dreamnet.Engine.Object
import Dreamnet.World
import Dreamnet.Rendering.Renderer


{-
newMap = let brandNew  = newWorldMap (V3 5 5 2) '.' Nothing
             addGuitar = replaceCell (V3 1 1 0) (Just 'G')
         in  execWorldMap addGuitar brandNew


newDrawMap ∷ RendererM C.Curses (RenderAction ())
newDrawMap = drawMap (fromMaybe ';') (const "default") 5 (view wm_data newMap) (V.replicate 25 Visible)
         -}


barWorldMap ∷ IO (WorldMap Char (Maybe Char))
barWorldMap = worldMap <$> tmap
    where
        tmap ∷ IO TileMap
        tmap = fromRight undefined <$> loadTileMap "res/bar"

        worldMap ∷ TileMap → WorldMap Char (Maybe Char)
        worldMap tm = 
            let baseF      = Right . view t_char
                contentF t = bool (Right Nothing) (Right $ Just $ view t_char t) (view t_char t == ' ')
            in  fromRight (error "Something happened while constructing a WorldMap") . fromTileMap baseF contentF $ tm


{-
 -- TODO FIX it to work with new drawMap style
renderTilemap ∷ TileMap → IO ()
renderTilemap tm = void $ C.runCurses do
    void $ execRenderer renderAc =<< newRenderEnvironment
    flip C.getEvent Nothing =<< C.defaultWindow
    where
        renderAc =
            let objFun  = id
                matFun  = const "default"
                mapW    = tm ^. m_width
                rawData = V.fromList $ tm ^.. m_layers.traversed.l_data.folded
                visData = V.replicate (length rawData) Visible
            in  do
                updateMain =<< drawMap objFun matFun mapW rawData visData
                flush
                -}


renderNewMap ∷ WorldMap WorldBase WorldCell → Int → IO ()
renderNewMap m z = C.runCurses do
    newRenderEnvironment >>=
        void . execRenderer do
            drawMap (cellChar . cellAt m . getTopCoord) (const "default") (const Visible) (width m) (height m) >>= updateMain
            --drawMap (cellChar . cellAt m . toV3AtZ . twoDCoord) (const "default") (const Visible) (width m) (height m) >>= updateMain
            flush
    C.defaultWindow >>=
        void . (`C.getEvent` Nothing)
    where
        twoDCoord  = coordLin m
        toV3AtZ v2 = clipToBounds m (V3 (v2 ^. _x) (v2 ^. _y) z)

        getTopCoord = head . column m . indexToCoord m . clipToBounds' m
        cellChar    = maybe ' ' id . preview (wc_contents._Just.o_symbol.s_char)


debugMain ∷ Int → IO ()
debugMain l = do
    dd ← defaultDesignData
    barTilemap ← either (error . show) id <$> loadTileMap "res/bar"
    let worldMap = either (error . show) id $ fromTileMap baseFromTile (objectFromTile dd) barTilemap
    renderNewMap worldMap l
    pure ()

--------------------------------------------------------------------------------
-- END DEBUG


main ∷ IO ()
main =
    let seed = 0
    in  evalRandT defaultDesignData (mkStdGen seed) >>= launchDreamnet
