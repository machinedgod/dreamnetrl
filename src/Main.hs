{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BlockArguments #-}

module Main
where

import Control.Monad.Random  (evalRandT, mkStdGen)
import Dreamnet.Dreamnet

-- DEBUG

import Control.Lens
import Control.Monad
import qualified Data.Vector as V
import Dreamnet.Rendering.Renderer
import Dreamnet.Engine.Visibility
import Dreamnet.Engine.TileMap
import UI.NCurses


renderTilemap ∷ TileMap → IO ()
renderTilemap tm = void $ runCurses do
    void $ execRenderer renderAc =<< newRenderEnvironment
    flip getEvent Nothing =<< defaultWindow
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
-- END DEBUG

main ∷ IO ()
main = 
    let seed = 0
    in  evalRandT defaultDesignData (mkStdGen seed) >>= launchDreamnet
