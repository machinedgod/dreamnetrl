{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Object Monad defines objects as composition of design primitives,
--   which are then in turn interpreted through WorldAPI language

module Dreamnet.ObjectMonad
where


import Control.Monad.Free
import Linear (V2(V2))

import Dreamnet.GameState
import Dreamnet.World
import Dreamnet.WorldMap
import Dreamnet.Visibility

--------------------------------------------------------------------------------

data ObjectF a = Move (V2 Int) a
               | GetPosition (V2 Int → a)
               | RequestGameState GameState a
               deriving(Functor)


move ∷ V2 Int → Free ObjectF ()
move v = Free $ Move v (Pure ())


getPosition ∷ Free ObjectF (V2 Int)
getPosition = Free $ GetPosition $ \v → Pure v


requestGameState ∷ GameState → Free ObjectF ()
requestGameState gs =  Free $ RequestGameState gs (Pure ())

--------------------------------------------------------------------------------

runObjectMonadIO ∷ V2 Int → Free ObjectF a → IO a
runObjectMonadIO cv (Free (Move v n)) = do
    putStrLn $ "Moving to " ++ show v
    runObjectMonadIO v n
runObjectMonadIO cv (Free (GetPosition fv)) = do
    putStrLn $ "Current position is " ++ show cv
    runObjectMonadIO cv (fv cv)
runObjectMonadIO cv (Free (RequestGameState gs n)) = do
    putStrLn $ "Game state request switched to: " ++ show gs
    runObjectMonadIO cv n
runObjectMonadIO cv (Pure x) = pure x


runObjectMonadWorld ∷ (Monad w, WorldAPI a b c w) ⇒ V2 Int → a → Free ObjectF d → w (d, GameState)
runObjectMonadWorld = runWithGameState Normal
    where
        runWithGameState gs cv o (Free (Move v n)) = do
            setStatus $ "Moving to " ++ show v
            moveObject cv o v
            runWithGameState gs v o n
        runWithGameState gs cv o (Free (GetPosition fv)) = do
            setStatus $ "Current position is " ++ show cv
            runWithGameState gs cv o (fv cv)
        runWithGameState _ cv o (Free (RequestGameState gs n)) = do
            setStatus $ "Requesting game state: " ++ show gs
            runWithGameState gs cv o n
        runWithGameState gs _ _ (Pure x) = pure (x, gs)



runWorldTest ∷ (GameState, World String Visibility ())
runWorldTest = runWorld (runObjectMonadWorld (V2 0 0) "@" littleProgram >>= pure . snd) world
    where
        world ∷ World String Visibility ()
        world = newWorld map []
        map ∷ WorldMap String Visibility
        map = newWorldMap 10 10
        littleProgram = do
            move (V2 2 2)
            p ← getPosition
            move $ (V2 2 2) + p
            --requestGameState Interaction
