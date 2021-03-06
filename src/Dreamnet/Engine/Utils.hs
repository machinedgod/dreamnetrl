{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE NegativeLiterals #-}

module Dreamnet.Engine.Utils
( --line
  bla, circle, floodFillRange

, fmt

, interpolateMonoid, interpolateEnum, interpolateSucc, interpolatePred

, maybeToError
) where


import Prelude             hiding (succ, pred)
import Safe                       (succSafe, predSafe)
import Control.Lens               (use, (%=), _1, _2)
import Control.Applicative        (Alternative ((<|>), empty))
import Control.Monad              (unless)
import Control.Monad.Except       (MonadError(..))
import Control.Monad.State.Strict (State, execState)
import Data.List                  (unfoldr)
import Data.Foldable              (foldl')
import Linear                     (V2(V2), distance)

import qualified Data.Set as S (Set, empty, singleton, fromList, toList, insert,
                                filter, member, union, null, map)

--------------------------------------------------------------------------------

-- TODO BUGGY!
{-
line ∷ V2 Int → V2 Int → [V2 Int]
line v1 v2 = let v1f       = fmap fromIntegral v1 ∷ V2 Float
                 v2f       = fmap fromIntegral v2 ∷ V2 Float
                 vd        = normalize (v2f - v1f)
                 step      = 0.5
                 steps     = ceiling (distance v1f v2f / step) ∷ Int
                 toCoord d = floor <$> vd ^* d
                 enough [] = []
                 enough l  = let !h = takeWhile (/=v2) l
                                 !t = dropWhile (/=v2) l
                             in  h ++ bool [] [head t] (not $ null t)
                 uniq []   = []
                 uniq xs   = foldr' (\v l → if null l || head l /= v
                                              then v : l
                                              else l) [] xs
             in  uniq $ enough $ fmap (v1^+^) $ toCoord <$> [0, step..fromIntegral steps]
-}


-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
-- Modified to stop at the second point
bla ∷ V2 Int -> V2 Int -> [V2 Int]
bla (V2 x0 y0) d@(V2 x1 y1) =
    let (V2 dx dy) = V2 (x1 - x0) (y1 - y0)
        xyStep b (V2 x y) = V2 (x + signum dx)     (y + signum dy * b)
        yxStep b (V2 x y) = V2 (x + signum dx * b) (y + signum dy)
        (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                     | otherwise       = (abs dx, abs dy, yxStep)
        walk w xy | xy == d   = [xy]
                  | otherwise = xy : walk (tail w) (step (head w) xy)
    in  walk (balancedWord p q 0) (V2 x0 y0)
    where
        balancedWord ∷ Int -> Int -> Int -> [Int]
        balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
        balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)


-- Takes the center of the circle and radius, and returns the circle points
circle ∷ Int → V2 Int → [V2 Int]
circle radius (V2 x0 y0) = uncurry V2 <$> iniPoints
    where
        -- Four initial points, plus the generated points
        iniPoints = (x0, y0 + radius) : (x0, y0 - radius) : (x0 + radius, y0) : (x0 - radius, y0) : points
        -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
        points = concatMap generatePoints $ unfoldr step initialValues
        generatePoints (x, y)
          = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
 
        -- The initial values for the loop
        initialValues = (1 - radius, 1, (-2) * radius, 0, radius)
 
        -- One step of the loop. The loop itself stops at Nothing.
        step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                     | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                       where
                                           (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                            | otherwise = (f + ddf_x, ddf_y, y)
                                           ddf_x' = ddf_x + 2
                                           x' = x + 1


-- TODO use ST monad
floodFillRange ∷ Word → V2 Int → [V2 Int]
floodFillRange r o = S.toList $ snd $ execState nearestNeighbor (S.singleton o, S.empty)
    where
        nearestNeighbor ∷ State (S.Set (V2 Int), S.Set (V2 Int)) ()
        nearestNeighbor = do
            openSet   ← use _1
            closedSet ← use _2

            mapM_ (\x → do
                        _2 %= S.insert x
                        _1 %= S.filter ((&&) . inRange r <*> not . (`S.member` closedSet)) . S.union (neighbors x)
                ) openSet
            unless (S.null openSet)
                nearestNeighbor

        inRange ∷ Word → V2 Int → Bool
        inRange d x = let tfv = fmap fromIntegral
                      in  abs (distance (tfv o) (tfv x) ∷ Float) < fromIntegral d

        neighbors ∷ V2 Int → S.Set (V2 Int)
        neighbors p = S.map (+p) $ S.fromList [ V2 -1 -1
                                              , V2  0 -1
                                              , V2  1 -1
                                              , V2 -1  0
                                              , V2  1  0
                                              , V2 -1  1
                                              , V2  0  1
                                              , V2  1  1
                                              ]

--------------------------------------------------------------------------------

fmt ∷ (Ord b, Eq a, Monoid a, Foldable t) ⇒ b → (a → b) → a → t a → [a]
fmt l lf sep xs =
    let (ln, r) = foldl' foo (mempty, empty) xs
    in  if r == [] && ln == mempty
          then error "Phrase longer than limit (cannot be separated by separator!)"
          else if r == []
            then pure ln
            else pure ln <|> fmt l lf sep r
    where
        foo (f, b) x
            | b /= mempty                           = (f, b <> pure x)
            | b == mempty && lf (f <> sep <> x) < l = (f <> sep <> x, b)
            | otherwise                             = (f, pure x)

--------------------------------------------------------------------------------

-- TODO allow interpolation even when value set is smaller than index set
interpolateMonoid ∷ (Monoid a, Monoid b, Ord b) ⇒ a → a → b → b → a
interpolateMonoid start step i iend = start <> recurseSteps mempty
    where
        recurseSteps c
            | c >= iend = mempty
            | otherwise = step <> recurseSteps (c <> i)


interpolateEnum ∷ (Enum a) ⇒ a → a → Float → a
interpolateEnum x n t =
    let xs = enumFromThen x n
    in  xs !! floor (t * fromIntegral (length xs))
{-# INLINE interpolateEnum #-}


interpolateSucc ∷ (Enum a, Eq a, Bounded a) ⇒ a → Float → a
interpolateSucc x = interpolateEnum x (succSafe x)
{-# INLINE interpolateSucc #-}


interpolatePred ∷ (Enum a, Eq a, Bounded a) ⇒ a → Float → a
interpolatePred x = interpolateEnum x (predSafe x)
{-# INLINE interpolatePred #-}

--------------------------------------------------------------------------------

{-
maybeToEither ∷ a → Maybe b → Either a b
maybeToEither d Nothing  = Left d
maybeToEither _ (Just x) = Right x
-}


maybeToError ∷ (MonadError a me) => a → Maybe b → me b
maybeToError d Nothing  = throwError d
maybeToError _ (Just x) = pure x
{-# INLINE maybeToError #-}
