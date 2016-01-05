module Solvable (Solvable (..), solve, solveWithTrace) where

import Prelude
import Debug.Trace

solveWithTrace :: Solvable a => a -> [a]
solveWithTrace = solve' True

solve :: Solvable a => a -> [a]
solve = solve' False

-- Note that this leaves two places to stop exploration of a path
-- in the search tree:
--  * if isDeadEnd returns True
--  * if nextStates returns []
-- This means that implementations can choose to rely solely on
-- isDeadEnd to eliminate invalid states and therefore abort the
-- search OR can limit the search by only creating valid states
-- in nextStates (OR a combination of both).
solve' :: Solvable a => Bool -> a -> [a]
solve' showTrace a = if showTrace
                    then trace ("checking: " ++ (show a)) $ ret
                    else ret
    where ret | isSolution a = [a]
              | isDeadEnd a = []
              | otherwise = concatMap (solve' showTrace) (nextStates a)
             
-- we want to be able to trace the execution of the algorithm
-- so Solvable must be a Subclass of Show
class (Show a) => Solvable a where
    isSolution :: a -> Bool
    isDeadEnd :: a -> Bool
    nextStates :: a -> [a]

