import Solvable 
import Prelude

-- A very simple example of how to implement the Solvable type class

-- We want to find two (different) numbers whose sum is 5 
data ShouldTotalFive = ShouldTotalFive (Maybe Int, Maybe Int) [Int] deriving (Eq, Show)

instance Solvable ShouldTotalFive where
    -- if both numbers are set and their sum is 5, we have a solution
    isSolution (ShouldTotalFive (Just x, Just y) _) = (x + y) == 5
    -- in all other cases, we don't have solution
    isSolution _                                    = False

    -- if both numbers are set and the sum is not 5, we have reached a dead end
    isDeadEnd (ShouldTotalFive (Just x, Just y) _) = (x + y) /= 5
    -- if one or more numbers are not set, we should continue the search
    isDeadEnd _                                    = False

    -- if no more numbers are available we cannot produce any next states
    nextStates (ShouldTotalFive _                  []) = [] 
    -- if the first number is not yet set, we set that first
    nextStates (ShouldTotalFive (Nothing, _      ) xs) = map (\x -> ShouldTotalFive (Just x, Nothing) (filter (/= x) xs)) xs
    -- if the first number is already set, we set the second one
    nextStates (ShouldTotalFive (Just x , Nothing) xs) = map (\y -> ShouldTotalFive (Just x, Just y ) (filter (/= y) xs)) xs

-- And another implementation using a list instead of a pair
data ShouldAlsoTotalFive = ShouldAlsoTotalFive [Int] [Int] deriving (Eq, Show)

instance Solvable ShouldAlsoTotalFive where
    isSolution (ShouldAlsoTotalFive xs _ ) = (length xs == 2) && (sum xs) == 5

    isDeadEnd (ShouldAlsoTotalFive xs _ ) = (length xs > 2) || (sum xs) > 5

    nextStates (ShouldAlsoTotalFive _  []) = []
    nextStates (ShouldAlsoTotalFive xs ys) = map (\y -> ShouldAlsoTotalFive (y:xs) (filter (/=y) ys)) ys

empty = ShouldTotalFive (Nothing, Nothing) [1,2,3,4,5]
incomplete = ShouldTotalFive (Just 3, Nothing) [1,2,3,4,5]
correct = ShouldTotalFive (Just 3, Just 2) []
incorrect = ShouldTotalFive (Just 3, Just 3) []

