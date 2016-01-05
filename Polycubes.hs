{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fst, snd)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing, Down(..))
import Solvable

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Control.Monad (forM_)

type Position3D = (Int, Int, Int)

type Space3D = (Int, Int, Int)

type Shape = [Position3D]

data PolycubePuzzle = PolycubePuzzle {
                    space :: Space3D,
                    placedShapes :: [Shape],
                    availableShapes ::  [Shape]
                } deriving (Show)

-- we will need matrices to perform the rotations on our Shapes

-- all matrices can be transposed
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose lists = (map head lists) : transpose (map tail lists)

-- numeric matrices can also be multiplied
mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult a b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]

-- to enable some inspection we will wrap all transformation matrices in a new type
-- that also contains a name for the transformation
data Transformation = Transformation String [[Int]] deriving (Show)

-- however, for equality checks, we will ignore the name
instance Eq Transformation where
    (==) (Transformation _ matrix1) (Transformation _ matrix2) = matrix1 == matrix2

-- to concatenate two Transformations to a new transformation we multiply their matrices
-- and concatenate the name (so we'll later know how we arrived at this transformation)
concatTrans :: Transformation -> Transformation -> Transformation
concatTrans (Transformation name1 matrix1) (Transformation name2 matrix2) = Transformation (name1 ++ name2) (mult matrix1 matrix2)

identity3D :: Transformation
identity3D = Transformation "" [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

--  (x,y,z) -> (-y,x,z) 
turn3D :: Transformation
turn3D = Transformation "r" [[0, -1, 0], [1, 0, 0], [0, 0, 1]]

--  (x,y,z) -> (x,z,-y)
roll3D :: Transformation
roll3D = Transformation "t" [[1, 0, 0], [0, 0, 1], [0, -1, 0]]

-- we will need a way to create all rotations of a Shape
-- There 24 orthogonal rotations of an object in 3D space which can be seen
-- by thinking of a simple die:
--  * each of the faces with the numbers 1-6 can be on top
--  * with each face on top there are four rotations of the die
-- according to this answer http://stackoverflow.com/a/16453299/763935 these 24
-- rotation matrices can be produces like this:
allRotations :: [Transformation]
allRotations = nub [foldl concatTrans identity3D ((take p $ repeat roll3D) ++ (take q $ repeat turn3D) ++ (take r $ repeat roll3D) ++ (take s $ repeat turn3D)) | let pots = [0 .. 3], p <- pots, q <- pots, r <- pots, s <- pots]

-- I cannot prove easily prove that this function will indeed produce all the rotations but:
--  * since the function uses only the valid rotations "turn" and "roll" all
--    produced Transformations will also be valid rotations (and not, for example,
--    mirror transformations)
--  * since allRotations produces 24 distinct rotation matrices it seems that we do indeed
--    create ALL possible rotations

applyToPosition3D :: Transformation -> Position3D -> Position3D
applyToPosition3D (Transformation _ matrix) (x, y, z) = toTriple result
    where toTriple [x:_, y:_, z:_] = (x, y, z)
          result = mult matrix [[x], [y], [z]]

applyToShape :: Transformation -> Shape -> Shape
applyToShape transformation shape = normalizeShape $ map (applyToPosition3D transformation) shape

-- after a rotation a Shape may need to normalized, i. e. translated so that one corner
-- of the bounding box of the Shape has the coordinates (0, 0, 0)
-- note that this does not mean, that the coordinates (0, 0, 0) must be included in the Shape
normalizeShape :: Shape -> Shape
normalizeShape shape = translateShape (-offZeroX, -offZeroY, -offZeroZ) shape
              where offZeroX = minimum $ map fst shape
                    offZeroY = minimum $ map snd shape
                    offZeroZ = minimum $ map trd shape

translateShape :: Position3D -> Shape -> Shape
translateShape (dx, dy, dz) = map (\(x, y, z) -> (x + dx, y + dy, z + dz))

allRotationsOfShape :: Shape -> [Shape]
allRotationsOfShape shape = nub $ map sort $ map (\rot -> applyToShape rot shape) allRotations

allPositionsBetween :: Position3D -> Position3D -> [Position3D]
allPositionsBetween (x1, y1, z1) (x2, y2, z2) = [(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]

allPositionsTo :: Position3D -> [Position3D]
allPositionsTo = allPositionsBetween (0, 0, 0)

fst (x, _, _) = x
snd (_, y, _) = y
trd (_, _, z) = z

maximumTranslation :: Space3D -> Shape -> Position3D
maximumTranslation (spaceX, spaceY, spaceZ) shape = (spaceX - sizeX, spaceY - sizeY, spaceZ - sizeZ)
        where sizeX = maximum (map fst shape) + 1
              sizeY = maximum (map snd shape) + 1
              sizeZ = maximum (map trd shape) + 1

allPlacementsWithin :: Space3D -> Shape -> [Shape]
allPlacementsWithin space shape = concatMap calculatePlacements rotations
        where calculatePlacements s = map (flip translateShape s) (possibleTranslations s)
              possibleTranslations = allPositionsTo . maximumTranslation space
              rotations = allRotationsOfShape shape

placeOneMore :: PolycubePuzzle -> [PolycubePuzzle]
-- if there are no unplaced shapes, no further states can be constructed from this one
placeOneMore sol@(PolycubePuzzle space placedShapes []) = [] 
placeOneMore sol@(PolycubePuzzle space placedShapes (nextShape:unplacedShapes)) = do
              let 
                newPlacements :: [Shape] 
                newPlacements = allPlacementsWithin space nextShape
                filterFunc :: Shape -> Bool
                filterFunc s = not $ any (collide s) placedShapes
                -- filterFunc = const True
                filteredPlacements = filter filterFunc newPlacements
              newPlacement <- filteredPlacements 
              return $ PolycubePuzzle space (newPlacement:placedShapes) unplacedShapes

collide :: (Eq a) => [a] -> [a] -> Bool
collide xs ys = any (\x -> x `elem` ys) xs

-- Note that isDeadEnd to always be False is risky: If nextStates
-- does not return empty lists at some point during the search the
-- search will never come to an end. However, since each tiles will
-- be removed from the second list
instance Solvable PolycubePuzzle where
    isSolution (PolycubePuzzle _ _ []) = True
    isSolution (PolycubePuzzle _ _ _) = False
    isDeadEnd _ = False
    nextStates = placeOneMore


-- Soma shapes
somaV = [(0, 0, 0), (0, 1, 0), (1, 0, 0)]

somaL = [(0, 0, 0), (0, 1, 0), (0, 2, 0), (1, 0, 0)]

somaT = [(0, 0, 0), (0, 1, 0), (0, 2, 0), (1, 1, 0)]

somaZ = [(0, 0, 0), (0, 1, 0), (1, 1, 0), (1, 2, 0)]

somaA = [(0, 0, 0), (0, 1, 0), (0, 1, 1), (1, 1, 1)]

somaB = [(0, 0, 0), (0, 1, 0), (1, 1, 0), (1, 1, 1)]

somaP = [(0, 0, 0), (0, 0, 1), (0, 1, 0), (1, 0, 0)]

somaCubePuzzle = PolycubePuzzle (3, 3, 3) [] [somaV, somaL, somaT, somaZ, somaA, somaB, somaP]

-- Cochise shapes:

p1 = [(1,3,3),(1,4,0),(1,4,1),(1,4,2),(1,4,3),(2,3,3),(2,4,0),(3,3,3),(3,4,0),(3,4,3),(4,1,0),(4,1,1),(4,2,0),(4,3,0),(4,4,0)]

p2 = [(2,0,0),(2,1,0),(2,2,0),(2,3,0),(2,3,1),(2,4,1),(3,0,0),(3,4,1),(4,0,0),(4,2,1),(4,3,1),(4,4,1)]

p3 = [(0,0,0),(0,1,0),(0,2,0),(0,3,0),(0,4,0),(1,0,0),(1,0,1),(2,0,1),(2,1,1),(3,1,0),(3,1,1),(3,2,0),(3,3,0),(3,3,1)]

p4 = [(1,1,0),(1,1,1),(1,1,2),(1,2,0),(2,1,2),(2,4,2),(2,4,3),(3,1,2),(3,4,2),(4,1,2),(4,2,2),(4,3,2),(4,4,2)]

p5 = [(0,0,1),(0,0,2),(0,1,1),(0,2,1),(1,0,2),(1,2,1),(2,0,2),(2,2,1),(3,0,2),(3,0,3),(3,0,4),(3,1,4),(3,2,1),(3,2,2)]

p6 = [(0,1,2),(0,2,2),(1,2,2),(1,4,4),(2,1,3),(2,2,2),(2,2,3),(2,4,4),(3,1,3),(3,4,4),(4,1,3),(4,2,3),(4,3,3),(4,4,3),(4,4,4)]

p7 = [(1,0,3),(1,1,3),(1,2,3),(1,2,4),(1,3,4),(2,0,3),(2,3,4),(3,0,1),(3,3,4),(4,0,1),(4,0,2),(4,0,3),(4,0,4),(4,1,4),(4,2,4),(4,3,4)]

p8 = [(0,1,4),(0,2,4),(0,3,1),(0,3,4),(0,4,1),(0,4,2),(0,4,3),(0,4,4),(1,1,4),(1,3,0),(1,3,1),(2,1,4),(2,2,4),(3,2,3),(3,2,4)]

p9 = [(0,0,3),(0,0,4),(0,1,3),(0,2,3),(0,3,2),(0,3,3),(1,0,4),(1,3,2),(2,0,4),(2,3,2),(3,3,2)]

cochiseCubeShapes = [p1, p2, p3, p4, p5, p6, p7, p8, p9]
cochiseCubeShapesSortedAsc = sortBy (comparing length) [p1, p2, p3, p4, p5, p6, p7, p8, p9]
cochiseCubeShapesSortedDesc = sortBy (comparing (Down . length)) [p1, p2, p3, p4, p5, p6, p7, p8, p9]

cochiseCubePuzzle = PolycubePuzzle (5, 5, 5) [] cochiseCubeShapesSortedDesc

main :: IO ()
main = do
    let solution = show $ head $ solve cochiseCubePuzzle
    putStrLn solution
