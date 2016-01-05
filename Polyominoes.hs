{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.List (nub, sort)
import Solvable

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Control.Monad (forM_)

type Position2D = (Int, Int)

type Space2D = (Int, Int)

type Shape = [Position2D]

-- these are the twelve pentominoes named as described at:
-- https://en.wikipedia.org/wiki/Pentomino
f5  = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 2)]
f5' = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 1)]

i5  = [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]

l5  = [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0)]
l5' = [(0, 0), (1, 0), (1, 1), (1, 2), (1, 3)]

n5  = [(0, 0), (0, 1), (1, 1), (1, 2), (1, 3)]
n5' = [(0, 1), (0, 2), (0, 3), (1, 0), (1, 1)]

p5  = [(0, 0), (0, 1), (0, 2), (1, 1), (1, 2)]
p5' = [(0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]

t5  = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2)]

u5  = [(0, 0), (0, 1), (1, 0), (2, 0), (2, 1)]

v5  = [(0, 0), (0, 1), (0, 2), (1, 0), (2, 0)]

w5  = [(0, 1), (0, 2), (1, 1), (1, 0), (2, 0)]

x5  = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]

y5  = [(0, 2), (1, 0), (1, 1), (1, 2), (1, 3)]
y5' = [(0, 0), (0, 1), (0, 2), (0, 3), (1, 2)]

z5  = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 0)]
z5' = [(0, 0), (1, 0), (1, 1), (1, 2), (2, 2)]

data PolyominoPuzzle = PolyominoPuzzle {
                    space :: Space2D,
                    placedShapes :: [Shape],
                    availableShapes ::  [Shape]
                } deriving (Show)

rotateShape90 :: Shape -> Shape
rotateShape90 = normalizeShape . map rotatePosition90

rotateShape180 :: Shape -> Shape
rotateShape180 = normalizeShape . map rotatePosition180

rotateShape270 :: Shape -> Shape
rotateShape270 = normalizeShape . map rotatePosition270

rotatePosition90 :: Position2D -> Position2D
rotatePosition90 (x, y) = (-y, x)

rotatePosition180 :: Position2D -> Position2D
rotatePosition180 = rotatePosition90 . rotatePosition90

rotatePosition270 :: Position2D -> Position2D
rotatePosition270 = rotatePosition90 . rotatePosition90 . rotatePosition90

normalizeShape :: Shape -> Shape
normalizeShape shape = translateShape (-offZeroX, -offZeroY) shape
              where offZeroX = minimum $ map fst shape
                    offZeroY = minimum $ map snd shape

translateShape :: Position2D -> Shape -> Shape
translateShape (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

allRotations :: Shape -> [Shape]
allRotations shape = nub $ map sort $ map ($ shape) [id, rotateShape90, rotateShape180, rotateShape270]  

allPositionsBetween :: Position2D -> Position2D -> [Position2D]
allPositionsBetween (x1, y1) (x2, y2) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

allPosTo :: Position2D -> [Position2D]
allPosTo = allPositionsBetween (0, 0)

maximumTranslation :: Space2D -> Shape -> (Position2D)
maximumTranslation (spaceX, spaceY) shape = (spaceX - sizeX, spaceY - sizeY)
        where sizeX = maximum (map fst shape) + 1
              sizeY = maximum (map snd shape) + 1

allPlacementsWithin :: Space2D -> Shape -> [Shape]
allPlacementsWithin (spaceX, spaceY) shape = concatMap calculatePlacements rotations
        where calculatePlacements s = map (flip translateShape s) (possibleTranslations s)
              possibleTranslations = allPosTo . maximumTranslation (spaceX, spaceY)
              rotations = allRotations shape

placeOneMore :: PolyominoPuzzle -> [PolyominoPuzzle]
placeOneMore sol@(PolyominoPuzzle space placedShapes []) = [] -- what is correct here?
placeOneMore sol@(PolyominoPuzzle space placedShapes (nextShape:moreShapesForLater)) = do
              let 
                newPlacements :: [Shape] -- [[Position2D]]
                newPlacements = allPlacementsWithin space nextShape
                filterFunc :: Shape -> Bool
                filterFunc s = not $ any (collide s) placedShapes
                -- filterFunc = const True
                filteredPlacements = filter filterFunc newPlacements
              newPlacement <- filteredPlacements 
              return $ PolyominoPuzzle space (newPlacement:placedShapes) moreShapesForLater

collide :: (Eq a) => [a] -> [a] -> Bool
collide xs ys = any (\x -> x `elem` ys) xs

-- Note that isDeadEnd to always be False is risky: If nextStates
-- does not return empty lists at some point during the search the
-- search will never come to an end. However, since each tiles will
-- be removed from the second list
instance Solvable PolyominoPuzzle where
    isSolution (PolyominoPuzzle _ _ []) = True
    isSolution (PolyominoPuzzle _ _ _) = False
    isDeadEnd _ = False
    nextStates = placeOneMore

pentomino0 = PolyominoPuzzle (6, 10) [] [i5, t5, p5]
pentomino1 = PolyominoPuzzle (6, 10) [] [i5, t5, p5, w5, x5, y5, f5, z5, u5, v5, l5', n5]
pentomino2 = PolyominoPuzzle (3, 20) [] [i5, t5, p5, w5, x5, y5, f5, z5, u5, v5, l5', n5]

pentomino3x5v1  = PolyominoPuzzle (3,  5) [] [p5, f5, u5]
pentomino3x10v1 = PolyominoPuzzle (3, 10) [] [u5, f5, i5, v5, p5, y5]
pentomino5x5v1  = PolyominoPuzzle (5,  5) [] [y5, f5', l5, i5, t5]
pentomino5x6v1  = PolyominoPuzzle (5,  6) [] [f5', l5', p5', v5, t5, i5]
pentomino5x7v1  = PolyominoPuzzle (5,  7) [] [f5', i5, n5', y5', u5, p5, l5]
pentomino5x8v1  = PolyominoPuzzle (5,  8) [] [l5, f5', t5, x5, v5, i5, z5', p5]

colors :: [String]
colors = ["#" ++ r ++ g ++ b | let values = ["00", "7f", "ff"], r <- values, g <- values, b <- values]

renderPolyominoPuzzle :: [String] -> PolyominoPuzzle -> S.Svg
renderPolyominoPuzzle colors (PolyominoPuzzle (w, h) shapes _) = S.docTypeSvg ! A.version "1.1" ! A.width (S.toValue $ w * 50) ! A.height (S.toValue $ h * 50)  ! A.viewbox (S.toValue viewBoxString) $ do 
        let shapesAndColors= zip shapes colors
        forM_ shapesAndColors renderShapeAndColor
        where viewBoxString :: String
              viewBoxString = "0 0 " ++ (show w) ++ " " ++ (show h)

renderShapeAndColor :: (Shape, String) -> S.Svg
renderShapeAndColor (ps, color) = do 
    forM_ ps (\p -> translateSvg p (aBox ! A.fill (S.toValue color)))

renderShape :: Shape -> S.Svg
renderShape ps = do 
    forM_ ps (\p -> translateSvg p aBox)

aBox :: S.Svg
aBox = S.rect ! A.width "1" ! A.height "1" 

translateSvg :: Position2D -> S.Svg -> S.Svg
translateSvg (x, y) svg = svg ! A.x (S.toValue x) ! A.y (S.toValue y)

main :: IO ()
main = do
  let solution = head $ solve pentomino5x6v1
  let svg = renderSvg $ renderPolyominoPuzzle colors solution
  putStrLn svg
