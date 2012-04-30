module Board where

import qualified Data.Map as Map
import qualified Data.List as List (groupBy, intersperse)
import Data.Function (on)

type Position = (Int, Int)

data BoardElement = Cell Position
                  | Edge Position
                  | Crossing Position

data Ball = Ball Position

data Board = Board (Map.Map Position BoardElement) [Ball]

getPosition :: BoardElement -> Position
getPosition (Cell p) = p
getPosition (Edge p) = p
getPosition (Crossing p) = p

instance Show BoardElement where
    show (Cell _) = " "
    show (Edge (x, y)) = if x `mod` 2 == 0
                              then "—"
                              else "│"
    show (Crossing (0, 0)) = "┌"
    show (Crossing (0, _)) = "┬"
    show (Crossing (_, 0)) = "├"
    show (Crossing _) = "┼"

instance Show Board where
    show (Board elements _) = concat . concat . List.intersperse ["\n"] $ map (map show) elementsByLine
                            where
                              elementsWithPosition = Map.toList elements
                              elementsByLineWithPosition = List.groupBy ((==) `on` fst . fst) elementsWithPosition
                              elementsByLine = map (map snd) elementsByLineWithPosition

merge :: [a] -> [a] -> [a]
merge (x:xs) (y:ys) = x:y:(merge xs ys)
merge (x:xs) [] = [x]

mkOuterElement element lineNumber length = [element (lineNumber, x) | x <- [0,2..length]]

mkInnerElement element lineNumber length = [element (lineNumber, x) | x <- [1,3..length]]

mkLine :: Int -> Int -> [BoardElement]
mkLine lineNumber length = merge (mkOuterElement outerElement lineNumber length) (mkInnerElement innerElement lineNumber length)
                         where
                           (outerElement, innerElement) = if lineNumber `mod` 2 == 0
                                                          then (Crossing, Edge)
                                                          else (Edge, Cell)

mkBoard :: Int -> Board
mkBoard side = Board elements []
             where
               elementLines = [mkLine lineNumber (2 * side) | lineNumber <- [0..2*side]]
               allElements = concat elementLines
               elementsWithPosition = map (\x -> (getPosition x, x)) allElements
               elements = Map.fromList elementsWithPosition

addBall :: Board -> Ball -> Board
addBall (Board cells balls) ball = Board cells (ball:balls)

-- char to show open vertical edge :
-- char to show closed vertical edge │
-- char to show open horizontal edge ∙
-- char to show closed horizontal edge —
