module Board where

import qualified Data.Map as Map
import qualified Data.List as List (intersperse, sortBy, groupBy)

type Position = (Int, Int)

data BallPosition = MidCell Position
                  | OverEdge Position Position

data Ball = Ball BallPosition

data Cell = Cell { leftEdge :: Bool
                 , rightEdge :: Bool
                 , topEdge :: Bool
                 , bottomEdge :: Bool
                 }

data Board = Board (Map.Map Position Cell) [Ball]

mkBoard :: Int -> Board
mkBoard side = Board cells []
             where
               mkCellAt x y = Cell (x == 1) (x == side) (y == 1) (y == side)
               cells = Map.fromList [((x, y), mkCellAt x y) | x <- [1..side], y <- [1..side]]

-- TODO make it work with edges with balls
showHorizontalEdge :: (Cell -> Bool) -> Cell -> String
showHorizontalEdge edge cell = if edge cell
                               then "———"
                               else "∙∙∙"

showTopEdges :: [Cell] -> String
showTopEdges = showEdgesLine topEdge

showLastLine :: [Cell] -> String
showLastLine = showEdgesLine bottomEdge

showEdgesLine :: (Cell -> Bool) -> [Cell] -> String
showEdgesLine edge cells = margin ++ edges ++ margin
                         where
                           margin = " "
                           edges = concat . List.intersperse " " . map (showHorizontalEdge edge) $ cells

showVerticalEdge :: (Cell -> Bool) -> Cell -> Char
showVerticalEdge edge cell = if edge cell
                             then '│'
                             else ':'

showFirstEdgeOfLine :: [Cell] -> Char
showFirstEdgeOfLine = showVerticalEdge leftEdge . head

showCell :: Cell -> String
showCell cell = "   " ++ [showVerticalEdge rightEdge cell]

-- TODO make it show a ball in the cell
showMiddleOfCell :: Cell -> String
showMiddleOfCell cell = "   " ++ [showVerticalEdge rightEdge cell]

showCells :: [Cell] -> String
showCells = showCellsUsing showCell

showMiddleOfCells :: [Cell] -> String
showMiddleOfCells = showCellsUsing showMiddleOfCell

showCellsUsing :: (Cell -> String) -> [Cell] -> String
showCellsUsing showFunction cells = firstEdge : allButFirstEdge
                where
                  firstEdge = showFirstEdgeOfLine cells
                  allButFirstEdge = foldr (++) "" $ map showFunction cells

boardLines :: Board -> [[Cell]]
boardLines (Board cellMap _) =
    let
        keys = Map.keys cellMap
        sortBySnd pairA pairB = if snd pairA == snd pairB
                                then fst pairA `compare` fst pairB
                                else snd pairA `compare` snd pairB
        keysByLine = List.groupBy (\a b -> snd a == snd b) $ List.sortBy sortBySnd keys
        unwrapMaybeCell (Just cell) = cell
        mapBack = unwrapMaybeCell . (flip Map.lookup) cellMap -- guaranteed because we are iterating on the keys
    in
        map (map mapBack) keysByLine

instance Show Board where
    show board =
        let
            lines = boardLines board
            showLine line = showTopEdges line ++ "\n" ++ showCells line ++ "\n" ++ showMiddleOfCells line ++ "\n" ++ showCells line
            allButLastLine = unlines $ map showLine lines
        in
            allButLastLine ++ showLastLine (last lines)

-- char to show open vertical edge :
-- char to show closed vertical edge │
-- char to show open horizontal edge ∙
-- char to show closed horizontal edge —
