module Main where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- The directions of movement
data Dir = North | East | South | West
  deriving (Eq, Show)

-- Where a blizzard is and where it is going
data Blizzard = Blizzard Int Int Dir
  deriving (Show)

-- Convert a character to a blizzard
posToBlizzard :: Int -> (Char, Int) -> Maybe Blizzard
posToBlizzard row ('^',col) = Just $ Blizzard row col North
posToBlizzard row ('>',col) = Just $ Blizzard row col East
posToBlizzard row ('v',col) = Just $ Blizzard row col South
posToBlizzard row ('<',col) = Just $ Blizzard row col West
posToBlizzard _ _ = Nothing

-- Map a row of chars to a list of blizzards
rowToBlizzards :: (Int, String) -> [Blizzard]
rowToBlizzards (rowNum, line) =
  mapMaybe (posToBlizzard rowNum) $ zip line [0..]

-- Moves a blizzard in its direction, wrapping when necessary
-- Since the edges of the map are walls, don't let the
-- row or column be 0 or num-1
moveBlizzard :: Int -> Int -> Blizzard -> Blizzard
moveBlizzard numRows _ (Blizzard row col North) =
  if row == 1 then
    Blizzard (numRows - 2) col North
  else
    Blizzard (row - 1) col North
moveBlizzard _ numCols (Blizzard row col East) =
  if col >= numCols - 2 then
    Blizzard row 1 East
  else
    Blizzard row (col + 1) East
moveBlizzard numRows _ (Blizzard row col South) =
  if row >= numRows - 2 then
    Blizzard 1 col South
  else
    Blizzard (row + 1) col South
moveBlizzard _ numCols (Blizzard row col West) =
  if col == 1 then
    Blizzard row (numCols - 2) West
  else
    Blizzard row (col - 1) West

-- Move all the blizzards in their directions
moveBlizzards :: Int -> Int -> [Blizzard] -> [Blizzard]
moveBlizzards numRows numCols = map (moveBlizzard numRows numCols)

-- Convert the blizzards into a set of coordinates indicating
-- where the blizzards are
makeBlizzardGrid :: [Blizzard] -> Set.Set (Int,Int)
makeBlizzardGrid blizzards =
  Set.fromList $ map blizzardToRC blizzards
  where
    blizzardToRC (Blizzard row col _) = (row,col)

-- Add a location to the set of possible presences, as long as the
-- row and column are allowable and there isn't a blizzard in
-- the location
addPresence :: Int -> Int -> Set.Set (Int,Int) -> (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
addPresence numRows numCols blizzGrid rc@(row,col) presence =
  if row > 0 && row < numRows - 1 && col > 0 && col < numCols - 1 &&
    not (Set.member rc blizzGrid) then
    Set.insert rc presence
  else
    presence

-- Given a presence at a particular row,col, try adding the presence
-- and its adjacent squares to the new set of presences
tryPresence :: Int -> Int -> Set.Set (Int,Int) -> Set.Set (Int,Int) -> (Int,Int) -> Set.Set (Int,Int)
tryPresence numRows numCols blizzGrid presence (row,col) =
  let wait = addPresence numRows numCols blizzGrid (row,col) presence in
  let north = addPresence numRows numCols blizzGrid (row-1,col) wait in
  let east = addPresence numRows numCols blizzGrid (row,col+1) north in
  let south = addPresence numRows numCols blizzGrid (row+1,col) east in
  addPresence numRows numCols blizzGrid (row,col-1) south

-- Execute a turn, and if we end up at end row,col, return the turn
-- number and the blizzard positions
doTurn :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Blizzard] -> Set.Set (Int,Int) -> (Int,[Blizzard])
doTurn numRows numCols startRow startCol endRow endCol turn blizzards oldPresence =
  if Set.member (endRow, endCol) nextPresence then
    (turn+1, nextBlizzards)
  else
    doTurn numRows numCols startRow startCol endRow endCol (turn+1) nextBlizzards nextPresence
  where
  -- Compute the next set of presences
    nextPresence1 = foldl' (tryPresence numRows numCols blizzGrid) Set.empty (Set.toList oldPresence)
  -- We could always have waited all this time to first enter
  -- the grid, so add the start position if there is no blizzard there
    nextPresence = if not (Set.member (startRow,startCol) blizzGrid) then
                     Set.insert (startRow, startCol) nextPresence1
                   else
                     nextPresence1
  -- Compute the set of blizzard positions
    blizzGrid = makeBlizzardGrid blizzards
  -- Compute the blizzard positions for the next turn
    nextBlizzards = moveBlizzards numRows numCols blizzards

main :: IO ()
main = do
  fileData <- readFile "data/day24.txt"
--  fileData <- readFile "test.txt"
  let fileLines = lines fileData
  let startCol = fromJust $ elemIndex '.' $ head fileLines
  let endCol = fromJust $ elemIndex '.' $ last fileLines

  let blizzards = concatMap rowToBlizzards $ zip [0..] fileLines
  let numRows = length fileLines
  let numCols = length $ head fileLines

  let (partA,blizzA) = doTurn numRows numCols 1 startCol (numRows - 2) endCol 0 blizzards Set.empty
  putStr "Part A path length "
  print partA
  let (goBack,blizzBack) = doTurn numRows numCols (numRows - 2) endCol 1 startCol partA blizzA Set.empty
  let (partB,_) = doTurn numRows numCols 1 startCol (numRows - 2) endCol goBack blizzBack Set.empty
  putStr "Part B path length "
  print partB


