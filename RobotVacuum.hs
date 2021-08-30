{-
  A robot vacuum cleaner that avoids obstacles and can only
  turn right.
  
  Given a matrix of tiles, return the number of unique tiles
  cleaned by the vacuum cleaner, assuming it starts at the top
  left corner. Obstacles are denoted by 'X' and available tiles
  are denoted by '.'
  
  Author:   Ninad Jog
  Updated:  August 30, 2021
  
  * The test cases shown here are representative, not exhaustive.
  
  * A more efficient implementation would use arrays instead of
    lists of lists to represent the grid of tiles.
    
  * A solution using State Monads is possible but is not shown
    here.
-}

module RobotVacuum where
  
import Prelude hiding (Left, Right)

import Data.Map (Map)
import qualified Data.Map as Map

------------------------------------------------------------
{- 
Directions in which the robot vacuum cleaner can turn in
a clockwise manner, starting from the Right.

None is also included because it appears when the cleaner
cannot turn in any direction because all around it are
either boundaries or the adjacent cells are blocked.

We hide Left and Right while importing the Prelude because
Left and Right belong to the Either data type. They clash
with Heading's Left and Right because Haskell uses a
global namespace.
-}

data Heading = Right | Down | Left | Up | None
  deriving (Show, Eq)

-- Turn clockwise by 90 degrees
turnRight :: Heading -> Heading
turnRight cur =
  case cur of
    Right -> Down
    Down  -> Left
    Left  -> Up
    Up    -> Right
    None  -> None
  
------------------------------------------------------------
{-
  A cell is the address of a tile. It is Cell x y, the same
  as Cell col row.
-}

data Cell = Cell Int Int
  deriving (Show, Ord, Eq)
  
------------------------------------------------------------
{-
This is the solution. Given a matrix of floor tiles, with
the vacant ones marked by a '.' and the obstacles marked
by 'X', this function returns the number of unique tiles 
cleaned by the robot vacuum cleaner starting at the top 
left tile and going right.

When the robot comes across an obstacle or a boundary, it
turns right (turns by 90 degrees). If there is still an
obstacle or a boundary, it turns right again.

This function assumes that the top left tile is always
vacant. It also assumes that the floor is rectangular;
i.e. it's an m x n matrix.

Based on the rules of turning right, the vacuum cleaner
could continue in an infinite loop. But this function
aborts as soon as it counts the number of unique tiles 
that the robot cleaner has visited.

Implementation: Place each tile in a hash map, with the
tile's coordinates as the key and the next heading as
the value. If such an entry already exists in the hash
map, it means that the vacuum cleaner has completed a
loop, and further visits will not be to any new tiles.
Therefore, return the size of the hash map, as it will be
the number of clean tiles. 

A slightly different implementation appears at the end of
this file in the function named solution2.
-}

solution1 :: [String] -> Int
solution1 ts = 
    _solution1 (Cell 0 0) Right Map.empty
    
  where
    _solution1 :: Cell -> Heading -> (Map Cell Heading) -> Int
    _solution1 cell curHeading visited
    
      -- Base cases
      | nextHeading == None                     = 1
      | isCellVisited cell nextHeading visited  = Map.size visited
      
      -- Recursive case
      | otherwise =
        let 
          traveled = Map.insert cell nextHeading visited
          nextCell = calcNextCell cell nextHeading
        in
          _solution1 nextCell nextHeading traveled 

      where
        nextHeading = calcNextHeading cell curHeading ts 

------        
{- Test cases:
   It's best to draw these grids of tiles and the robot vacuum
   cleaner's path to get a pictorial understanding of why the
   following solutions are correct
   
   1. solution1 ["...X..", "....XX", "..X..."]                == 6
   2. solution1 ["....X..", "X......", ".....X.", "......."]  == 15
   3. solution1 ["...X.", ".X..X", "X...X", "..X.."]          == 9
   4. solution1 ["."]                                         == 1
   5. solution1 [".X", "X."]                                  == 1
   6. solution1 ["...", ".X.", "..."]                         == 8
-}

------------  HELPER FUNCTIONS ------------------------
{- 
 Returns the address of the adjacent cell based
 on the heading.
 
 The adjacent cell might not exist if the current cell is at 
 the matrix's boundary. But it is not the responsibility of
 this function to verify the existence of the adjacent cell;
 this function merely calculates the adjacent cell's coordinates.
-}

calcNextCell :: Cell -> Heading -> Cell
calcNextCell (Cell x y) heading = 
  case heading of
    Right -> Cell (x+1) y
    Down  -> Cell x     (y+1)
    Left  -> Cell (x-1) y
    Up    -> Cell x     (y-1)
    None  -> Cell x     y
    
------------------------------------------------------------
{-
Given a matrix as a list of String, assuming each row contains
the same number of characters, this function returns the 
number of rows and columns as a tuple.
-}

getMatrixSize :: [[a]] -> (Int, Int)
getMatrixSize [[]]      = (0, 0)
getMatrixSize xs@(y:ys) = (length xs, length y)

{- Test Cases:
getMatrixSize [[]]           == (0, 0)
getMatrixSize ["."]          == (1, 1)
getMatrixSize ["..X"]        == (1, 3)
getMatrixSize ["..X", ".X."] == (2, 3)
-}

------------------------------------------------------------
{- Returns true if the given cell is a border cell,
   false otherwise.
-} 
isBorderCell :: Cell -> Int -> Int -> Bool
isBorderCell (Cell x y) cols rows
  | x == 0          = True
  | x == (cols - 1) = True
  | y == 0          = True
  | y == (rows - 1) = True
  | otherwise       = False
  
------------------------------------------------------------
{- Returns true if the matrix entry at the given cell
   location is an 'X', false otherwise.
-}

isObstacle :: Cell -> [String] -> Bool
isObstacle cell ts = 
  getMatrixEntry cell ts == 'X'

------------------------------------------------------------
{- Returns true if there's no boundary or obstacle in
   the adjacent cell in the direction of 'heading'
   
   Right -> Return False if the given cell is already at the
     right boundary OR if the cell to the right contains an
     obstacle 'X'
     
   Left -> Returns False if the given cell is already at the
     left boundary OR if the cell to the left contains an
     obstacle 'X'
-}
isAdjCellAvailable :: Cell -> Heading -> [String] -> Bool
isAdjCellAvailable cell@(Cell x y) heading ts =

  case heading of
    None  -> False
    Right -> not $ x == (cols-1) || isObstacle adjCell ts
    Left  -> not $ x == 0        || isObstacle adjCell ts
    Down  -> not $ y == (rows-1) || isObstacle adjCell ts
    Up    -> not $ y == 0        || isObstacle adjCell ts
    
  where
    (rows, cols) = getMatrixSize ts
    adjCell      = calcNextCell cell heading

{- Test cases:
isAdjCellAvailable (Cell 4 0) None [ "...X..", "....XX", "..X..."] == False
-}

------------------------------------------------------------
{-
  Given a matrix and the column and the row, it returns the
  entry at that location
-}

getMatrixEntry :: Cell -> [String] -> Char
getMatrixEntry (Cell col row) ts = (ts !! row) !! col

{- Test cases:
getMatrixEntry (Cell 1 1) ["..X", ".X."] == 'X'
getMatrixEntry (Cell 2 0) ["...", ".X."] == '.'
-}

------------------------------------------------------------
{- Turn right relative to the current heading. If the cell
   to the right is not available, turn right again, and so on.
   
   Return the heading of the first cell that's available; 
   return None if no cell is available.
-}

calcNextHeading :: Cell -> Heading -> [String] -> Heading
calcNextHeading cell heading ts =

  _calcNextHeading heading 1
  
  where
    _calcNextHeading :: Heading -> Int -> Heading
    _calcNextHeading curHeading turns
    
      -- Return None if we have returned to the starting point
      -- after turning right 4 times.
      | curHeading == None || turns >= 4      = None
      
      -- Check whether adjacent cell is available or is blocked
      | isAdjCellAvailable cell curHeading ts = curHeading
      
      -- Otherwise turn right
      | otherwise = _calcNextHeading (turnRight curHeading) (turns + 1)
      
{- Test cases:
calcNextHeading (Cell 0 1) Left [ "...X..", "....XX", "..X..."] == Up
calcNextHeading (Cell 0 0) None ["."] == None
-}

------------------------------------------------------------
{- Returns true if the cell has been visited previously
   while heading in the SAME direction, otherwise returns
   false.
-}

isCellVisited :: Cell -> Heading -> Map Cell Heading -> Bool
isCellVisited cell heading visited =

  case (Map.lookup cell visited) of
    Nothing             -> False
    Just (prevHeading)  -> heading == prevHeading    

{- Test cases:
isCellVisited (Cell 0 0) Right Map.empty == False
-}

------------------------------------------------------------
{- 
This function might be a little cleaner than solution1.
The test cases are the same as for solution1.
-}
solution2 :: [String] -> Int
solution2 ts = 
    _solution2 (Cell 0 0) nextDir Map.empty
    
  where
    nextDir = calcNextHeading (Cell 0 0) Right ts
    
    _solution2 :: Cell -> Heading -> (Map Cell Heading) -> Int
    _solution2 cell nextHeading visited
    
      -- Base cases
      | nextHeading == None                     = 1
      | isCellVisited cell nextHeading visited  = Map.size visited
      
      -- Recursive case
      | otherwise = 
        let
          traveled  = Map.insert cell nextHeading visited
          nextCell  = calcNextCell cell nextHeading
          nextDir   = calcNextHeading nextCell nextHeading ts
        in
          _solution2 nextCell nextDir traveled      
