module Logic (
    findPathTowardsClosestEnemy,
    isValidMove,
    adjacentSquares,
    playerHasWon,
    squareIsEnemy,
    findClosestEnemy,
    findClosestEnemyHelper,
    isEmptySquare,
    distance
) where

import Board



-- | Given a board and a position of a piece, it returns the position the piece would want to move to.
-- | If the piece cannot move, returns the same position.
-- | It first checks whether there is an enemy piece in the board, and if there is, it identifies the adjacent squares to the current position. 
-- | It then filters out invalid squares (squares where the piece cannot move to) and calculates the distance between each of the valid squares and the enemy piece. 
-- | It selects the minimum distance and filters out all the squares that are not at that minimum distance from the enemy piece. Finally, it returns the head of that list of squares, or the original position if no enemy piece is found on the board.
findPathTowardsClosestEnemy :: GameBoard -> BoardPosition -> BoardPosition
findPathTowardsClosestEnemy board pos =
  case findClosestEnemy board pos of
    Just enemyPos ->
      let adjSquares = adjacentSquares board pos
          validSquares = filter (isValidMove board pos) adjSquares
          distances = map (distance enemyPos) validSquares
          minDistance = minimum distances
          closestSquares = map snd $ filter ((== minDistance) . fst) $ zip distances validSquares
      in if null closestSquares then pos else head closestSquares
    Nothing -> pos



-- | Finds all adjacent squares that can be moved to
adjacentSquares :: GameBoard -> BoardPosition -> [BoardPosition]
adjacentSquares board (Position x y) = filter (isValidMove board (Position x y)) [Position (x+1) y, Position (x-1) y, Position x (y+1), Position x (y-1)]

-- | This function checks if the move the piece is trying to do is valid. The pieces should be able to move up, down, left or right within the boards limits
isValidMove :: GameBoard -> BoardPosition -> BoardPosition -> Bool
isValidMove board (Position x1 y1) (Position x2 y2)
  | x2 < 0 || x2 >= width || y2 < 0 || y2 >= height = False
  | x1 == x2 && abs (y1 - y2) == 1 = True
  | y1 == y2 && abs (x1 - x2) == 1 = True
  | otherwise = False
  where
    width = length (head board)
    height = length board


-- | Function that checks if there are any Enemy pieces on there board. Returns True if there are none
playerHasWon :: GameBoard -> Bool
playerHasWon board = all (not . squareIsEnemy) (concat board)

-- | Helper Function to check if there is an Enemy piece on the square
squareIsEnemy :: Square -> Bool
squareIsEnemy (OccupiedSquare _ isEnemy stats) = isEnemy
squareIsEnemy EmptySquare = False


-- | Finds the closest enemy given a position on the board
findClosestEnemy :: GameBoard -> BoardPosition -> Maybe BoardPosition
findClosestEnemy board pos = findClosestEnemyHelper board [(pos, 0)] []

-- | Recursive helper function for findClosestEnemy
-- | Tries to find the closest enemy by exploring each adjacent square
-- | If the current position contains an enemy, it returns Just p, where p is the current position. 
-- | Otherwise, it generates a new list of squares with their respective distances to the starting position, which includes all unvisited adjacent squares, and appends this new list to the original list of squares. 
-- | It also adds the new adjacent squares to the list of visited squares.
-- | This function will continue to explore each adjacent square until it either finds an enemy or has explored all squares. If it cannot find an enemy, it will return Nothing.
findClosestEnemyHelper :: GameBoard -> [(BoardPosition, Int)] -> [BoardPosition] -> Maybe BoardPosition
findClosestEnemyHelper _ [] _ = Nothing
findClosestEnemyHelper b ((p, d):ps) visited
  | squareIsEnemy (getSquare b p) = Just p
  | otherwise =
      let adjSquares = filter (`notElem` visited) (adjacentSquares b p)
          newSquares = [(s, d+1) | s <- adjSquares]
      in findClosestEnemyHelper b (ps ++ newSquares) (visited ++ adjSquares)

-- | Checks if a square is empty or not
isEmptySquare :: GameBoard -> BoardPosition -> Bool
isEmptySquare b p = case getSquare b p of
                      EmptySquare -> True
                      _ -> False

-- | Finds the absolute difference between two positions on the board
distance :: BoardPosition -> BoardPosition -> Int
distance (Position x1 y1) (Position x2 y2) = abs (x1 - x2) + abs (y1 - y2)














