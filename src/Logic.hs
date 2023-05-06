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
findPathTowardsClosestEnemy :: GameBoard -> Position -> Position
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
adjacentSquares :: GameBoard -> Position -> [Position]
adjacentSquares board (Position x y) = filter (isValidMove board (Position x y)) [Position (x+1) y, Position (x-1) y, Position x (y+1), Position x (y-1)]

-- | This function checks if the move the piece is trying to do is valid. The pieces should be able to move up, down, left or right within the boards limits
isValidMove :: GameBoard -> Position -> Position -> Bool
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

findClosestEnemy :: GameBoard -> Position -> Maybe Position
findClosestEnemy board pos = findClosestEnemyHelper board [(pos, 0)] []


findClosestEnemyHelper :: GameBoard -> [(Position, Int)] -> [Position] -> Maybe Position
findClosestEnemyHelper _ [] _ = Nothing
findClosestEnemyHelper b ((p, d):ps) visited
  | squareIsEnemy (getSquare b p) = Just p
  | otherwise =
      let adjSquares = filter (`notElem` visited) (adjacentSquares b p)
          newSquares = [(s, d+1) | s <- adjSquares]
      in findClosestEnemyHelper b (ps ++ newSquares) (visited ++ adjSquares)


isEmptySquare :: GameBoard -> Position -> Bool
isEmptySquare b p = case getSquare b p of
                      EmptySquare -> True
                      _ -> False


distance :: Position -> Position -> Int
distance (Position x1 y1) (Position x2 y2) = abs (x1 - x2) + abs (y1 - y2)














