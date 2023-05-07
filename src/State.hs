module State (
    GameState(..),
    GamePhase(..),
    initialState,
    addPiece,
    setShopping,
    getPhase,
    runGame,
    moveAllPiecesTowardsEnemy,
    placePieceOnBoard,
    allPiecesAttack,
    attackFrom,
    inRange,
    getAttackRange,
    addPieceToCollection
) where

import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad

import Board
import Logic

-- | There are three phases in the game loop
data GamePhase = Shopping | Preparation | Battle deriving (Eq, Show)

-- | The GameState stores the board, state of the game aswell as the players pieces
data GameState = GameState {
    gameboard :: GameBoard,
    phase :: GamePhase,
    collection :: [(PieceType, Bool, PieceStats)]}

-- | Places a specific piece on a specific position on the board
-- | The function gets the currentState, then updates the board using placePiece and sets the new state with the updated board using put
addPiece :: (Int, Int) -> (PieceType, Bool, PieceStats) -> StateT GameState IO ()
addPiece (x,y) piece = do
    currentState <- get
    put $ currentState { gameboard = placePiece (gameboard currentState) (x,y) piece }

-- | Sets the initial state of the game to Shopping
initialState :: GameState
initialState = GameState { gameboard = initBoard, phase = Shopping, collection = [] }

-- | Sets the game phase to Shopping
setShopping :: StateT GameState IO ()
setShopping = modify (\s -> s { phase = Shopping })

-- | Gets the current game phase
getPhase :: StateT GameState IO GamePhase
getPhase = gets phase

-- | Used to place pieces on the board
-- | It checks if there are empty positions on the board, if there are none it returns
-- | If there is an emptyPoistion it places the piece at the first available one
placePieceOnBoard :: GameBoard -> (Int, Int) -> (PieceType, Bool, PieceStats) -> StateT GameState IO ()
placePieceOnBoard board (x,y) piece = do
  currentState <- get
  let emptyPositions = emptySquares (gameboard currentState)
  if null emptyPositions
    then return ()
    else do
      let newPosition = head emptyPositions
      addPiece (row newPosition, col newPosition) piece

-- | Moves all pieces towards the closest enemy piece
-- | It first gets the state of the game, and extracts the game board. Then it creates a list of all the positions of enemies on the board using list comprehension
-- | It then defines a function movePiece that takes a position as input, and using findPathTowardsClosestEnemy it calculates the position it wants to move addPieceToCollection
-- | It then moves the piece using movePieceOnBoard and updates the gameState with the new board
-- | it then maps over movePiece causing all enemy pieces to move
moveAllPiecesTowardsEnemy :: StateT GameState IO ()
moveAllPiecesTowardsEnemy = do
  currentState <- get
  let board = gameboard currentState
  let height = length board
  let piecePositions = [Position x y | x <- [0..height-1], y <- [0..height-1], squareIsEnemy (getSquare board (Position x y))]
  let movePiece pos =
        let newPos = findPathTowardsClosestEnemy board pos
        in case newPos of
             p -> do
               let newBoard = movePieceOnBoard board pos p
               modify (\s -> s { gameboard = newBoard })
  mapM_ movePiece piecePositions



-- | Runs the game given an initial state
runGame :: StateT GameState IO () -> GameState -> IO GameState
runGame = execStateT

-- | maps the attackFrom functrion over a list of positions that have pieces on them.
allPiecesAttack :: StateT GameState (StateT GameState IO) ()
allPiecesAttack = do
  currentState <- get
  let board = gameboard currentState
      height = length board
      width = length (head board)
      piecePositions = [Position row col | row <- [0..(height-1)], col <- [0..(width-1)], getSquare board (Position row col) /= EmptySquare]
  mapM_ attackFrom piecePositions


-- | takes a position on the board and attacks all enemy pieces within range
-- | It does this by first getting the current state of the game board, then finding the attackingSquare which is the piece located at pos on the board
-- | If the attackingSquare is an occupied square and is not an enemy piece, then it calculates the attack range of the piece, finds all the positions within this range, and filters out only those positions occupied by enemy pieces
-- | It then applies the attackPiece function to each of the attacked positions and updates the game board. If attackingSquare is not an occupied square or is an enemy piece, then it simply returns without making any changes to the game board
-- | This function is used by allPiecesAttack which applies this function to all occupied squares on the board.
attackFrom :: BoardPosition -> StateT GameState (StateT GameState IO) ()
attackFrom pos = do
  board <- gets gameboard
  let attackingSquare = getSquare board pos
  let width = length (head board)
  let height = length board
  case attackingSquare of
    OccupiedSquare _ False attStats -> do
      let attackRange = getAttackRange pos board
      let positionsInRange = filter (inRange pos attackRange) [Position row col | row <- [0..(height-1)], col <- [0..(width-1)]]
      let attackedPositions = filter (\p -> let s = getSquare board p in case s of { OccupiedSquare _ True _ -> True; _ -> False }) positionsInRange
      let newBoard = foldl (`attackPiece` pos) board attackedPositions
      modify (\s -> s { gameboard = newBoard })
    _ -> return ()

-- | Returns a boolean depending if an enemy is within range
inRange :: BoardPosition -> Int -> BoardPosition -> Bool
inRange (Position row1 col1) range (Position row2 col2) = abs (row1 - row2) + abs (col1 - col2) <= range

-- | Gets the attackrange for a given piece, which is its attack stat
getAttackRange :: BoardPosition -> GameBoard -> Int
getAttackRange pos board = case getSquare board pos of
                             OccupiedSquare _ _ stats -> attack stats
                             EmptySquare -> error "Cannot get attack range for empty square"


-- | Adds a piece to the players collection of pieceStats
-- | It takes in a tuple representing the piece to add, which contains the piece type, whether it is an enemy piece, and the piece's stats
-- | It then gets the current game state, updates the collection with the new piece added, and sets the game state's collection to the updated collection. Finally, it returns the updated collection.
addPieceToCollection :: (PieceType, Bool, PieceStats) -> StateT GameState IO [(PieceType, Bool, PieceStats)]
addPieceToCollection piece = do
  currentState <- get
  let newCollection = piece : collection currentState
  put $ currentState { collection = newCollection }
  return newCollection




