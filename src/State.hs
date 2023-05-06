module State (
    GameState(..),
    GamePhase(..),
    initialState,
    addPiece,
    setShopping,
    setPreparation,
    setBattle,
    getPhase, 
    runGame,
    moveAllPiecesTowardsEnemy,
    placePieceOnBoard,
    hasPlayerWon
) where

import Control.Monad.State


import Board
import Logic

data GamePhase = Shopping | Preparation | Battle deriving (Eq, Show)

data GameState = GameState {
    gameboard :: GameBoard,
    phase :: GamePhase
}

-- | Adds a piece to the board
addPiece :: (Int, Int) -> (PieceType, Bool, PieceStats) -> State GameState ()
addPiece pos piece = do
    currentState <- get
    put $ currentState { gameboard = placePiece (gameboard currentState) pos piece }

initialState :: GameState
initialState = GameState { gameboard = emptyBoard 6 6, phase = Shopping }


-- | Sets the game phase to Shopping
setShopping :: State GameState ()
setShopping = modify (\s -> s { phase = Shopping })

-- | Sets the game phase to Preparation
setPreparation :: State GameState ()
setPreparation = modify (\s -> s { phase = Preparation })

-- | Sets the game phase to Battle
setBattle :: State GameState ()
setBattle = modify (\s -> s { phase = Battle })

-- | Gets the current game phase
getPhase :: State GameState GamePhase
getPhase = gets phase

placePieceOnBoard :: (Int, Int) -> (PieceType, Bool, PieceStats) -> State GameState ()
placePieceOnBoard pos piece = do
  currentState <- get
  let emptyPositions = emptySquares (gameboard currentState)
  if null emptyPositions
    then return ()
    else do
      let newPosition = head emptyPositions
      addPiece (row newPosition, col newPosition) piece

moveAllPiecesTowardsEnemy :: State GameState ()
moveAllPiecesTowardsEnemy = do
  currentState <- get
  let board = gameboard currentState
  let pieces = [(Position x y, getSquare board (Position x y)) | x <- [0..width-1], y <- [0..height-1], isPiece (getSquare board (Position x y))]
  let movedPieces = map (\(pos, piece) -> (findPathTowardsClosestEnemy board pos, piece)) pieces
  put $ currentState { gameboard = foldl (\b (pos, piece) -> setSquare b (row pos, col pos) piece) board movedPieces }
  where
    width = length (head (gameboard initialState))
    height = length (gameboard initialState)
    isPiece (OccupiedSquare _ False _) = True
    isPiece _ = False

hasPlayerWon :: State GameState Bool
hasPlayerWon = do
  currentState <- get
  return $ playerHasWon (gameboard currentState)


-- | Runs the game given an initial state
runGame :: State GameState a -> GameState -> GameState
runGame = execState
