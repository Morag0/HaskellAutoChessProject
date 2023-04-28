module GameState (
    GameState(..),
    addPiece,
    setShopping,
    setPreparation,
    setBattle,
    getPhase,
    runGame
) where

import Control.Monad.State.Lazy


import Board

data GamePhase = Shopping | Preparation | Battle deriving (Eq, Show)

data GameState = GameState {
    board :: Board,
    phase :: GamePhase
}

-- | Adds a piece to the board
addPiece :: (Int, Int) -> (PieceType, Bool) -> State GameState ()
addPiece pos piece = do
    currentState <- get
    put $ currentState { board = placePiece (board currentState) pos piece }

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

-- | Runs the game given an initial state
runGame :: State GameState a -> GameState -> GameState
runGame = execState
