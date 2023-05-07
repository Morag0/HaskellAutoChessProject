{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad.Trans.State
import Logic
import State
import Control.Monad.Trans (lift, MonadIO (liftIO))
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Data.List
import Board

gameLoop :: StateT GameState IO ()
gameLoop = do
  currentState <- get
  case phase currentState of
    Shopping -> do
      let options = "Select a new piece to add to your collection: (1) Assassin, (2) Skirmisher, (3) Defender"
      liftIO $ putStrLn options
      liftIO $ hFlush stdout
      choice <- liftIO getLine
      newState <- case choice of
        "1" -> do
          let pieceToAdd = (Assassin, False, assasinStats)
          liftIO $ putStrLn $ "Adding piece to collection: " ++ show pieceToAdd
          return $ addPieceToCollection pieceToAdd currentState
        "2" -> do
          let pieceToAdd = (Skirmisher, False, skirmisherStats)
          liftIO $ putStrLn $ "Adding piece to collection: " ++ show pieceToAdd
          return $ addPieceToCollection pieceToAdd currentState
        "3" -> do
          let pieceToAdd = (Defender, False, defenderStats)
          liftIO $ putStrLn $ "Adding piece to collection: " ++ show pieceToAdd
          return $ addPieceToCollection pieceToAdd currentState
        _ -> error "Should not happen"
      let updatedState = update newState { phase = Preparation }
      modify $ const updatedState
      gameLoop
    Preparation -> do
      currentState <- get
      liftIO $ displayGameBoard (gameboard currentState)
      let board = gameboard currentState
      let height = length board
      let bottomHalf = take (height `div` 2) (reverse board)
      let emptyPositions = concatMap emptySquares bottomHalf
      if null emptyPositions
        then do
          liftIO $ putStrLn "No more empty positions"
          modify $ \s -> return $ s { phase = Battle, gameboard = newBoard, collection = newCollection }
      else do
        let options = "Select a position to place your piece: " ++ displayPositions emptyPositions ++ "\n\nYour collection: " ++ show (collection currentState)
        liftIO $ putStrLn options
        choice <- liftIO getLine
        case readMaybe choice of
          Just pos -> do
            case collection currentState of
              [] -> do
                liftIO $ putStrLn "No more pieces in collection"
                modify (\s -> s { phase = Battle })
                gameLoop
              (piece : rest) -> do
                let newBoard = placePiece board (row pos, col pos) piece
                let newCollection = rest
                liftIO $ displayGameBoard (gameboard currentState)
                modify (\currentState -> currentState { collection = newCollection })
                gameLoop
          Nothing -> do
            liftIO $ putStrLn "Invalid position"
            gameLoop
    Battle -> do
      currentState <- get
      liftIO $ displayGameBoard (gameboard currentState)
      let attackAndMove = do
        allPiecesAttack
        moveAllPiecesTowardsEnemy
      lift $ evalStateT attackAndMove currentState
      currentState <- get
      liftIO $ displayGameBoard (gameboard currentState)
      hasWon <- gets (playerHasWon . gameboard)
      if hasWon
        then do
          liftIO $ putStrLn "Game over. You Won!"
          return ()
      else do
        enemyPiecesLeft <- gets (any (\case (OccupiedSquare _ False _) -> True; _ -> False) . concat . gameboard)
        if enemyPiecesLeft
          then gameLoop
        else do
          liftIO $ putStrLn "Game over. You lost!"
          modify (\s -> s { phase = Shopping })
          gameLoop

displayGameBoard :: GameBoard -> IO ()
displayGameBoard board = putStrLn (showBoardWithPieces board)

startGame :: IO ()
startGame = do
  putStrLn "Starting game..."
  finalState <- execStateT gameLoop initialState
  putStrLn "Game over."
  return ()

main :: IO ()
main = startGame

