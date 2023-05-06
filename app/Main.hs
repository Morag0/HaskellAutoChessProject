module Main (main) where

import Control.Monad.State
import Board
import Logic
import State


gameLoop :: State GameState ()
gameLoop = do
  currentState <- get
  case phase currentState of
    Shopping -> do
      -- Allow the player to place pieces on the board
      setPreparation
      gameLoop
    Preparation -> do
      -- Move all non-enemy pieces towards the closest enemy and allow the player to remove pieces from the board
      moveAllPiecesTowardsEnemy
      setBattle
      gameLoop
    Battle -> do
      -- Check if the player has won
      hasWon <- hasPlayerWon
      if hasWon
        then return ()
        else gameLoop


startGame :: IO ()
startGame = do
  putStrLn "Starting game..."
  let finalState = runGame gameLoop initialState
  putStrLn "Game over."
  return ()


main :: IO ()
main = do
  startGame