module Main where

import Card
import Player
import Actions
import Data.List (find)
import Control.Monad (forM_)

main :: IO ()
main = gameLoop

-- Initialize game and start the main game loop
gameLoop :: IO ()
gameLoop = do
    putStrLn "=== Starting the Game ==="
    
    -- Create and shuffle the deck
    shuffledDeck <- shuffleDeck buildDeck
    
    -- Set up initial table and deal cards
    let initialTable = table1 { drawDeck = shuffledDeck }
        gameTable = deal4CardsToEachPlayer initialTable
    
    putStrLn "\nInitial hands dealt:"
    print gameTable
    
    -- Start the game with player 1 (index 0)
    finalTable <- playRounds gameTable 0
    
    -- Show final state and scores
    putStrLn "\nGame Over! Final scores:"
    showFinalScores finalTable
    
    -- Ask for new game
    putStrLn "\nPlay again? (y/n)"
    choice <- getLine
    if choice == "y"
        then gameLoop
        else putStrLn "Goodbye!"

-- Main game round loop
playRounds :: Table -> Int -> IO Table
playRounds table currentPlayerIdx
    | null (drawDeck table) = do
        putStrLn "\nDeck is empty! Game Over!"
        return table
    | otherwise = do
        putStrLn $ "\nCurrent table state:"
        print table
        
        -- Execute current player's turn
        newTable <- playerTurn table currentPlayerIdx
        
        -- Move to next player (cycle through 0-3)
        let nextPlayerIdx = (currentPlayerIdx + 1) `mod` 4
        
        -- Continue with next player
        playRounds newTable nextPlayerIdx

-- Show final scores for all players
showFinalScores :: Table -> IO ()
showFinalScores table = 
    forM_ (players table) $ \p -> do
        let score = playerScore p
        putStrLn $ "Player " ++ show (playerId p) ++ ": " ++ show score ++ " points"
