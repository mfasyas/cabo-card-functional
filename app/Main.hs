module Main where

import Card
import Player
import Actions
import Data.List (find)
import Control.Monad (forM_)
import Text.Read (readMaybe)

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
    
    -- peek phase
    peekCardPhase <- peekIt gameTable 0
    peekCardPhase <- peekIt gameTable 1
    peekCardPhase <- peekIt gameTable 2
    peekCardPhase <- peekIt gameTable 3

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

peekIt :: Table -> Int -> IO Table
peekIt table currentPlayerIdx = do
    let ps = players table
        (before, p:after) = splitAt currentPlayerIdx ps
        Hand hs = hand p
        n = length hs

    putStrLn $ "\nPlayer " ++ show (playerId p) ++ ", choose 2 cards to peek (1-" ++ show n ++ ")"
    mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..n]

    -- Read tuple input (a,b)
    putStrLn "Enter two distinct indices separated by space (e.g. 1 3):"
    input <- words <$> getLine

    case mapM readMaybe input of
      Just [a,b]
        | a /= b && a >= 1 && a <= n && b >= 1 && b <= n -> do
            let showCardAt i = showCardRS (hs !! (i-1))
            putStrLn "You peeked: "
            putStrLn $ " - " ++ showCardAt a
            putStrLn $ " - " ++ showCardAt b
            return table  -- we don’t modify anything
      _ -> do
          putStrLn "Invalid input! Please enter two distinct numbers (e.g. 1 3)."
          peekIt table currentPlayerIdx

-- Main game round loop
playRounds :: Table -> Int -> IO Table
playRounds table currentPlayerIdx
    | null (drawDeck table) = do
        putStrLn "\nDeck is empty! Game Over!"
        return table
    | otherwise = do
        putStrLn "\nCurrent table state:"
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
