module Main where

import Data.List (find)
import Control.Monad (forM_)
import Text.Read (readMaybe)

import Card
import Player
import Actions

main :: IO ()
main = gameLoop

-- Inisialisasi Permainan
gameLoop :: IO ()
gameLoop = do
    putStrLn "=== Memulai Permainan ==="
    
    -- Buat dek kartu acak
    shuffledDeck <- shuffleDeck buildDeck
    -- assumsi pure sirna, coba dibuat seeded (?)
    -- bisa dibuat ala-ala balatro
    
    let initialTable = table1 { drawDeck = shuffledDeck }
        gameTable = deal4CardsToEachPlayer initialTable
    
    putStrLn "\nInitial hands dealt:"
    print gameTable
    
    peekCardPhase <- peekIt gameTable 0
    peekCardPhase <- peekIt gameTable 1
    -- peekCardPhase <- peekIt gameTable 2
    -- peekCardPhase <- peekIt gameTable 3
    -- to be fixed for random player, for now 2 player

    finalTable <- playRounds gameTable 0
    putStrLn "\nPermainan Berakhir, skor saat ini:"
    finalTable' <- showFinalScores finalTable
    
    -- Loop permainan
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

    putStrLn $ "\nPemain " ++ show (playerId p) ++ ", pilih dua kartu untuk diintip (1-" ++ show n ++ ")"
    mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..n]

    putStrLn "Masukkan pilihan kartu (contoh 1 3):"
    input <- words <$> getLine

    case mapM readMaybe input of
      Just [a,b]
        | a /= b && a >= 1 && a <= n && b >= 1 && b <= n -> do
            let showCardAt i = showCardRS (hs !! (i-1))
            putStrLn "Kartu yang diintip sesuai urutan di tangan: "
            putStrLn $ " - " ++ showCardAt a
            putStrLn $ " - " ++ showCardAt b
            return table
      _ -> do
          putStrLn "Salah mas, masukkan sesuai tangan dan sesuai format (e.g. 1 3)."
          peekIt table currentPlayerIdx

-- Main game round loop
playRounds :: Table -> Int -> IO Table
playRounds table currentPlayerIdx
    | null (drawDeck table) = do
        putStrLn "\nKartu di dek habis! Permainan berakhir!"
        return table
    | otherwise = do
        print table
        let numPlayers = length $ players table
        
        -- Eksekusi giliran pemain
        newTable <- playerTurn table currentPlayerIdx
        
        -- Set indeks pada pemain berikutnya
        let nextPlayerIdx = (currentPlayerIdx + 1) `mod` numPlayers
        playRounds newTable nextPlayerIdx

-- Skor akhir masing-masing pemain
showFinalScores :: Table -> IO Table
showFinalScores table = do
    forM_ (players table) $ \p -> do
        let score = playerScore p
            Hand hs = hand p
        putStrLn $ "Skor setiap pemain " ++ show (playerId p) ++ ": " ++ show score ++ " poin dengan (" ++ show (length hs) ++ " kartu)"

    let ranked = rankPlayers table
        winner = head ranked
    putStrLn $ "\nPemenang: Pemain " ++ show (playerId winner)

    let updatedTable = updateStandings table
    showStandings updatedTable
    return updatedTable
