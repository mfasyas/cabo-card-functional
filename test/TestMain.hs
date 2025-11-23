module Main where

import System.IO
import Text.Read (readMaybe)
import Control.Monad (forever)

-- Import modul-modul game kita
import Card (shuffleDeck, buildDeck, Card(..), Rank(..), Suit(..), Hand(..))
import Player
import GameStates
import GameEngine (updateGame)

main :: IO ()
main = do
    putStrLn "=== SIMULASI GAME ENGINE (STATE MACHINE) ==="
    
    -- 1. Setup Deck & State Awal
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    
    -- 2. Masuk ke Loop Simulasi
    gameLoop state0

gameLoop :: GameState -> IO ()
gameLoop state = do
    printState state
    
    -- Cek Game Over
    if phase state == GameOver
        then putStrLn "!!! GAME OVER !!!"
        else do
            -- Minta Input User
            putStr "\nPerintah (draw / discard <idx> / target <idx1> <idx2>): "
            hFlush stdout
            input <- getLine
            
            -- Parsing Input User ke GameAction
            let pid = currentTurn state -- Otomatis pakai ID player yang sedang giliran (0 atau 1)
            let action = parseInput pid input
            
            case action of
                Nothing -> do
                    putStrLn "Error: Perintah tidak dikenali."
                    gameLoop state
                Just act -> do
                    -- === INI INTI TESTINGNYA ===
                    -- Panggil Engine Update
                    case updateGame state act of
                        Left err -> do
                            putStrLn $ "\n[!!!] ATURAN DILANGGAR: " ++ err
                            gameLoop state -- Ulangi loop dengan state lama
                        Right newState -> do
                            putStrLn "\n[OK] Aksi berhasil."
                            gameLoop newState -- Lanjut dengan state baru

-- Fungsi Tampilan Sederhana
printState :: GameState -> IO ()
printState gs = do
    putStrLn "\n=========================================="
    putStrLn $ "Giliran: Pemain " ++ show (playerId (currentPlayer gs))
    putStrLn $ "Fase   : " ++ show (phase gs)
    
    let (Hand myHand) = hand (currentPlayer gs)
    putStrLn "Kartu di Tangan:"
    mapM_ (\(i, c) -> putStrLn $ "  " ++ show i ++ ". " ++ show c) (zip [0..] myHand)
    
    putStrLn "Logs Terakhir:"
    mapM_ (\l -> putStrLn $ "  > " ++ l) (take 3 $ reverse $ logs gs)
    
    case privateInfo gs of
        [] -> return ()
        info -> putStrLn $ "INFO RAHASIA: " ++ show info
    putStrLn "=========================================="

-- Parsing perintah text jadi Data Action
parseInput :: Int -> String -> Maybe GameAction
parseInput pid input = 
    case words input of
        ["draw"]          -> Just (DrawAction pid)
        ["discard", idx]  -> Just (DiscardAction pid (read idx))
        ("target":idxs)   -> Just (TargetAction pid (map read idxs))
        ["finish"]        -> Just (FinishGameAction pid)
        _                 -> Nothing