module Main where

import System.IO
import Card (shuffleDeck, buildDeck, Card(..), Rank(..), Suit(..), Hand(..))
import Player
import GameStates
import GameEngine (updateGame)

main :: IO ()
main = do
    putStrLn "=== SIMULASI GAME ENGINE (STATE MACHINE) ==="
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    gameLoop state0

gameLoop :: GameState -> IO ()
gameLoop state = do
    printState state
    
    if phase state == GameOver
        then do
            putStrLn "\n!!! PERMAINAN BERAKHIR !!!"
            putStrLn "Klasemen Akhir:"
            mapM_ (\p -> putStrLn $ "Player " ++ show (playerId p) ++ " Score Kartu: " ++ show (handScore (hand p)) ++ " -> Match Points: " ++ show (score p)) (players state)
        else do
            putStr "\nPerintah (draw / kabul / discard <idx> / timpa <idx> / pass / target <i1> <i2> / skip): "
            hFlush stdout
            input <- getLine
            
            -- Parsing ID berdasarkan Phase. 
            -- Jika Phase Timpa, ID adalah askingIdx. Jika tidak, ID = currentTurn.
            let pid = case phase state of
                        TimpaPhase _ _ asking _ -> asking
                        _ -> currentTurn state

            let action = parseInput pid input
            
            case action of
                Nothing -> do
                    putStrLn "Error: Perintah tidak dikenali."
                    gameLoop state
                Just act -> do
                    case updateGame state act of
                        Left err -> do
                            putStrLn $ "\n[!!!] ATURAN DILANGGAR: " ++ err
                            gameLoop state 
                        Right newState -> do
                            putStrLn "\n[OK] Aksi berhasil."
                            gameLoop newState 

printState :: GameState -> IO ()
printState gs = do
    putStrLn "\n"
    print gs 

    let currP = currentPlayer gs
    let (Hand h) = hand currP
    
    -- Tampilkan kartu tangan untuk pemain yang sedang aktif (Hotseat simulation)
    -- Jika fase Timpa, tampilkan kartu pemain yang ditanya
    let activePId = case phase gs of 
                      TimpaPhase _ _ asking _ -> asking 
                      _ -> currentTurn gs
    
    let activeHand = hand ((players gs) !! activePId)
    let (Hand hActive) = activeHand

    putStrLn $ "Kartu di Tangan Pemain " ++ show activePId ++ ":"
    mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "] " ++ show c) (zip [0..] hActive)

    putStrLn "Logs Terakhir:"
    mapM_ (\l -> putStrLn $ "  > " ++ l) (take 3 $ reverse $ logs gs)

    -- Fitur 6: Private Info Check
    -- Hanya tampilkan jika privateInfo ditujukan untuk pemain yang sedang aktif
    case privateInfo gs of
        [] -> return ()
        infoList -> do
            let myInfo = filter (\(ownerId, _) -> ownerId == activePId) infoList
            if null myInfo 
                then return () 
                else putStrLn $ "\n[!!!] INFO RAHASIA ANDA: " ++ show myInfo

parseInput :: Int -> String -> Maybe GameAction
parseInput pid input = 
    case words input of
        ["draw"]                -> Just (DrawAction pid)
        ["kabul"]               -> Just (KabulAction pid)
        ["discard", idx]        -> Just (DiscardAction pid (read idx))
        ["timpa", idx]          -> Just (TimpaAction pid (read idx))
        ["pass"]                -> Just (PassTimpaAction pid)
        ["skip"]                -> Just (SkipPowerupAction pid)
        ["target", idx1, idx2]  -> Just (TargetAction pid (read idx1, read idx2))
        _                       -> Nothing