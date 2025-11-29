module Main where

import System.IO
import Text.Read (readMaybe)
import Card (shuffleDeck, buildDeck, Card(..), Rank(..), Suit(..), Hand(..))
import Player
import GameStates
import GameEngine (updateGame)

main :: IO ()
main = do
    putStrLn "=== SIMULASI GAME ENGINE ==="
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    gameLoop state0

gameLoop :: GameState -> IO ()
gameLoop state = do
    printState state
    
    if phase state == GameOver
        then do
            putStrLn "\n!!! GAME OVER !!!"
            putStrLn "=== KLASEMEN AKHIR ==="
            mapM_ (\p -> putStrLn $ "Player " ++ show (playerId p) ++ 
                                   " | Score: " ++ show (handScore (hand p)) ++ 
                                   " | POINTS: " ++ show (matchPoints p)) 
                  (players state)
        else do
            let prompt = case phase state of
                           InitialPeekPhase _ -> "\n[INIT] initpeek <i1> <i2>: "
                           InitPeekFeedback _ _ -> "\n[INFO] Ketik 'finish' untuk lanjut: "
                           TimpaRound{} -> "\n[TIMPA] timpa <idx> / pass: "
                           PostRoundDecision -> "\n[END] kabul / finish: "
                           _ -> "\nCMD: draw | discard <i> | target <pid> <ids...> | skip: "
            
            putStr prompt
            hFlush stdout
            input <- getLine
            
            let pid = getCurrentActor state
            let action = parseInput pid input
            
            case action of
                Nothing -> do
                    putStrLn "[ERROR] Command salah."
                    gameLoop state
                Just act -> do
                    case updateGame state act of
                        Left err -> do
                            putStrLn $ "[GAGAL] " ++ err
                            gameLoop state 
                        Right newState -> do
                            putStrLn "[OK]"
                            gameLoop newState 

getCurrentActor :: GameState -> Int
getCurrentActor gs = 
    case phase gs of
        InitialPeekPhase (p:_) -> p
        InitPeekFeedback p _ -> p -- PENTING: Agar info ditampilkan ke pemain yang benar
        TimpaRound _ _ asking _ -> asking
        _ -> currentTurn gs

printState :: GameState -> IO ()
printState gs = do
    putStrLn "\n"
    print gs 

    let actorId = getCurrentActor gs
    let actor = (players gs) !! actorId
    let (Hand h) = hand actor

    -- Tampilkan kartu tangan
    putStrLn $ "Kartu Anda (Player " ++ show actorId ++ "):"
    mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "] " ++ show c) (zip [0..] h)
    
    -- Tampilkan Private Info jika ada
    case lookup actorId (privateInfo gs) of
        Just msg -> putStrLn $ "\n[!!! INFO RAHASIA !!!] -> " ++ msg
        Nothing  -> return ()

parseInput :: Int -> String -> Maybe GameAction
parseInput pid input = 
    case words input of
        ["draw"]                -> Just (DrawAction pid)
        ["kabul"]               -> Just (KabulAction pid)
        ["finish"]              -> Just (FinishTurnAction pid)
        ["pass"]                -> Just (PassTimpaAction pid)
        ["skip"]                -> Just (SkipPowerupAction pid)
        ["discard", iStr]       -> fmap (DiscardAction pid) (readMaybe iStr)
        ["timpa", iStr]         -> fmap (TimpaAction pid) (readMaybe iStr)
        ["initpeek", i1, i2]    -> do
                                     x <- readMaybe i1
                                     y <- readMaybe i2
                                     Just (InitPeekAction pid x y)
        
        ("target":pidStr:idxStrs) -> 
            case (readMaybe pidStr, mapM readMaybe idxStrs) of
                (Just tPid, Just indices) -> Just (TargetAction pid tPid indices)
                _ -> Nothing
                
        _ -> Nothing