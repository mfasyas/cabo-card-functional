module Main where

import System.IO
import Text.Read (readMaybe)
import Card 
import Player
import GameStates
import GameEngine (updateGame)

main :: IO ()
main = do
    putStrLn "====== GAME SIMULATION IN CLI ======"
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    gameLoop state0

gameLoop :: GameState -> IO ()
gameLoop state = do
    printState state
    
    if phase state == GameOver
        then do
            putStrLn "\n!!! GAME OVER !!!"
            putStrLn "=== STANDINGS ==="
            mapM_ (\p -> putStrLn $ "Player " ++ show (playerId p) ++ 
                                   " | SCORE  : " ++ show (handScore (hand p)) ++ 
                                   " | POINTS : " ++ show (matchPoints p)) 
                  (players state)
        else do
            let prompt = case phase state of
                           InitialPeekPhase _ -> "\n[INIT] initpeek <i1> <i2>: "
                           InitPeekFeedback _ _ -> "\n[INFO] Type 'finish' to proceed: "
                           TimpaRound{} -> "\n[== STACK ==] timpa <idx> / pass: "
                           PostRoundDecision -> "\n[END] kabul / finish: "
                           _ -> "\nCMD: draw | discard <i> | target <pid> <ids...> | skip: "
            
            putStr prompt
            hFlush stdout
            input <- getLine
            
            let pid = getCurrentActor state
            let action = parseInput pid input
            
            case action of
                Nothing -> do
                    putStrLn "[== ERROR ==] Wrong Command."
                    gameLoop state
                Just act -> do
                    case updateGame state act of
                        Left err -> do
                            putStrLn $ "[== FAILED ==] " ++ err
                            gameLoop state 
                        Right newState -> do
                            putStrLn "[==== OK ====] "
                            gameLoop newState 

getCurrentActor :: GameState -> Int
getCurrentActor gs = 
    case phase gs of
        InitialPeekPhase (p:_) -> p
        InitPeekFeedback p _ -> p 
        TimpaRound _ _ asking _ -> asking
        _ -> currentTurn gs

printState :: GameState -> IO ()
printState gs = do
    putStrLn "\n"
    print gs 

    let actorId = getCurrentActor gs
        actor = (players gs) !! actorId
        (Hand h) = hand actor

    -- Tampilkan kartu tangan
    putStrLn $ "Your Cards (Player " ++ show actorId ++ "):"
    -- mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "] " ++ show c) (zip [0..] h) -- for testing
    let cardStrings = map (\(i, c) -> "[" ++ "Card at " ++ show i ++ "] ") (zip [0..] h)
        fullLine    = unwords cardStrings -- Gabungkan dengan spasi

    -- Print sekali saja
    putStrLn fullLine
    
    -- Tampilkan Private Info jika ada
    case lookup actorId (privateInfo gs) of
        Just msg -> putStrLn $ "\n[!!! SECRET INFORMATION !!!] -> " ++ msg
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