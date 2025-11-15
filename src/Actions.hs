module Actions where

import Card
import Player

import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)

-- Menentukan peringkat berdasarkan skor
rankPlayers :: Table -> [Player]
rankPlayers table =
    let ps = players table
    in sortBy comparePlayers ps
  where
    comparePlayers p1 p2 =
        case compare (playerScore p1) (playerScore p2) of
            EQ -> compare (lengthHand p1) (lengthHand p2)
            other -> other
    lengthHand (Player _ (Hand hs)) = length hs

-- Update standings secara keseluruhan
-- ======================== ini belum selesai ========================
updateStandings :: Table -> Table
updateStandings table =
    let ranking = rankPlayers table
        n = length ranking
        -- awardPoints: n-1 down to 0
        awardPoints = [n-1, n-2 .. 0]
        updates = zip (map playerId ranking) awardPoints
        updateScore (pid, pts) =
            case lookup pid (standings table) of
                Just old -> (pid, old + pts)
                Nothing  -> (pid, pts)
        newStandings = map updateScore updates
    in table { standings = newStandings }

-- Tampilan Standings
showStandings :: Table -> IO ()
showStandings table = do
    putStrLn "\n=== Overall Standings ==="
    let sorted = sortBy (flip $ comparing snd) (standings table)
    mapM_ (\(pid, pts) ->
        putStrLn $ "Player " ++ show pid ++ ": " ++ show pts ++ " pts") sorted


-- Step 1: Draw for player, put new card at front of hand
drawForHand :: Table -> Int -> Table
drawForHand table playerIdx =
    let ps = players table
        deck = drawDeck table
        (drawnCard, newDeck) = drawCard deck
        (before, p:after) = splitAt playerIdx ps
        Hand hs = hand p
        newHand = drawnCard : hs
        newPlayer = p { hand = Hand newHand }
        newPlayers = before ++ [newPlayer] ++ after
    in table { players = newPlayers, drawDeck = newDeck }

-- Step 2: Discard from hand at given index (1-based)
discardFromHand :: Table -> Int -> Int -> Table
discardFromHand table playerIdx discardIdx =
    let ps = players table
        pile = discardPile table
        (before, p:after) = splitAt playerIdx ps
        Hand hs = hand p
        (toDiscard, keptHand) = removeAt (discardIdx - 1) hs
        newPlayer = p { hand = Hand keptHand }
        newPlayers = before ++ [newPlayer] ++ after
        newPile = toDiscard : pile
    in table { players = newPlayers, discardPile = newPile }

-- Helper function to remove element at index
removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs = let (l, r) = splitAt idx xs in (head r, l ++ tail r)

-- Show a player's hand with indices
showHandWithIndices :: Hand -> String
showHandWithIndices (Hand cards) =
    unlines $ zipWith showCard [1..] cards
    where
        showCard idx card = "Card at " ++ show idx ++ ". " ++ showCardRS card
            -- | idx == 1  = "Card at " ++ show idx ++ ". " ++ showCardRS card
            -- | otherwise = "Card at " ++ show idx
-- untuk saat ini buka dulu sebagai testing agar mudah, tinggal uncomment untuk tutup dan hapus line = where

-- Player Turn, IO aktivitas berdasarkan aksi player
playerTurn :: Table -> Int -> IO Table
playerTurn table playerIdx = do
    let currentPlayer = players table !! playerIdx
    putStrLn $ "\nPlayer " ++ show (playerId currentPlayer) ++ "'s turn"
    putStrLn "Do you want to end the game now? (y/n)"
    endChoice <- getLine
    if endChoice == "y"
        then do
            putStrLn $ "\nPlayer " ++ show (playerId currentPlayer) ++ " has ended the game!"
            return table { drawDeck = [] } -- Trigger end case di mainLoop
        else do
            -- Step 1: Draw card
            let tableAfterDraw = drawForHand table playerIdx
                updatedPlayer = players tableAfterDraw !! playerIdx
                currentHand = hand updatedPlayer

            -- Step 2: Show hand and ask discard
            putStrLn "Current hand, top of Card is newly drawn Card:"
            putStrLn $ showHandWithIndices currentHand

            -- Step 3: Discard
            choice <- getValidChoice (length $ let Hand h = currentHand in h)
            let tableAfterDiscard = discardFromHand tableAfterDraw playerIdx choice
            putStrLn $ "Card " ++ show choice ++ " has been discarded."

            -- Step 4: Trigger addToPile phase
            addToPile tableAfterDiscard playerIdx

-- Helper to get valid input
getValidChoice :: Int -> IO Int
getValidChoice maxIdx = do
    putStrLn $ "Choose a card to discard (1-" ++ show maxIdx ++ "):"
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= maxIdx -> return n
        _ -> do
            putStrLn "Please choose a valid index!"
            getValidChoice maxIdx


-- =============================================================================================

addToPile :: Table -> Int -> IO Table
addToPile table playerIdx = do
    let pileTop = head (discardPile table)
        topRank = rank pileTop
        ps = players table
        numPlayers = length ps

    putStrLn $ "\nThe top of the pile is: " ++ showCardRS pileTop

    -- Step 1: ask the discarding player first
    newTable <- askPlayerToAdd table playerIdx topRank
    if newTable /= table
        then return newTable  -- someone added successfully
        else checkOtherPlayers table ((playerIdx + 1) `mod` numPlayers) playerIdx topRank
  where
    -- Recursively check other players until back to start
    checkOtherPlayers t currentIdx startIdx targetRank
        | currentIdx == startIdx = return t
        | otherwise = do
            newT <- askPlayerToAdd t currentIdx targetRank
            if newT /= t then return newT
            else checkOtherPlayers t ((currentIdx + 1) `mod` length (players t)) startIdx targetRank

askPlayerToAdd :: Table -> Int -> Rank -> IO Table
askPlayerToAdd table playerIdx targetRank = do
    let ps = players table
        player = ps !! playerIdx
        Hand hs = hand player

    putStrLn $ "\nPlayer " ++ show (playerId player) ++ ", do you want to add a card with rank " ++ show targetRank ++ "? (y/n)"
    choice <- getLine
    if choice /= "y" then return table
    else do
        putStrLn "Your hand:"
        putStrLn $ showHandWithIndices (hand player)
        idx <- getValidChoice (length hs)
        let selected = hs !! (idx - 1)
        if rank selected == targetRank
            then do
                putStrLn "Correct rank! Card added to pile."
                let updatedTable = discardFromHand table playerIdx idx
                return updatedTable
            else do
                putStrLn "Wrong rank! You must draw one penalty card."
                let t1 = drawForHand table playerIdx
                return t1

determineWinner :: Table -> Player
determineWinner table =
    let ps = players table
    in minimumBy comparePlayers ps
  where
    comparePlayers p1 p2 =
        case compare (playerScore p1) (playerScore p2) of
            EQ -> compare (handCount p1) (handCount p2)
            other -> other
    handCount (Player _ (Hand hs)) = length hs
