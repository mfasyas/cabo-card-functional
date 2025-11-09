module Actions where

import Card
import Player


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

-- Interactive player turn
playerTurn :: Table -> Int -> IO Table
playerTurn table playerIdx = do
    let currentPlayer = players table !! playerIdx
    putStrLn $ "\nPlayer " ++ show (playerId currentPlayer) ++ "'s turn"
    putStrLn "Do you want to end the game now? (y/n)"
    endChoice <- getLine
    if endChoice == "y"
        then do
            putStrLn $ "\nPlayer " ++ show (playerId currentPlayer) ++ " has ended the game!"
            -- Use a special marker: clear draw deck to trigger end in playRounds
            return table { drawDeck = [] }
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
