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
    where showCard idx card = show idx ++ ". " ++ showCardRS card

-- Interactive player turn
playerTurn :: Table -> Int -> IO Table
playerTurn table playerIdx = do
    -- Step 1: Draw card for player
    let tableAfterDraw = drawForHand table playerIdx
        currentPlayer = players tableAfterDraw !! playerIdx
        currentHand = hand currentPlayer

    -- Step 2: Show hand and get player choice
    putStrLn $ "\nPlayer " ++ show (playerId currentPlayer) ++ "'s turn"
    putStrLn "Your hand (with newly drawn card at position 1):"
    putStrLn $ showHandWithIndices currentHand

    -- Step 3: Get valid input
    choice <- getValidChoice (length $ let Hand h = currentHand in h)
    
    -- Step 4: Discard chosen card and return new table
    let finalTable = discardFromHand tableAfterDraw playerIdx choice
    putStrLn $ "Card " ++ show choice ++ " has been discarded."
    
    return finalTable

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