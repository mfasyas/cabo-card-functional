{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use forM_" #-}
module Player where
import Card
import System.Random (randomRIO)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Control.Monad (forM)
import Card (Deck)

data Player = Player 
    {
        playerId :: Int,
        hand :: Hand
    } deriving (Show, Eq)

playerScore :: Player -> Int
playerScore player = handScore (hand player) 

instance Ord Player where
    compare p1 p2 =
        case compare (playerScore p1) (playerScore p2) of
            EQ -> compare (handSize p1) (handSize p2)
            other -> other
        where
            handSize p = let Hand hs = hand p in length hs

data Table = Table
    { players :: [Player]
    , drawDeck :: Deck
    , discardPile :: Pile
    }

instance Show Table where
    show table =
        let [p1, p2, p3, p4] = players table
            row1 = showRow p1 p3
            row2 = showRow p2 p4
        in unlines
            [ "========================================================="
            , "                    Current Table View"
            , "========================================================="
            , row1
            , ""
            , row2
            , "========================================================="
            , "Discard Pile Top: " ++ showTop (discardPile table)
            , "Remaining Deck:   " ++ show (length (drawDeck table)) ++ " cards"
            , "========================================================="
            ]
      where
        showTop []    = "Empty"
        showTop (x:_) = showCardRS x

        showPlayer :: Player -> String
        showPlayer p =
            let Hand hs = hand p
                masked  = unwords (replicate (length hs) "[??]")
            in padRight 10 ("Player " ++ show (playerId p) ++ ":") ++ " " ++ masked

        showRow :: Player -> Player -> String
        showRow left right = showPlayer left ++ replicate 12 ' ' ++ showPlayer right

        padRight :: Int -> String -> String
        padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- =============================================================================
-- Player Functions

-- A Fisher-Yates shuffle function using your IO imports
shuffleDeck :: Deck -> IO Deck
shuffleDeck cards = do
    let len = length cards
    -- Create a mutable array from the card list
    arr <- newListArray (0, len - 1) cards :: IO (IOArray Int Card)
    
    -- Perform the shuffle
    forM [0 .. len - 2] $ \i -> do
        j <- randomRIO (i, len - 1) -- Get a random index
        vi <- readArray arr i        -- Swap elements
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi
    
    -- Convert the mutable array back to a pure list
    -- (The "elems" function from Data.Array does this, but this way works too)
    mapM (readArray arr) [0 .. len - 1]

dealToPlayer :: Card -> Player -> Player
dealToPlayer card player =
  let Hand hs = hand player
  in player { hand = Hand (card : hs) }

-- This function combines drawing and dealing
playerDraw :: Deck -> Player -> (Player, Deck)
playerDraw deck player =
    -- 1. Draw a card, getting the card and the new deck
    let (drawnCard, newDeck) = drawCard deck
    
    -- 2. Give that card to the player using your existing function
        newPlayer = dealToPlayer drawnCard player
        
    -- 3. Return the new player and the new deck
    in (newPlayer, newDeck)

dealOneCard :: ([Player], Deck) -> Player -> ([Player], Deck)
dealOneCard (updatedPlayers, currentDeck) player =
    let 
        (newPlayer, newDeck) = playerDraw currentDeck player
    in 
        (newPlayer : updatedPlayers, newDeck)

dealRound :: Table -> Table
dealRound table =
    let 
        initialPlayer = players table
        initialDeck = drawDeck table

        (reversedNewPlayers, finalDeck) = foldl dealOneCard ([], initialDeck) initialPlayer

        newPlayers = reverse reversedNewPlayers

    in
        table { players = newPlayers, drawDeck = finalDeck }

deal4CardsToEachPlayer :: Table -> Table
deal4CardsToEachPlayer table =
    foldl (\currentTable _ -> dealRound currentTable) table [1..4]

-- =============================================================================
-- Player Test Data
player1 :: Player
player1 = Player {
    playerId = 1, 
    hand = Hand []
}

player2 :: Player
player2 = Player
  { playerId = 2
  , hand = Hand []
  }

player3 :: Player
player3 = Player
  { playerId = 3
  , hand = Hand []
  }

player4 :: Player
player4 = Player
  { playerId = 4
  , hand = Hand []
  }
-- =============================================================================
-- Table Test Data
table1 :: Table
table1 = Table 
    { players = [player1, player2, player3, player4]
    , drawDeck = buildDeck
    , discardPile = []
    }

-- =============================================================================

