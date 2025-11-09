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
    , standings :: [(Int, Int)] -- (idPlayer, score)
    } deriving (Eq)


instance Show Table where
    show table =
        let [p1, p2] = players table
            row = showRow p1 p2
        in unlines
            [ "========================================================================"
            , "                                  Meja"
            , "========================================================================"
            , row
            , "========================================================================"
            , "Dek Buangan: " ++ showTop (discardPile table)
            , "Dek tersisa: " ++ show (length (drawDeck table)) ++ " kartu"
            , "========================================================================"
            ]
      where
        showTop []    = "Kosong"
        showTop (x:_) = showCardRS x

        showPlayer :: Player -> String
        showPlayer p =
            let Hand hs = hand p
                masked  = unwords (replicate (length hs) "[()]")
            in "Pemain " ++ show (playerId p) ++ ": " ++ masked

        showRow :: Player -> Player -> String
        showRow left right = showPlayer left ++ replicate 6 ' ' ++ showPlayer right

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
    -- shuffle ini masih random, asumsi pure function gagal

dealToPlayer :: Card -> Player -> Player
dealToPlayer card player =
  let Hand hs = hand player
  in player { hand = Hand (card : hs) }

-- Kombinasi deal dan discard
playerDraw :: Deck -> Player -> (Player, Deck)
playerDraw deck player =
    let (drawnCard, newDeck) = drawCard deck
        newPlayer = dealToPlayer drawnCard player
    in (newPlayer, newDeck)

dealOneCard :: ([Player], Deck) -> Player -> ([Player], Deck)
dealOneCard (updatedPlayers, currentDeck) player =
    let (newPlayer, newDeck) = playerDraw currentDeck player
    in (newPlayer : updatedPlayers, newDeck)

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
-- Persiapan player, maks 4 pemain
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

-- tetapkan 2 pemain untuk saat ini agar mudah dites
table1 :: Table
table1 = Table 
    { players = [player1, player2]
    , drawDeck = buildDeck
    , discardPile = []
    , standings = [(1,0), (2,0)]
    }
-- =============================================================================

