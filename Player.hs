{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Player where
import Card
import System.Random (randomRIO)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Control.Monad (forM)

data Player = Player 
    {
        playerId :: Int,
        hand :: Hand,
        score :: Int
    } deriving (Show, Eq)

instance Ord Player where
    compare p1 p2 
        | score p1 == score p2 = compare (handSize p1) (handSize p2)
        | otherwise = compare (score p1) (score p2)
        where
            handSize p = let Hand hs = hand p in length hs

newtype Table = Table [Player]
    deriving (Show, Eq)

dealToPlayer :: Card -> Player -> Player
dealToPlayer card player =
  let Hand hs = hand player
  in player { hand = Hand (card : hs) }

-- =============================================================================
-- Player Test Data
player1 :: Player
player1 = Player {
    playerId = 1, 
    hand = Hand [Card Ace Hearts Normal, Card Ten Diamonds Normal, Card Jack  Clubs Normal, Card Queen Spades  Normal], 
    score = handScore (Hand [Card Ace Hearts Normal, Card Ten Diamonds Normal, Card Jack  Clubs Normal, Card Queen Spades  Normal])
}

player2 :: Player
player2 = Player
  { playerId = 2
  , hand = Hand
      [ Card Five  Hearts  Normal
      , Card Six   Diamonds Normal
      , Card Seven Clubs   Normal
      ]
  , score = handScore (Hand
      [ Card Five  Hearts  Normal
      , Card Six   Diamonds Normal
      , Card Seven Clubs   Normal
      ])
  }

player3 :: Player
player3 = Player
  { playerId = 3
  , hand = Hand
      [ Card Nine  Hearts  Normal
      , Card Ten   Diamonds Normal
      , Card Jack  Clubs   Normal
      , Card Queen Spades  Normal
      ]
  , score = handScore (Hand
      [ Card Nine  Hearts  Normal
      , Card Ten   Diamonds Normal
      , Card Jack  Clubs   Normal
      , Card Queen Spades  Normal
      ])
  }

player4 :: Player
player4 = Player
  { playerId = 4
  , hand = Hand
      [ Card King  Hearts  Normal
      , Card Ace   Diamonds Normal
      , Card Joker RedJoker  Normal
      , Card Joker BlackJoker Normal
      ]
  , score = handScore (Hand
      [ Card King  Hearts  Normal
      , Card Ace   Diamonds Normal
      , Card Joker RedJoker  Normal
      , Card Joker BlackJoker Normal
      ])
  }
-- =============================================================================
-- Table Test Data
table1 :: Table
table1 = Table [player1, player2, player3, player4]
