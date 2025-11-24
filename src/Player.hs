module Player where

import Card

data Player = Player 
    {
        playerId :: Int,
        hand :: Hand
    } deriving (Show, Eq)

data Table = Table
    { players :: [Player]
    , drawDeck :: Deck
    , discardPile :: Deck
    , standings :: [(Int, Int)] -- (idPlayer, score)
    } deriving (Eq)

handScore :: Hand -> Int
handScore (Hand hs) = sum (map cardValue hs)

playerScore :: Player -> Int
playerScore player = handScore (hand player) 

instance Ord Player where
    compare p1 p2 =
        case compare (playerScore p1) (playerScore p2) of
            EQ -> compare (handSize p1) (handSize p2)
            other -> other
        where
            handSize p = let Hand hs = hand p in length hs

instance Show Table where
    show table =
        let [p1, p2, p3, p4] = players table
            row1 = showRow p1 p2
            row2 = showRow p3 p4
        in unlines
            [ "========================================================================"
            , "                                  Meja"
            , "========================================================================"
            , row1
            , ""
            , row2
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
    foldl (\currentTable _ -> dealRound currentTable) table ([1 .. 4] :: [Int])

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

-- tetapkan 4 pemain untuk saat ini agar mudah dites
initialTable' :: Table
initialTable' = Table 
    { players = [player1, player2, player3, player4]
    , drawDeck = buildDeck
    , discardPile = []
    , standings = [(1,0), (2,0), (3,0), (4,0)]
    }
-- =============================================================================

