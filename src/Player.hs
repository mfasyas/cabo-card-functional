{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Player where

import Card 
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Player = Player 
    { playerId :: Int
    , hand :: Hand
    , score :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

handScore :: Hand -> Int
handScore (Hand hands) = sum (map cardValue hands)

updatePlayerScore :: Player -> Player
updatePlayerScore player' = player' { score = handScore (hand player')}

emptyHand :: Hand
emptyHand = Hand []

makePlayer :: Int -> Player
makePlayer pid = Player pid emptyHand 0