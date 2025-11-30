{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Player where

import Card 
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Player = Player 
    { playerId :: Int
    , hand :: Hand
    , matchPoints :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

handScore :: Hand -> Int
handScore (Hand hands) = sum (map cardValue hands)

updatePlayerScore :: Player -> Player
updatePlayerScore player' = player' { matchPoints = handScore (hand player')}

emptyHand :: Hand
emptyHand = Hand []

makePlayer :: Int -> Player
makePlayer pid = Player pid emptyHand 0

{-
A player is defined as a data that contains an ID as identifier, 
hand that is a list of cards, and the score of the player
-}