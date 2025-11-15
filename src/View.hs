{-# LANGUAGE DeriveGeneric #-}

module View where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON)

import Player (Table(..), Player(..), Hand(..))  -- sesuaikan dengan modulmu
import Card   (Card, cardValue, showCardRS)      -- sesuaikan yang ada

-- Data pemain untuk dikirim ke frontend
data PlayerView = PlayerView
  { pvId        :: Int
  , pvScore     :: Int
  , pvHandSize  :: Int
  , pvCardsText :: [String]   -- optional: representasi teks tiap kartu
  } deriving (Show, Generic)

instance ToJSON PlayerView

-- Data table untuk frontend
data TableView = TableView
  { tvPlayers    :: [PlayerView]
  , tvDeckCount  :: Int
  , tvDiscardTop :: Maybe String
  } deriving (Show, Generic)

instance ToJSON TableView

toPlayerView :: Player -> PlayerView
toPlayerView p =
  let Hand hs = hand p
  in PlayerView
       { pvId        = playerId p
       , pvScore     = playerScore p
       , pvHandSize  = length hs
       , pvCardsText = map showCardRS hs
       }

toTableView :: Table -> TableView
toTableView t =
  TableView
    { tvPlayers    = map toPlayerView (players t)
    , tvDeckCount  = length (drawDeck t)
    , tvDiscardTop =
        case discardPile t of
          []    -> Nothing
          (c:_) -> Just (showCardRS c)
    }
