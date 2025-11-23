{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameStates where

import Card
import Player
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data GamePhase
    = DrawPhase
    | DiscardPhase
    | ResolvePowerup Powerup
    | GameOver
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
-- Game Phases

data GameAction
    = DrawAction Int
    | DiscardAction Int Int
    | TargetAction Int (Int, Int) -- di awal Int [Int] jaga-jaga lupa
    | FinishGameAction Int
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
-- Action Games
-- Bagian TargetAction menyimpan index player saat turn dan player setelahnya.

data GameState = GameState
    {
        players         :: [Player]
    ,   drawDeck        :: Deck
    ,   discardPile     :: Deck
    ,   currentTurn     :: Int
    ,   phase           :: GamePhase
    ,   logs            :: [String]
    ,   privateInfo     :: [(Int, String)]
    } deriving (Show, Generic, ToJSON, FromJSON)
-- Game States (Alternatif Table)
-- Ini digunakan agar kode lebih deklaratif dan harapannya akan k
--  lebih mudah untuk pembuatan UI/UX

-- Inisiasi state awal permainan
initialState :: Deck -> GameState
initialState deck = 
    let
        -- Gunakan parameter 'deck', bukan 'shuffledDeck'
        (p1Cards, rest1) = splitAt 4 deck
        (p2Cards, rest2) = splitAt 4 rest1
        (p3Cards, rest3) = splitAt 4 rest2
        (p4Cards, rest4) = splitAt 4 rest3
 
        drawDeckRest = rest4
        -- drawDeckRest = rest2

        p1 = (makePlayer 0) { hand = Hand p1Cards }
        p2 = (makePlayer 1) { hand = Hand p2Cards }
        p3 = (makePlayer 2) { hand = Hand p3Cards }
        p4 = (makePlayer 3) { hand = Hand p4Cards }
    in GameState 
    {
        players         = [p1, p2, p3, p4]
    ,   drawDeck        = drawDeckRest
    ,   discardPile     = []
    ,   currentTurn     = 0
    ,   phase           = DrawPhase
    ,   logs            = ["Permainan Dimulai."]
    ,   privateInfo     = []
    }

-- Helper untuk player aktif
currentPlayer :: GameState -> Player
currentPlayer gamestate = (players gamestate) !! (currentTurn gamestate)

-- Mengambil Object Lawan (Asumsi pemain berikutnya)
-- Subject to change
getOpponent :: GameState -> Player
getOpponent gs = 
    let myPid = currentTurn gs
        oppPid = (myPid + 1) `mod` length (players gs)
    in (players gs) !! oppPid