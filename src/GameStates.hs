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
    | TimpaPhase 
        { targetRank    :: Rank
        , originIdx     :: Int
        , askingIdx     :: Int
        , savedPowerup  :: Powerup
        }
    | ResolvePowerup Powerup
    | PostRoundDecision
    | GameOver
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
-- Game Phases

data GameAction
    = DrawAction Int
    | DiscardAction Int Int
    | TargetAction Int (Int, Int) -- di awal Int [Int] jaga-jaga lupa
    | KabulAction Int
    | TimpaAction Int Int
    | PassTimpaAction Int
    | SkipPowerupAction Int
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
    } deriving (Generic, ToJSON, FromJSON)
-- Game States (Alternatif Table)
-- Ini digunakan agar kode lebih deklaratif dan  
-- harapannya akan lebih mudah untuk pembuatan UI/UX

-- Inisiasi state awal permainan
initialState :: Deck -> GameState
initialState deck = 
    let

        (p1Cards, rest1) = splitAt 4 deck
        (p2Cards, rest2) = splitAt 4 rest1
        (p3Cards, rest3) = splitAt 4 rest2
        (p4Cards, rest4) = splitAt 4 rest3
 
        drawDeckRest = rest4

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

-- ==================================== Instances ============================================

instance Show GameState where
    show gamestate = 
        let
            playersList = players gamestate
            p1 = playersList !! 0
            p2 = playersList !! 1
            p3 = if length playersList > 2 then playersList !! 2 else p1 
            p4 = if length playersList > 3 then playersList !! 3 else p2 

            row1 = showRow p1 p2
            row2 = if length playersList > 2 then showRow p3 p4 else ""
            
            phaseMsg = case phase gamestate of
                TimpaPhase rank _ asker _ -> "Timpa Check! (Rank: " ++ show rank ++ ") -> Menunggu Pemain " ++ show asker
                p -> show p

        in unlines 
            [ "========================================================================"
            , "                                 Table"
            , "========================================================================"
            , row1
            , ""
            , row2
            , "========================================================================"
            , "Pile Deck: " ++ showTop (discardPile gamestate) 
            , "Draw Deck: " ++ show (length (drawDeck gamestate)) ++ " Cards"
            , "========================================================================"
            , "Giliran  : Pemain " ++ show (currentTurn gamestate)
            , "Fase     : " ++ phaseMsg
            ]
        where
            showTop []       = "[Kosong]"
            showTop (card:_) = show card
            showPlayer player =
                let Hand hands = hand player
                    masked = unwords (replicate (length hands) "[[]]")
                    scoreTxt = " (Pts: " ++ show (score player) ++ ")"
                in "Pemain " ++ show (playerId player + 1) ++ scoreTxt ++ ": " ++ masked
            showRow left right = showPlayer left ++ replicate 5 ' ' ++ showPlayer right