{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameStates where

import Card
import Player
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data GamePhase
    = InitialPeekPhase [Int]      -- Giliran input
    | InitPeekFeedback Int [Int]  -- FASE BARU: Giliran baca hasil (PlayerId, RemainingList)
    | DrawPhase
    | DiscardPhase
    | TimpaRound 
        { targetRank :: Rank
        , originIdx :: Int
        , askingIdx :: Int
        , savedPowerup :: Powerup
        }
    | ResolvePowerup Powerup
    | PostRoundDecision       
    | GameOver
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GameAction
    = DrawAction Int
    | DiscardAction Int Int
    | TimpaAction Int Int
    | PassTimpaAction Int
    | SkipPowerupAction Int
    | TargetAction Int Int [Int] 
    | KabulAction Int
    | FinishTurnAction Int
    | InitPeekAction Int Int Int 
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

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

initialState :: Deck -> GameState
initialState deck = 
    let
        (p1Cards, rest1) = splitAt 4 deck
        (p2Cards, rest2) = splitAt 4 rest1
        (p3Cards, rest3) = splitAt 4 rest2
        (p4Cards, rest4) = splitAt 4 rest3
 
        p1 = (makePlayer 0) { hand = Hand p1Cards }
        p2 = (makePlayer 1) { hand = Hand p2Cards }
        p3 = (makePlayer 2) { hand = Hand p3Cards }
        p4 = (makePlayer 3) { hand = Hand p4Cards }
    in GameState 
    {
        players         = [p1, p2, p3, p4]
    ,   drawDeck        = rest4
    ,   discardPile     = []
    ,   currentTurn     = 0 
    ,   phase           = InitialPeekPhase [0, 1, 2, 3] 
    ,   logs            = ["Permainan Dimulai. Fase Intip Awal."]
    ,   privateInfo     = []
    }

currentPlayer :: GameState -> Player
currentPlayer gs = (players gs) !! (currentTurn gs)

instance Show GameState where
    show gs = 
        let
            pList = players gs
            showP i = if i < length pList then showPlayer (pList !! i) else ""
            row1 = showP 0 ++ "      " ++ showP 1
            row2 = showP 2 ++ "      " ++ showP 3
            
            phaseMsg = case phase gs of
                InitialPeekPhase (p:_)    -> "INTIP AWAL: Giliran Pemain " ++ show p
                InitPeekFeedback p _      -> "INTIP AWAL: Hasil Pemain " ++ show p ++ " (Ketik 'finish' untuk lanjut)"
                TimpaRound r _ asker _    -> "TIMPA CHECK (Rank: " ++ show r ++ ") -> Menunggu P" ++ show asker
                PostRoundDecision         -> "AKHIR GILIRAN -> Ketik 'kabul' atau 'finish'"
                p -> show p

        in unlines 
            [ "=================== TABLE ==================="
            , row1
            , ""
            , row2
            , "============================================="
            , "Discard Pile: " ++ showTop (discardPile gs) 
            , "Draw Deck   : " ++ show (length (drawDeck gs))
            , "Phase       : " ++ phaseMsg
            , "============================================="
            ]
        where
            showTop [] = "[Empty]"
            showTop (c:_) = show c
            showPlayer p = 
                let (Hand h) = hand p
                    masked = unwords (replicate (length h) "[#]")
                in "P" ++ show (playerId p) ++ "(" ++ show (matchPoints p) ++ "pts): " ++ masked