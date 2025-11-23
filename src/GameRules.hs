module GameRules where

import GameStates
import Player
import Card

type Rule = GameState -> GameAction -> Either String ()

-- Turn Check, mengecek apa yang seharusnya terjadi.
isPlayerTurn :: Rule
isPlayerTurn gamestate action = 
    let player_id = case action of
            DrawAction p           -> p
            DiscardAction p _      -> p 
            TargetAction p _       -> p 
            FinishGameAction p     -> p

    in if player_id == playerId (currentPlayer gamestate)
       then Right ()
       else Left "Bukan giliran Anda!"

-- Phase Check, cek aksi apa yang seharusnya terjadi.
isPhaseCorrect :: Rule
isPhaseCorrect gamestate action =
    case (phase gamestate, action) of
        (DrawPhase, DrawAction _)             -> Right ()
        (DiscardPhase, DiscardAction _ _)     -> Right ()
        (ResolvePowerup _, TargetAction _ _)  -> Right ()
        (GameOver, _)                         -> Left "Permainan Berakhir."
        _                                     -> Left "Aksi tidak valid di fase ini."

-- Index Check, mengecek keberadaan kartu di tangan suatu player.
isValidIndex :: Rule
isValidIndex gamestate (DiscardAction _ idx) = checkIndex gamestate idx
isValidIndex gamestate (TargetAction _ (playerIdx1, playerIdx2)) = do
    checkIndex gamestate playerIdx1
    checkOpponentIndex gamestate playerIdx2
isValidIndex _ _ = Right ()

checkIndex :: GameState -> Int -> Either String ()
checkIndex gamestate idx = 
    let (Hand hands) = hand (currentPlayer gamestate)
    in if idx >= 0 && idx < length hands
       then Right ()
       else Left $ "Index kartu " ++ show idx ++ " tidak valid."

checkOpponentIndex :: GameState -> Int -> Either String ()
checkOpponentIndex gamestate idx =
    let 
        opp = getOpponent gamestate
        (Hand h) = hand opp
    in if idx >= 0 && idx < length h
       then Right ()
       else Left $ "Index kartu lawan " ++ show idx ++ " tidak valid."

-- Combinator
infixl 0 .&&.
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) gamestate action = r1 gamestate action >> r2 gamestate action

-- HAPUS isTargetCountValid dari sini
gameRules :: Rule
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex