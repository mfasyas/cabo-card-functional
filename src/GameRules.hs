module GameRules where

import GameStates
import Player
import Card

type Rule = GameState -> GameAction -> Either String ()

getActor :: GameAction -> Int
getActor (DrawAction p)        = p
getActor (DiscardAction p _)   = p
getActor (TimpaAction p _)     = p
getActor (PassTimpaAction p)   = p
getActor (SkipPowerupAction p) = p
getActor (TargetAction p _ _)  = p
getActor (KabulAction p)       = p
getActor (FinishTurnAction p)  = p
getActor (InitPeekAction p _ _) = p

-- 1. TURN CHECK
isPlayerTurn :: Rule
isPlayerTurn gs action = 
    let actor = getActor action
    in case phase gs of
        InitialPeekPhase (nextP:_) ->
            if actor == nextP then Right () else Left $ "Tunggu! Giliran Pemain " ++ show nextP
        
        InitPeekFeedback p _ ->
            if actor == p then Right () else Left "Anda sedang melihat hasil intipan."

        TimpaRound _ _ asker _ -> 
            if actor == asker then Right () else Left $ "Giliran Pemain " ++ show asker ++ " (Timpa)."
            
        _ -> 
            if actor == currentTurn gs then Right () else Left "Bukan giliran Anda!"

-- 2. PHASE CHECK
isPhaseCorrect :: Rule
isPhaseCorrect gs action =
    case (phase gs, action) of
        (InitialPeekPhase _, InitPeekAction _ _ _) -> Right ()
        (InitPeekFeedback _ _, FinishTurnAction _) -> Right () -- Izin Finish saat Feedback
        
        (DrawPhase, DrawAction _)             -> Right ()
        (DiscardPhase, DiscardAction _ _)     -> Right ()
        
        (TimpaRound{}, TimpaAction _ _)       -> Right ()
        (TimpaRound{}, PassTimpaAction _)     -> Right ()
        
        (ResolvePowerup _, TargetAction _ _ _) -> Right ()
        (ResolvePowerup _, SkipPowerupAction _) -> Right ()
        
        (PostRoundDecision, KabulAction _)    -> Right ()
        (PostRoundDecision, FinishTurnAction _) -> Right ()
        
        (GameOver, _)                         -> Left "Permainan Berakhir."
        (p, a)                                -> Left $ "Aksi tidak valid di fase " ++ show p

-- 3. INDEX CHECK
isValidIndex :: Rule
isValidIndex gs (DiscardAction pid idx) = checkHandIdx gs pid idx
isValidIndex gs (TimpaAction pid idx)   = checkHandIdx gs pid idx
isValidIndex _ _ = Right ()

checkHandIdx :: GameState -> Int -> Int -> Either String ()
checkHandIdx gs pid idx =
    let p = (players gs) !! pid
        (Hand h) = hand p
    in if idx >= 0 && idx < length h then Right () else Left "Index kartu tidak valid."

infixl 0 .&&.
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) gs action = r1 gs action >> r2 gs action

gameRules :: Rule
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex