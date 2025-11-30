module GameRules where

import GameStates
import Player
import Card

type Rule = GameState -> GameAction -> Either String ()

-- Pattern Mathing getting playerId for actions.
-- Needed because of different types of actions
getActor :: GameAction -> Int
getActor (DrawAction playerId)         = playerId
getActor (DiscardAction playerId _)    = playerId
getActor (TimpaAction playerId _)      = playerId
getActor (PassTimpaAction playerId)    = playerId
getActor (SkipPowerupAction playerId)  = playerId
getActor (TargetAction playerId _ _)   = playerId
getActor (KabulAction playerId)        = playerId
getActor (FinishTurnAction playerId)   = playerId
getActor (InitPeekAction playerId _ _) = playerId

-- 1. TURN CHECK
isPlayerTurn :: Rule
isPlayerTurn ofGameState action = 
    let actor = getActor action
    in case phase ofGameState of
        InitialPeekPhase (nextP:_) ->
            if actor == nextP then Right () else Left $ "Wait! Current player " ++ show nextP
        
        InitPeekFeedback p _ ->
            if actor == p then Right () else Left "You are looking at cards spied on."

        TimpaRound _ _ asker _ -> 
            if actor == asker then Right () else Left $ "Player in turn " ++ show asker ++ " (stack)."
            
        _ -> 
            if actor == currentTurn ofGameState then Right () else Left "Not your Turn!"

-- 2. PHASE CHECK
isPhaseCorrect :: Rule
isPhaseCorrect ofGameState action =
    case (phase ofGameState, action) of
        -- Peek phase control
        (InitialPeekPhase _, InitPeekAction _ _ _) -> Right ()
        (InitPeekFeedback _ _, FinishTurnAction _) -> Right () 
        
        -- Draw & Discard phase control
        (DrawPhase, DrawAction _)             -> Right ()
        (DiscardPhase, DiscardAction _ _)     -> Right ()
        
        -- Stack control
        (TimpaRound{}, TimpaAction _ _)       -> Right ()
        (TimpaRound{}, PassTimpaAction _)     -> Right ()
        
        -- Trigger powerup control
        (ResolvePowerup _, TargetAction _ _ _) -> Right ()
        (ResolvePowerup _, SkipPowerupAction _) -> Right ()
        
        -- Kabul and Finish turn control
        (PostRoundDecision, KabulAction _)    -> Right ()
        (PostRoundDecision, FinishTurnAction _) -> Right ()
        
        -- End entire game
        (GameOver, _)                         -> Left "Game Over"
        (p, a)                                -> Left $ "Action not valid in phase " ++ show p

-- 3. INDEX CHECK
isValidIndex :: Rule
isValidIndex ofGameState (DiscardAction pid idx) = checkHandIdx ofGameState pid idx
isValidIndex ofGameState (TimpaAction pid idx)   = checkHandIdx ofGameState pid idx
isValidIndex _ _ = Right ()

checkHandIdx :: GameState -> Int -> Int -> Either String ()
checkHandIdx ofGameState pid idx =
    let p = (players ofGameState) !! pid
        (Hand h) = hand p
    in if idx >= 0 && idx < length h then Right () else Left "Card index not valid."

infixl 0 .&&.
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) ofGameState action = r1 ofGameState action >> r2 ofGameState action

-- 4. RULE CHECK
-- If one of the rule Fails, go to the error case.
gameRules :: Rule
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex