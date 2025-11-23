module GameRules where

import GameStates
import Player
import Card

type Rule = GameState -> GameAction -> Either String ()

-- 1. Cek Giliran
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

-- 2. Cek Fase (PERBAIKAN: DrawPhase & DiscardPhase tidak pakai underscore '_')
isPhaseCorrect :: Rule
isPhaseCorrect gamestate action =
    case (phase gamestate, action) of
        (DrawPhase, DrawAction _)             -> Right ()
        (DiscardPhase, DiscardAction _ _)     -> Right ()
        (ResolvePowerup _, TargetAction _ _)  -> Right ()
        (GameOver, _)                         -> Left "Permainan Berakhir."
        _                                     -> Left "Aksi tidak valid di fase ini."

-- 3. Cek Index Kartu
isValidIndex :: Rule
isValidIndex gamestate (DiscardAction _ idx) = checkIndex gamestate idx
-- TargetAction membawa List [Int], jadi kita cek satu-satu pakai mapM_
isValidIndex gamestate (TargetAction _ indices) = mapM_ (checkIndex gamestate) indices
isValidIndex _ _ = Right ()

checkIndex :: GameState -> Int -> Either String ()
checkIndex gamestate idx = 
    let (Hand hands) = hand (currentPlayer gamestate)
    in if idx >= 0 && idx < length hands
       then Right ()
       else Left $ "Index kartu " ++ show idx ++ " tidak valid."

-- 4. Cek Jumlah Target Powerup
isTargetCountValid :: Rule
isTargetCountValid gs (TargetAction _ indices) =
    case phase gs of
        ResolvePowerup p -> 
            let required = requiredTargets p
            in if length indices == required
               then Right ()
               else Left $ "Powerup ini butuh " ++ show required ++ " target, tapi menerima " ++ show (length indices)
        _ -> Right () -- Skip jika bukan fase resolve
isTargetCountValid _ _ = Right ()

-- Helper jumlah target
requiredTargets :: Powerup -> Int
requiredTargets PeekSelf     = 1
requiredTargets PeekOpponent = 1
requiredTargets PeekSO       = 2
requiredTargets Switch       = 2
requiredTargets PeekSwitch   = 2
requiredTargets PeekDouble   = 2
requiredTargets _            = 0

-- Combinator
infixl 0 .&&.
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) gamestate action = r1 gamestate action >> r2 gamestate action

gameRules :: Rule
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex .&&. isTargetCountValid