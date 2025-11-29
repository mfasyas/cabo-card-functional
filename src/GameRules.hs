module GameRules where

import GameStates
import Player
import Card

type Rule = GameState -> GameAction -> Either String ()

-- Helper ekstraksi Player ID dari Action
getActionPlayerId :: GameAction -> Int
getActionPlayerId (DrawAction p)          = p
getActionPlayerId (DiscardAction p _)     = p
getActionPlayerId (TargetAction p _)      = p
getActionPlayerId (KabulAction p)         = p
getActionPlayerId (TimpaAction p _)       = p
getActionPlayerId (PassTimpaAction p)     = p
getActionPlayerId (SkipPowerupAction p)   = p

-- 1. Turn Check
-- Dimodifikasi: Untuk Phase TimpaPhase, yang boleh gerak adalah `askingIdx`, bukan `currentTurn`.
isPlayerTurn :: Rule
isPlayerTurn gamestate action = 
    let actorId = getActionPlayerId action
    in case phase gamestate of
        TimpaPhase _ _ asker _ -> 
            if actorId == asker then Right () else Left $ "Menunggu respon Timpa dari Pemain " ++ show asker
        _ -> 
            if actorId == currentTurn gamestate then Right () else Left "Bukan giliran Anda!"

-- 2. Phase Check
isPhaseCorrect :: Rule
isPhaseCorrect gamestate action =
    case (phase gamestate, action) of
        (DrawPhase, DrawAction _)             -> Right ()
        (DrawPhase, KabulAction _)            -> Right () -- Kabul diperbolehkan di awal giliran (pengganti Draw)
        
        (DiscardPhase, DiscardAction _ _)     -> Right ()
        
        (ResolvePowerup _, TargetAction _ _)  -> Right ()
        (ResolvePowerup _, SkipPowerupAction _) -> Right () -- Fitur 5: Skip Powerup
        
        (TimpaPhase _ _ _ _, TimpaAction _ _) -> Right ()
        (TimpaPhase _ _ _ _, PassTimpaAction _) -> Right ()
        
        (GameOver, _)                         -> Left "Permainan Berakhir."
        _                                     -> Left "Aksi tidak valid di fase ini."

-- 3. Index Check
isValidIndex :: Rule
isValidIndex gamestate (DiscardAction _ idx) = checkHandIndex (currentPlayer gamestate) idx
isValidIndex gamestate (TimpaAction pid idx) = 
    -- Cek tangan pemain yang melakukan Timpa
    let p = (players gamestate) !! pid
    in checkHandIndex p idx

isValidIndex gamestate (TargetAction _ (idx1, idx2)) = do
    checkHandIndex (currentPlayer gamestate) idx1
    -- idx2 lawan tidak dicek ketat karena logic powerup bervariasi
    Right ()
isValidIndex _ _ = Right ()

checkHandIndex :: Player -> Int -> Either String ()
checkHandIndex player idx = 
    let (Hand hands) = hand player
    in if idx >= 0 && idx < length hands
       then Right ()
       else Left $ "Index kartu " ++ show idx ++ " tidak valid."

-- Combinator
infixl 0 .&&.
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) gamestate action = r1 gamestate action >> r2 gamestate action

gameRules :: Rule
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex