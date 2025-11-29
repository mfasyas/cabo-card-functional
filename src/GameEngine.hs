module GameEngine where

import GameStates
import GameRules
import Card
import Player
import Data.List (sortBy, splitAt)
import Data.Function (on)

updateGame :: GameState -> GameAction -> Either String GameState
updateGame gamestate action = do
    gameRules gamestate action            
    return (applyAction gamestate action)

applyAction :: GameState -> GameAction -> GameState
applyAction gs (DrawAction _)          = logicDraw gs
applyAction gs (DiscardAction _ idx)   = logicDiscard gs idx
applyAction gs (KabulAction pid)       = logicKabul gs pid
applyAction gs (TimpaAction pid idx)   = logicTimpa gs pid idx
applyAction gs (PassTimpaAction _)     = logicPassTimpa gs
applyAction gs (SkipPowerupAction _)   = endTurn gs "Powerup di-skip."
applyAction gs (TargetAction _ idx)    = logicResolve gs idx

-- ================= LOGIC DRAW =================
logicDraw :: GameState -> GameState
logicDraw gs =
    case drawDeck gs of
        [] -> gs { phase = GameOver, logs = logs gs ++ ["Deck habis! Permainan Berakhir."] }
        (card:rest) -> 
            let 
                p = currentPlayer gs
                (Hand oldH) = hand p
                newP = p { hand = Hand (card : oldH) }
                newPs = replacePlayer (players gs) (currentTurn gs) newP
            in gs { drawDeck = rest, players = newPs, phase = DiscardPhase, logs = logs gs ++ ["Mengambil kartu."] }

-- ================= LOGIC DISCARD & TIMPA TRIGGER =================
logicDiscard :: GameState -> Int -> GameState
logicDiscard gs idx =
    let 
        p = currentPlayer gs
        (Hand hands) = hand p
        (card, newHandList) = removeAt idx hands
        newP = p { hand = Hand newHandList }
        newPs = replacePlayer (players gs) (currentTurn gs) newP
        newPile = card : (discardPile gs)
        
        -- State dasar setelah discard (belum ganti giliran)
        baseState = gs { players = newPs, discardPile = newPile }
        
        -- Persiapan masuk Fase Timpa
        -- Mulai tanya dari pemain yang membuang kartu (Fitur 4)
        pUp = powerup card
        nextPhase = TimpaPhase 
            { targetRank = rank card
            , originIdx = currentTurn gs
            , askingIdx = currentTurn gs 
            , savedPowerup = pUp
            }
            
    in baseState 
        { phase = nextPhase
        , logs = logs gs ++ ["Membuang " ++ show card ++ ". Cek Timpa..."]
        }

-- ================= LOGIC TIMPA =================
-- Jika pemain memilih "Timpa" (meletakkan kartu rank sama)
logicTimpa :: GameState -> Int -> Int -> GameState
logicTimpa gs pid idx =
    let
        targetP = (players gs) !! pid
        (Hand h) = hand targetP
        card = h !! idx
    in if rank card /= targetRank (unwrapTimpaPhase (phase gs))
       then gs -- Harusnya dicek di Rules, tapi fail-safe: abaikan jika rank beda
       else 
           let
                -- Buang kartu timpa
               (discarded, newH) = removeAt idx h
               newP = targetP { hand = Hand newH }
               newPs = replacePlayer (players gs) pid newP
               newPile = discarded : (discardPile gs)
               
               msg = "Pemain " ++ show pid ++ " melakukan TIMPA dengan " ++ show discarded
               
               -- "Fase timpa sudah selesai" -> Lanjut ke logika kartu ORIGINAL.
               -- Kartu timpa tidak trigger powerup (Fitur 4).
               -- Powerup original tetap berlaku (disimpan di savedPowerup)
               savedP = savedPowerup (unwrapTimpaPhase (phase gs))
           in processPostTimpa (gs { players = newPs, discardPile = newPile }) savedP msg

unwrapTimpaPhase :: GamePhase -> GamePhase
unwrapTimpaPhase p = p -- Helper safe cast

-- Jika pemain Pass saat ditanya Timpa
logicPassTimpa :: GameState -> GameState
logicPassTimpa gs =
    case phase gs of
        TimpaPhase r origin asking savedP ->
            let 
                totalP = length (players gs)
                nextAsking = (asking + 1) `mod` totalP
            in if nextAsking == origin 
               -- Jika sudah memutar balik ke pembuang awal -> Fase Timpa Selesai, tidak ada yang timpa.
               then processPostTimpa gs savedP "Fase Timpa Selesai (Tidak ada yang menimpa)."
               -- Tanya pemain berikutnya
               else gs { phase = TimpaPhase r origin nextAsking savedP }
        _ -> gs

-- Helper transisi setelah Fase Timpa selesai
processPostTimpa :: GameState -> Powerup -> String -> GameState
processPostTimpa gs powerType msg =
    let updatedLogs = logs gs ++ [msg]
        gsLog = gs { logs = updatedLogs }
    in case powerType of
        Normal -> endTurn gsLog "Giliran Selesai."
        pt     -> gsLog 
            { phase = ResolvePowerup pt
            , logs = updatedLogs ++ ["Powerup " ++ show pt ++ " aktif! Pilih target atau Skip."]
            }

-- ================= LOGIC KABUL (SCORING) =================
-- Fitur 1, 2, 3
logicKabul :: GameState -> Int -> GameState
logicKabul gs callerId =
    let
        allPlayers = players gs
        caller = allPlayers !! callerId
        callerScore = handScore (hand caller)
        
        -- Cek apakah caller punya nilai terkecil secara ketat?
        -- "Bukan pemilik kartu terkecil" -> Penalty. 
        -- Artinya ScoreCaller > min(Others) -> Penalty.
        otherScores = map (handScore . hand) (filter (\p -> playerId p /= callerId) allPlayers)
        minOther = minimum otherScores
        
        isLowest = callerScore <= minOther -- Jika seri dengan terkecil, kita anggap aman/menang.
        
        finalState = if isLowest 
            then applyRankingPoints gs -- Fitur 2
            else applyPenaltyPoints gs callerId -- Fitur 3
            
    in finalState { phase = GameOver, logs = logs finalState ++ ["KABUL dipanggil oleh Pemain " ++ show callerId] }

-- Fitur 2: 3, 2, 1, 0 poin berdasarkan urutan score
applyRankingPoints :: GameState -> GameState
applyRankingPoints gs =
    let 
        sortedPlayers = sortBy (compare `on` (handScore . hand)) (players gs)
        -- sortedPlayers :: [Player] (urut dari score terkecil)
        
        -- Zip dengan poin [3, 2, 1, 0, ...]
        pointsAllocation = zip (map playerId sortedPlayers) [3, 2, 1, 0]
        
        newPlayers = map (\p -> 
            let addPt = lookUpPoints (playerId p) pointsAllocation
            in p { score = score p + addPt }
            ) (players gs)
            
    in gs { players = newPlayers, logs = logs gs ++ ["Distribusi Poin Klasemen Berhasil."] }

-- Fitur 3: Penalty (-1 caller, +1 others)
applyPenaltyPoints :: GameState -> Int -> GameState
applyPenaltyPoints gs culpritId =
    let newPlayers = map (\p -> 
            if playerId p == culpritId 
            then p { score = score p - 1 }
            else p { score = score p + 1 }
            ) (players gs)
    in gs { players = newPlayers, logs = logs gs ++ ["Kabul Gagal! Penalti diberikan."] }

lookUpPoints :: Int -> [(Int, Int)] -> Int
lookUpPoints _ [] = 0
lookUpPoints pid ((id, pt):xs)
    | pid == id = pt
    | otherwise = lookUpPoints pid xs

-- ================= LOGIC POWERUP & UTILS =================
logicResolve :: GameState -> (Int, Int) -> GameState
logicResolve gamestate idx =
    case phase gamestate of
        ResolvePowerup powerType -> applyPowerupLogic gamestate powerType idx
        _ -> gamestate 

applyPowerupLogic :: GameState -> Powerup -> (Int, Int) -> GameState
applyPowerupLogic gs PeekSelf (idx, _) =
    let 
        p = currentPlayer gs
        c = getCardAt (hand p) idx
        msg = "Kartu: " ++ show c
    in (endTurn gs "PeekSelf Selesai") { privateInfo = [(playerId p, msg)] }

applyPowerupLogic gs PeekOpponent (_, idx) =
    let 
        p = currentPlayer gs
        opp = getOpponent gs
        c = getCardAt (hand opp) idx
        msg = "Kartu lawan: " ++ show c
    in (endTurn gs "PeekOpponent Selesai") { privateInfo = [(playerId p, msg)] }

applyPowerupLogic gs PeekSO (myIdx, oppIdx) =
    let 
        p = currentPlayer gs
        opp = getOpponent gs
        mc = getCardAt (hand p) myIdx
        oc = getCardAt (hand opp) oppIdx
        msg = "Saya: " ++ show mc ++ " | Lawan: " ++ show oc
    in (endTurn gs "PeekSO Selesai") { privateInfo = [(playerId p, msg)] }

applyPowerupLogic gs Switch (myIdx, oppIdx) =
    endTurn (executeSwap gs myIdx oppIdx) "Switch Selesai"

applyPowerupLogic gs PeekSwitch (myIdx, oppIdx) =
    let 
        p = currentPlayer gs
        opp = getOpponent gs
        mc = getCardAt (hand p) myIdx
        oc = getCardAt (hand opp) oppIdx
        msg = "Sebelum Tukar -> Saya: " ++ show mc ++ " | Lawan: " ++ show oc
        swapped = executeSwap gs myIdx oppIdx
    in (endTurn swapped "PeekSwitch Selesai") { privateInfo = [(playerId p, msg)] }

applyPowerupLogic gs PeekDouble (idx1, idx2) =
    let 
        p = currentPlayer gs
        opp = getOpponent gs
        c1 = getCardAt (hand opp) idx1
        c2 = getCardAt (hand opp) idx2
        mc1 = getCardAt (hand p) idx1
        mc2 = getCardAt (hand p) idx2
        msg = "Lawan: " ++ show c1 ++ ", " ++ show c2 ++ " | Saya: " ++ show mc1 ++ ", " ++ show mc2
    in (endTurn gs "PeekDouble Selesai") { privateInfo = [(playerId p, msg)] }

applyPowerupLogic gs _ _ = endTurn gs "Action"

endTurn :: GameState -> String -> GameState
endTurn gs reason =
    let nextPlayer = (currentTurn gs + 1) `mod` (length (players gs))
    in gs 
        { currentTurn = nextPlayer
        , phase = DrawPhase
        , logs = logs gs ++ [reason]
        , privateInfo = [] -- Reset private info
        }

replacePlayer :: [Player] -> Int -> Player -> [Player]
replacePlayer list idx newPlayer =
    let (before, _:after) = splitAt idx list
    in before ++ [newPlayer] ++ after

removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs = (xs !! idx, take idx xs ++ drop (idx + 1) xs)

getCardAt :: Hand -> Int -> Card
getCardAt (Hand cards) idx 
    | idx >= 0 && idx < length cards = cards !! idx
    | otherwise = Card Joker Red Normal 

replaceListIndex :: [a] -> Int -> a -> [a]
replaceListIndex list idx newVal =
    take idx list ++ [newVal] ++ drop (idx + 1) list

executeSwap :: GameState -> Int -> Int -> GameState
executeSwap gs myIdx oppIdx =
    let
        meIdx = currentTurn gs
        oppIdxGlobal = (meIdx + 1) `mod` length (players gs)
        pMe = (players gs) !! meIdx
        pOpp = (players gs) !! oppIdxGlobal
        (Hand hMe) = hand pMe
        (Hand hOpp) = hand pOpp
        
        safeMyIdx = min myIdx (length hMe - 1)
        safeOppIdx = min oppIdx (length hOpp - 1)
        
        cardMine = hMe !! safeMyIdx
        cardTheirs = hOpp !! safeOppIdx
        
        newHMe = replaceListIndex hMe safeMyIdx cardTheirs
        newHOpp = replaceListIndex hOpp safeOppIdx cardMine
        
        pMeNew = pMe { hand = Hand newHMe }
        pOppNew = pOpp { hand = Hand newHOpp }
        
        ps1 = replaceListIndex (players gs) meIdx pMeNew
        psFinal = replaceListIndex ps1 oppIdxGlobal pOppNew
    in gs { players = psFinal }