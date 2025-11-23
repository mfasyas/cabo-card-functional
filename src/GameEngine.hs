module GameEngine where

import GameStates
import GameRules
import Card
import Player

import Data.List (splitAt)

updateGame :: GameState -> GameAction -> Either String GameState
updateGame gamestate action = do
    gameRules gamestate action            -- Rule Check
    return (applyAction gamestate action) -- Aplikasikan logic

applyAction :: GameState -> GameAction -> GameState
applyAction gamestate (DrawAction _)        = logicDraw gamestate
applyAction gamestate (DiscardAction _ idx) = logicDiscard gamestate idx
applyAction gamestate (TargetAction _ idx)  = logicResolve gamestate idx
applyAction gamestate _                     = gamestate

logicDraw :: GameState -> GameState
logicDraw gamestate =
    case drawDeck gamestate of
        []          -> gamestate { phase = GameOver, logs = logs gamestate ++ ["Deck habis! Permainan Berakhir."] }
        (card:rest) -> 
            let 
                player          = currentPlayer gamestate
                (Hand oldHand)  = hand player
                newHand         = Hand (card : oldHand)
                newPlayer       = player { hand = newHand }
                newPlayers      = replacePlayer (players gamestate) (currentTurn gamestate) newPlayer
            in gamestate 
                { drawDeck = rest
                , players  = newPlayers
                , phase    = DiscardPhase
                , logs     = logs gamestate ++ ["Pemain " ++ show (playerId player) ++ " mengambil kartu."]
                }

logicDiscard :: GameState -> Int -> GameState
logicDiscard gamestate idx =
    let 
        player              = currentPlayer gamestate
        (Hand hands)        = hand player
        (card, newHand)     = removeAt idx hands
        newPlayer           = player { hand = Hand newHand }
        newPlayers          = replacePlayer (players gamestate) (currentTurn gamestate) newPlayer
        newPile             = card : (discardPile gamestate)
        
        nextStateBase       = gamestate { players = newPlayers, discardPile = newPile }
        
    in case (powerup card) of
        Normal     -> endTurn nextStateBase ("Membuang " ++ show card)
        powerType  -> nextStateBase 
            { phase = ResolvePowerup powerType
            , logs = logs gamestate ++ ["Powerup " ++ show powerType ++ " aktif! Pilih target."]
            }

-- Logic Powerup (Sederhana: Peek Self)
logicResolve :: GameState -> (Int, Int) -> GameState
logicResolve gamestate (pIndex1, pIndex2) =
    case phase gamestate of
        ResolvePowerup powerType -> applyPowerupLogic gamestate powerType (pIndex1, pIndex2)
        _ -> gamestate 

applyPowerupLogic :: GameState -> Powerup -> (Int, Int) -> GameState

-- 1. PEEK SELF: Mengintip 1 kartu di tangan sendiri
applyPowerupLogic gamestate PeekSelf (idx, _) =
    let 
        player      = currentPlayer gamestate
        card        = getCardAt (hand player) idx
        msg         = "Kartu: " ++ showCardRS card
    in (endTurn gamestate "Menggunakan PeekSelf") { privateInfo = [(playerId player, msg)] }

-- 2. PEEK OPPONENT: Mengintip 1 kartu di tangan lawan
applyPowerupLogic gamestate PeekOpponent (_, idx) =
    let 
        player      = currentPlayer gamestate
        opp         = getOpponent gamestate
        card        = getCardAt (hand opp) idx
        msg         = "Kartu lawan: " ++ showCardRS card
    in (endTurn gamestate "Menggunakan PeekOpponent") { privateInfo = [(playerId player, msg)] }

-- 3. PEEK SO (Self & Opponent): Intip 1 punya sendiri, 1 punya lawan
applyPowerupLogic gamestate PeekSO (myIdx, oppIdx) =
    let 
        player      = currentPlayer gamestate
        opp         = getOpponent gamestate
        myCard      = getCardAt (hand player) myIdx
        oppCard     = getCardAt (hand opp) oppIdx
        msg         = "PeekSO -> Saya: " ++ showCardRS myCard ++ " | Lawan: " ++ showCardRS oppCard
    in (endTurn gamestate "Menggunakan PeekSO") { privateInfo = [(playerId player, msg)] }

-- 4. SWITCH: Tukar 1 kartu di tangan sendiri dengan kartu lawan.
applyPowerupLogic gamestate Switch (myIdx, oppIdx) =
    let 
        newState = executeSwap gamestate myIdx oppIdx
    in endTurn newState "Melakukan SWITCH kartu dengan lawan"

-- 5. PEEK SWITCH: Intip dulu, baru tukar
applyPowerupLogic gamestate PeekSwitch (myIdx, oppIdx) =
    let 
        player      = currentPlayer gamestate
        opp         = getOpponent gamestate
        myCard      = getCardAt (hand player) myIdx
        oppCard     = getCardAt (hand opp) oppIdx
        msg         = "PeekSwitch (Sebelum Tukar) -> Saya: " ++ showCardRS myCard ++ " | Lawan: " ++ showCardRS oppCard
        
        swappedState = executeSwap gamestate myIdx oppIdx
        
    in (endTurn swappedState "Melakukan Peek & Switch") { privateInfo = [(playerId player, msg)] }

-- 6. PEEK DOUBLE: Intip 2 kartu sendiri dan 2 kartu lawan
-- Subject to change, index yang diintip sama. Perlu diupdate lagi nanti.
applyPowerupLogic gamestate PeekDouble (idx1, idx2) =
    let 
        player      = currentPlayer gamestate
        opp         = getOpponent gamestate
        c1          = getCardAt (hand opp) idx1
        c2          = getCardAt (hand opp) idx2
        c1own       = getCardAt (hand player) idx1
        c2own       = getCardAt (hand player) idx2
        msg         = "PeekDouble Lawan -> Kartu 1: " ++ showCardRS c1 ++ ", Kartu 2: " ++ showCardRS c2 ++ "| Sendiri -> Kartu 1:" ++ showCardRS c1own ++ ", Kartu 2: " ++ showCardRS c2own
    in (endTurn gamestate "Menggunakan PeekDouble") { privateInfo = [(playerId player, msg)] }

-- Fallback jika input index ngaco
applyPowerupLogic gamestate _ _ = endTurn gamestate "Gagal memproses Powerup (Invalid Input)"

-- Fungsi helper
endTurn :: GameState -> String -> GameState
endTurn gamestate reason =
    let nextPlayer = (currentTurn gamestate + 1) `mod` (length (players gamestate))
    in gamestate 
        { currentTurn = nextPlayer
        , phase = DrawPhase
        , logs = logs gamestate ++ [reason, "Giliran Pemain Berikutnya."]
        , privateInfo = [] 
        }

-- Memperbarui state pemain
replacePlayer :: [Player] -> Int -> Player -> [Player]
replacePlayer list idx newPlayer =
    let (before, _:after) = splitAt idx list
    in before ++ [newPlayer] ++ after

-- Memisahkan kartu dari posisi tertentu
removeAt :: Int -> [a] -> (a, [a])
removeAt idx hands = (hands !! idx, take idx hands ++ drop (idx + 1) hands)

-- Mengambil kartu dari tangan
getCardAt :: Hand -> Int -> Card
getCardAt (Hand cards) idx 
    | idx >= 0 && idx < length cards = cards !! idx
    | otherwise = Card Joker Red Normal -- Fallback dummy jika error

-- Utilitas umum list replacement
replaceListIndex :: [a] -> Int -> a -> [a]
replaceListIndex list idx newVal =
    take idx list ++ [newVal] ++ drop (idx + 1) list

-- Logika Tukar Kartu (Complex State Mutation)
executeSwap :: GameState -> Int -> Int -> GameState
executeSwap gs myIdx oppIdx =
    let
        -- 1. Ambil data player
        meIdx = currentTurn gs
        oppIdxGlobal = (meIdx + 1) `mod` length (players gs)
        
        pMe = (players gs) !! meIdx
        pOpp = (players gs) !! oppIdxGlobal
        
        (Hand hMe) = hand pMe
        (Hand hOpp) = hand pOpp
        
        -- 2. Ambil Kartu yang mau ditukar
        cardMine = hMe !! myIdx
        cardTheirs = hOpp !! oppIdx
        
        -- 3. Buat Hand Baru (Replace at index)
        newHMe = replaceListIndex hMe myIdx cardTheirs
        newHOpp = replaceListIndex hOpp oppIdx cardMine
        
        -- 4. Update Player Struct
        pMeNew = pMe { hand = Hand newHMe }
        pOppNew = pOpp { hand = Hand newHOpp }
        
        -- 5. Masukkan kembali ke daftar players
        ps1 = replaceListIndex (players gs) meIdx pMeNew
        psFinal = replaceListIndex ps1 oppIdxGlobal pOppNew
        
    in gs { players = psFinal }