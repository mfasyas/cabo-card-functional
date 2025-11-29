module GameEngine where

import GameStates
import GameRules
import Card
import Player
import Data.List (sortBy, splitAt)
import Data.Function (on)

updateGame :: GameState -> GameAction -> Either String GameState
updateGame gs action = do
    gameRules gs action           
    applyAction gs action         

applyAction :: GameState -> GameAction -> Either String GameState
applyAction gs (InitPeekAction p i1 i2) = logicInitPeek gs p i1 i2
applyAction gs (DrawAction _)          = logicDraw gs
applyAction gs (DiscardAction _ idx)   = logicDiscard gs idx
applyAction gs (TimpaAction pid idx)   = logicTimpa gs pid idx
applyAction gs (PassTimpaAction _)     = logicPassTimpa gs
applyAction gs (SkipPowerupAction _)   = logicSkipPowerup gs
applyAction gs (KabulAction pid)       = logicKabul gs pid
applyAction gs (FinishTurnAction _)    = logicFinishTurn gs
applyAction gs (TargetAction _ t idcs) = logicResolve gs t idcs

-- ================= LOGIC INIT PEEK =================
logicInitPeek :: GameState -> Int -> Int -> Int -> Either String GameState
logicInitPeek gs pid i1 i2 =
    case phase gs of
        InitialPeekPhase (current:rest) ->
            if pid /= current then Left "Bukan giliranmu intip."
            else 
                let 
                    p = (players gs) !! pid
                    c1 = safeGetCard p i1
                    c2 = safeGetCard p i2
                    msg = "Intip Awal (P"++show pid++"): ["++show i1++"] " ++ c1 ++ " & ["++show i2++"] " ++ c2
                in Right $ gs 
                    { phase = InitPeekFeedback pid rest -- JANGAN langsung lanjut, tunggu finish
                    , logs = logs gs ++ ["Pemain " ++ show pid ++ " mengintip kartu."]
                    , privateInfo = [(pid, msg)]
                    }
        _ -> Left "Bukan fase Initial Peek."

-- ================= LOGIC FINISH TURN =================
logicFinishTurn :: GameState -> Either String GameState
logicFinishTurn gs = 
    case phase gs of
        -- Handling khusus InitPeek
        InitPeekFeedback _ rest ->
            let nextPhase = if null rest then DrawPhase else InitialPeekPhase rest
            in Right $ gs 
                { phase = nextPhase
                , privateInfo = [] -- Hapus info setelah finish
                }
        
        -- Handling Normal (PostRoundDecision)
        PostRoundDecision ->
            let nextP = (currentTurn gs + 1) `mod` length (players gs)
            in Right $ gs 
                { currentTurn = nextP
                , phase = DrawPhase
                , logs = logs gs ++ ["Giliran selesai."]
                , privateInfo = [] 
                }
        
        _ -> Left "Tidak bisa finish sekarang."

-- ... (SISA KODE SAMA SEPERTI SEBELUMNYA: Draw, Discard, Timpa, dll) ...
-- Pastikan function Helper & Logic lain (logicDraw, logicDiscard, dll) tetap ada.

-- (Copy paste sisa logicDraw, logicDiscard, logicTimpa, applyPenaltyLogic, logicPassTimpa, processPostTimpa, logicSkipPowerup, logicResolve, applyPowerupLogic, logicKabul, safeGetCard, replacePlayer, removeAt, executeSwap, replaceListIndex, applyRankingPoints, applyPenaltyPoints, lookupPt dari respon sebelumnya)

-- START COPY HELPER DAN LOGIC LAIN
logicDraw :: GameState -> Either String GameState
logicDraw gs =
    case drawDeck gs of
        [] -> Right $ gs { phase = GameOver, logs = logs gs ++ ["Deck Habis!"] }
        (c:rest) -> 
            let p = currentPlayer gs
                (Hand oldH) = hand p
                newP = p { hand = Hand (c : oldH) }
                newPs = replacePlayer (players gs) (currentTurn gs) newP
            in Right $ gs 
                { drawDeck = rest
                , players = newPs
                , phase = DiscardPhase
                , logs = logs gs ++ ["Draw kartu baru."] 
                }

logicDiscard :: GameState -> Int -> Either String GameState
logicDiscard gs idx =
    let p = currentPlayer gs
        (Hand h) = hand p
        (c, newH) = removeAt idx h
        newP = p { hand = Hand newH }
        newPs = replacePlayer (players gs) (currentTurn gs) newP
        newPile = c : (discardPile gs)
        nextPhase = TimpaRound 
            { targetRank = rank c
            , originIdx = currentTurn gs
            , askingIdx = currentTurn gs
            , savedPowerup = powerup c
            }
    in Right $ gs { players = newPs, discardPile = newPile, phase = nextPhase, logs = logs gs ++ ["Discard " ++ show c] }

logicTimpa :: GameState -> Int -> Int -> Either String GameState
logicTimpa gs pid idx =
    case phase gs of
        TimpaRound targetR origin asking savedP ->
            let p = (players gs) !! pid
                (Hand h) = hand p
                c = h !! idx
            in if rank c /= targetR
               then applyPenaltyLogic gs pid ("Salah Timpa! Penalti.") targetR origin asking savedP
               else 
                   let (disc, newH) = removeAt idx h
                       newP = p { hand = Hand newH }
                       newPs = replacePlayer (players gs) pid newP
                       newPile = disc : (discardPile gs)
                   in processPostTimpa (gs { players = newPs, discardPile = newPile }) savedP ("Timpa Sukses P" ++ show pid)
        _ -> Left "Error Timpa"

applyPenaltyLogic :: GameState -> Int -> String -> Rank -> Int -> Int -> Powerup -> Either String GameState
applyPenaltyLogic gs pid reason r origin asking savedP =
    case drawDeck gs of
        [] -> Right $ gs { phase = GameOver }
        (card:rest) ->
            let p = (players gs) !! pid
                (Hand oldH) = hand p
                newP = p { hand = Hand (card : oldH) }
                newPs = replacePlayer (players gs) pid newP
                nextAsking = (asking + 1) `mod` length (players gs)
                newState = gs { players = newPs, drawDeck = rest, logs = logs gs ++ [reason] }
            in if nextAsking == origin
               then processPostTimpa newState savedP "Fase Timpa Selesai."
               else Right $ newState { phase = TimpaRound r origin nextAsking savedP }

logicPassTimpa :: GameState -> Either String GameState
logicPassTimpa gs =
    case phase gs of
        TimpaRound r origin asking savedP ->
            let nextAsking = (asking + 1) `mod` length (players gs)
            in if nextAsking == origin
               then processPostTimpa gs savedP "Timpa selesai."
               else Right $ gs { phase = TimpaRound r origin nextAsking savedP }
        _ -> Left "Error"

processPostTimpa :: GameState -> Powerup -> String -> Either String GameState
processPostTimpa gs pType msg =
    let gsLog = gs { logs = logs gs ++ [msg] }
    in case pType of
        Normal -> Right $ gsLog { phase = PostRoundDecision, logs = logs gsLog ++ ["Ketik 'kabul' atau 'finish'."] }
        pt     -> Right $ gsLog { phase = ResolvePowerup pt, logs = logs gsLog ++ ["Powerup " ++ show pt ++ "!"] }

logicSkipPowerup :: GameState -> Either String GameState
logicSkipPowerup gs = Right $ gs { phase = PostRoundDecision, logs = logs gs ++ ["Skip Powerup."] }

logicResolve :: GameState -> Int -> [Int] -> Either String GameState
logicResolve gs t idcs =
    case phase gs of
        ResolvePowerup pt -> 
            case applyPowerupLogic gs pt t idcs of
                Left e -> Left e
                Right (ns, msg) -> Right $ ns { phase = PostRoundDecision, privateInfo = [(currentTurn gs, msg)] }
        _ -> Left "Bukan Powerup"

applyPowerupLogic :: GameState -> Powerup -> Int -> [Int] -> Either String (GameState, String)
applyPowerupLogic gs PeekSelf t [i] = if t/=currentTurn gs then Left "Target Salah" else Right (gs, "PeekSelf: " ++ safeGetCard ((players gs)!!t) i)
applyPowerupLogic gs PeekOpponent t [i] = if t==currentTurn gs then Left "Target Salah" else Right (gs, "PeekOpponent: " ++ safeGetCard ((players gs)!!t) i)
applyPowerupLogic gs PeekDouble t [i1, i2] = 
    if t==currentTurn gs then Left "Target Salah" else 
    let me=(players gs)!!(currentTurn gs); opp=(players gs)!!t 
    in Right (gs, "SAYA: "++safeGetCard me i1++","++safeGetCard me i2++" | LAWAN: "++safeGetCard opp i1++","++safeGetCard opp i2)
applyPowerupLogic gs Switch t [i1, i2] = if t==currentTurn gs then Left "Target Salah" else Right (executeSwap gs (currentTurn gs) i1 t i2, "Switch Done")
applyPowerupLogic _ _ _ _ = Left "Format Salah"

logicKabul :: GameState -> Int -> Either String GameState
logicKabul gs cid = 
    let allP=players gs; caller=allP!!cid; cScore=handScore(hand caller)
        minOther=minimum(map (handScore.hand) (filter (\p->playerId p/=cid) allP))
        finalGS=if cScore<=minOther then applyRankingPoints gs else applyPenaltyPoints gs cid
    in Right $ finalGS { phase = GameOver }

safeGetCard :: Player -> Int -> String
safeGetCard p idx = let (Hand h)=hand p in if idx>=0 && idx<length h then show (h!!idx) else "Out Of Index"

replacePlayer l i n = let (b,_:a)=splitAt i l in b++[n]++a
removeAt i xs = (xs!!i, take i xs ++ drop (i+1) xs)
replaceListIndex l i v = take i l ++ [v] ++ drop (i+1) l

executeSwap gs p1 i1 p2 i2 = 
    let pl=players gs; o1=pl!!p1; o2=pl!!p2; (Hand h1)=hand o1; (Hand h2)=hand o2
        c1=if i1<length h1 then h1!!i1 else Card Joker Red Normal
        c2=if i2<length h2 then h2!!i2 else Card Joker Red Normal
        n1=if i1<length h1 then replaceListIndex h1 i1 c2 else h1
        n2=if i2<length h2 then replaceListIndex h2 i2 c1 else h2
    in gs { players = replacePlayer (replacePlayer pl p1 (o1{hand=Hand n1})) p2 (o2{hand=Hand n2}) }

applyRankingPoints gs = let s=sortBy (compare `on` (handScore.hand)) (players gs) in gs { players = map (\p->p{matchPoints=matchPoints p + lookupPt (playerId p) (zip (map playerId s) [3,2,1,0])}) (players gs) }
applyPenaltyPoints gs bad = gs { players = map (\p->if playerId p==bad then p{matchPoints=matchPoints p-1} else p{matchPoints=matchPoints p+1}) (players gs) }
lookupPt i ((x,v):xs) = if i==x then v else lookupPt i xs; lookupPt _ [] = 0