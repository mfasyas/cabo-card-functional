module GameEngine where

import GameStates
import GameRules
import Card
import Player
import Data.List (sortBy, splitAt)
import Data.Function (on)

updateGame :: GameState -> GameAction -> Either String GameState
updateGame ofGameState action = do
    gameRules ofGameState action      -- Rule Check | return empty if True | return error message if False   
    applyAction ofGameState action    -- Case of True, apply the action.

-- Pattern matching of GameAction
applyAction :: GameState -> GameAction -> Either String GameState
applyAction ofGameState (InitPeekAction p i1 i2) = logicInitPeek ofGameState p i1 i2
applyAction ofGameState (DrawAction _)           = logicDraw ofGameState
applyAction ofGameState (DiscardAction _ idx)    = logicDiscard ofGameState idx
applyAction ofGameState (TimpaAction pid idx)    = logicTimpa ofGameState pid idx
applyAction ofGameState (PassTimpaAction _)      = logicPassTimpa ofGameState
applyAction ofGameState (SkipPowerupAction _)    = logicSkipPowerup ofGameState
applyAction ofGameState (KabulAction pid)        = logicKabul ofGameState pid
applyAction ofGameState (FinishTurnAction _)     = logicFinishTurn ofGameState
applyAction ofGameState (TargetAction _ t idcs)  = logicResolve ofGameState t idcs

-- ================= LOGIC INIT PEEK =================
logicInitPeek :: GameState -> Int -> Int -> Int -> Either String GameState
logicInitPeek ofGameState pid i1 i2 =
    case phase ofGameState of
        InitialPeekPhase (current:rest) ->
            if pid /= current then Left "Not your turn to peek."
            else 
                let 
                    p   = (players ofGameState) !! pid
                    c1  = safeGetCard p i1
                    c2  = safeGetCard p i2
                    msg = "Inital Peek (P" ++ show pid ++"): ["++show i1 ++ "] " ++ c1 ++ " & [" ++ show i2 ++ "] " ++ c2
                in Right $ ofGameState 
                    { phase         = InitPeekFeedback pid rest 
                    , logs          = logs ofGameState ++ ["Player " ++ show pid ++ " peeking card."]
                    , privateInfo   = [(pid, msg)]
                    }
        _ -> Left "Not initial peek phase."

-- =========================================================================================
-- Game logic

logicFinishTurn :: GameState -> Either String GameState
logicFinishTurn ofGameState = 
    case phase ofGameState of
        -- InitPeek handler
        InitPeekFeedback _ rest ->
            let nextPhase = if null rest then DrawPhase else InitialPeekPhase rest
            in Right $ ofGameState 
                { phase = nextPhase
                , privateInfo = [] -- Always delete info after round ends
                }
        
        -- PostRoundDecision
        PostRoundDecision ->
            let nextP = (currentTurn ofGameState + 1) `mod` length (players ofGameState)
            in Right $ ofGameState 
                { currentTurn   = nextP
                , phase         = DrawPhase
                , logs          = logs ofGameState ++ ["Turn ends."]
                , privateInfo   = [] 
                }
        
        _ -> Left "Cannot finish turn now."

logicDraw :: GameState -> Either String GameState
logicDraw ofGameState =
    case drawDeck ofGameState of
        [] -> Right $ ofGameState { phase = GameOver, logs = logs ofGameState ++ ["Deck's Empty"] }
        (c:rest) -> 
            let p = currentPlayer ofGameState
                (Hand oldH) = hand p
                newP = p { hand = Hand (c : oldH) }
                newPs = replacePlayer (players ofGameState) (currentTurn ofGameState) newP

                drawnInfo = "You've taken the card " ++ show c
            in Right $ ofGameState 
                { drawDeck      = rest
                , players       = newPs
                , phase         = DiscardPhase
                , logs          = logs ofGameState ++ ["Draw new card."] 
                , privateInfo   = [(playerId p, drawnInfo)]
                }

logicDiscard :: GameState -> Int -> Either String GameState
logicDiscard ofGameState idx =
    let p = currentPlayer ofGameState
        (Hand h)    = hand p
        (c, newH)   = removeAt idx h
        newP        = p { hand = Hand newH }
        newPs       = replacePlayer (players ofGameState) (currentTurn ofGameState) newP
        newPile     = c : (discardPile ofGameState)
        nextPhase   = TimpaRound 
            { targetRank    = rank c
            , originIdx     = currentTurn ofGameState
            , askingIdx     = currentTurn ofGameState
            , savedPowerup  = powerup c
            }
    in Right $ ofGameState 
        { players       = newPs
        , discardPile   = newPile
        , phase         = nextPhase
        , logs          = logs ofGameState ++ ["Discard " ++ show c]
        , privateInfo   = []
         }

logicTimpa :: GameState -> Int -> Int -> Either String GameState
logicTimpa ofGameState pid idx =
    case phase ofGameState of
        TimpaRound targetR origin asking savedP ->
            let p           = (players ofGameState) !! pid
                (Hand h)    = hand p
                c           = h !! idx
            in if rank c /= targetR
               then applyPenaltyLogic ofGameState pid ("Wrong stack! Draw one card.") targetR origin asking savedP
               else 
                   let (disc, newH) = removeAt idx h
                       newP         = p { hand = Hand newH }
                       newPs        = replacePlayer (players ofGameState) pid newP
                       newPile      = disc : (discardPile ofGameState)
                   in processPostTimpa (ofGameState { players = newPs, discardPile = newPile }) savedP ("Stack success for " ++ show pid)
        _ -> Left "Error stack"

applyPenaltyLogic :: GameState -> Int -> String -> Rank -> Int -> Int -> Powerup -> Either String GameState
applyPenaltyLogic ofGameState pid reason r origin asking savedP =
    case drawDeck ofGameState of
        [] -> Right $ ofGameState { phase = GameOver }
        (card:rest) ->
            let p           = (players ofGameState) !! pid
                (Hand oldH) = hand p
                newP        = p { hand = Hand (card : oldH) }
                newPs       = replacePlayer (players ofGameState) pid newP
                nextAsking  = (asking + 1) `mod` length (players ofGameState)
                newState    = ofGameState { players = newPs, drawDeck = rest, logs = logs ofGameState ++ [reason] }
            in if nextAsking == origin
               then processPostTimpa newState savedP "Stack phase ends."
               else Right $ newState { phase = TimpaRound r origin nextAsking savedP }

logicPassTimpa :: GameState -> Either String GameState
logicPassTimpa ofGameState =
    case phase ofGameState of
        TimpaRound r origin asking savedP ->
            let nextAsking = (asking + 1) `mod` length (players ofGameState)
            in if nextAsking == origin
               then processPostTimpa ofGameState savedP "Stack done."
               else Right $ ofGameState { phase = TimpaRound r origin nextAsking savedP }
        _ -> Left "Error"

processPostTimpa :: GameState -> Powerup -> String -> Either String GameState
processPostTimpa ofGameState pType msg =
    let ofGameStateLog = ofGameState { logs = logs ofGameState ++ [msg] }
    in case pType of
        Normal -> Right $ ofGameStateLog { phase = PostRoundDecision, logs = logs ofGameStateLog ++ ["Type 'kabul' or 'finish'."] }
        pt     -> Right $ ofGameStateLog { phase = ResolvePowerup pt, logs = logs ofGameStateLog ++ ["Powerup " ++ show pt ++ "!"] }

logicSkipPowerup :: GameState -> Either String GameState
logicSkipPowerup ofGameState = Right $ ofGameState { phase = PostRoundDecision, logs = logs ofGameState ++ ["Skip Powerup."] }

logicResolve :: GameState -> Int -> [Int] -> Either String GameState
logicResolve ofGameState t idcs =
    case phase ofGameState of
        ResolvePowerup pt -> 
            case applyPowerupLogic ofGameState pt t idcs of
                Left e -> Left e
                Right (ns, msg) -> Right $ ns { phase = PostRoundDecision, privateInfo = [(currentTurn ofGameState, msg)] }
        _ -> Left "Bukan Powerup"


applyPowerupLogic :: GameState -> Powerup -> Int -> [Int] -> Either String (GameState, String)
-- PeekSelf
applyPowerupLogic ofGameState PeekSelf t [i] = 
    if t /= currentTurn ofGameState 
        then 
            Left "Wrong target" 
        else 
            Right (ofGameState, "PeekSelf: " ++ safeGetCard ((players ofGameState)!!t) i)

-- PeekOpponent
applyPowerupLogic ofGameState PeekOpponent t [i] = 
    if t == currentTurn ofGameState 
        then 
            Left "Wrong target" 
        else 
            Right (ofGameState, "PeekOpponent: " ++ safeGetCard ((players ofGameState)!!t) i)

-- PeekDouble
applyPowerupLogic ofGameState PeekDouble t [i1, i2] = 
    if t == currentTurn ofGameState 
        then 
            Left "Wrong target" 
        else 
            let me = (players ofGameState)!!(currentTurn ofGameState); opp=(players ofGameState)!!t 
            in Right (ofGameState, "Me: "++safeGetCard me i1++","++safeGetCard me i2++" | Enemy: "++safeGetCard opp i1++","++safeGetCard opp i2)

-- Switch
applyPowerupLogic ofGameState Switch t [i1, i2] = 
    if t == currentTurn ofGameState 
        then 
            Left "Wrong target" 
        else 
            Right (executeSwap ofGameState (currentTurn ofGameState) i1 t i2, "Switch Done")

-- Wrong format case
applyPowerupLogic _ _ _ _ = Left "Wrong format, look at tutorial."


logicKabul :: GameState -> Int -> Either String GameState
logicKabul ofGameState callerId = 
    let playerList  = players ofGameState
        caller      = playerList !! callerId
        cScore      = handScore (hand caller)

        minOther         = minimum (map (handScore.hand) (filter (\player -> playerId player /= callerId) playerList ))
        finalGameState   = if cScore <= minOther then applyRankingPoints ofGameState else applyPenaltyPoints ofGameState callerId
    in Right $ finalGameState { phase = GameOver }

safeGetCard :: Player -> Int -> String
safeGetCard p idx = 
    let (Hand h) = hand p 
    in 
        if idx >= 0 && idx < length h then show (h!!idx) else "Out Of Index"

-- ============================================================================================
-- Helpers

replacePlayer :: [Player] -> Int -> Player -> [Player]
replacePlayer list idx newPlayer = let (b,_:a) = splitAt idx list in b ++ [newPlayer] ++a

removeAt :: Int -> [a] -> (a, [a])
removeAt idx list = (list !! idx, take idx list ++ drop (idx + 1) list)

replaceListIndex :: [a] -> Int -> a -> [a]
replaceListIndex list idx newVal =
    take idx list ++ [newVal] ++ drop (idx + 1) list

executeSwap :: GameState -> Int -> Int -> Int -> Int -> GameState
executeSwap ofGameState player1_id idx1 player2_id idx2 = 
    let playerList  = players ofGameState
        oPlayer1    = playerList !! player1_id 
        oPlayer2    = playerList !! player2_id 
        (Hand h1)   = hand oPlayer1
        (Hand h2)   = hand oPlayer2

        c1 = if idx1 < length h1 then h1 !! idx1 else Card Joker Red Normal
        c2 = if idx2 < length h2 then h2 !! idx2 else Card Joker Red Normal
        -- To counter error, give Red Joker to debug.

        n1 = if idx1 < length h1 then replaceListIndex h1 idx1 c2 else h1
        n2 = if idx2 < length h2 then replaceListIndex h2 idx2 c1 else h2
    in ofGameState { players = replacePlayer (replacePlayer playerList player1_id (oPlayer1 { hand = Hand n1 })) player2_id (oPlayer2{ hand = Hand n2 }) }

applyRankingPoints :: GameState -> GameState
applyRankingPoints ofGameState = 
    let sorted = sortBy (compare `on` (handScore.hand)) (players ofGameState) 
    in ofGameState 
    { 
        players = map (\player -> player {matchPoints = matchPoints player + lookupPt (playerId player) (zip (map playerId sorted) [3,2,1,0])}) (players ofGameState) 
    }

applyPenaltyPoints :: GameState -> Int -> GameState
applyPenaltyPoints ofGameState bad = ofGameState { players = map (\player' -> if playerId player' == bad then player' {matchPoints = matchPoints player'-1} else player' {matchPoints = matchPoints player'+1}) (players ofGameState) }

lookupPt _ [] = 0
lookupPt idx ((x,v):xs) = if idx == x then v else lookupPt idx xs