{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, object, (.=))
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors (simpleCors)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import System.Random (newStdGen)

import Card (shuffleDeck, buildDeck)
import GameStates
import GameEngine (updateGame)
import Player (Player(..))

-- Wrapper JSON
data TaggedAction = TaggedAction { tag :: String, contents :: A.Value } 
    deriving (Show)

instance FromJSON TaggedAction where
    parseJSON = A.withObject "TaggedAction" $ \v -> TaggedAction
        <$> v A..: "tag"
        <*> v A..:? "contents" A..!= A.Null

main :: IO ()
main = do
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    stateVar <- newTVarIO state0

    putStrLn "====== KABUL SERVER READY (PORT 3000) ======"

    scotty 3000 $ do
        middleware simpleCors 

        -- Static Files
        get "/" $ do setHeader "Content-Type" "text/html"; file "index.html"
        get "/styles.css" $ do setHeader "Content-Type" "text/css"; file "styles.css"
        get "/script.js" $ do setHeader "Content-Type" "application/javascript"; file "script.js"

        -- API
        get "/game/state" $ do
            currentState <- liftIO $ readTVarIO stateVar
            json currentState

        post "/game/reset" $ do
            newDeck <- liftIO $ shuffleDeck buildDeck
            let newState = initialState newDeck
            liftIO $ atomically $ writeTVar stateVar newState
            json newState

        post "/game/action" $ do
            tagged <- jsonData :: ActionM TaggedAction
            liftIO $ putStrLn $ "[ACTION] " ++ show tagged
            
            currentState <- liftIO $ readTVarIO stateVar
            
            case parseAction tagged of
                Left err -> json $ object ["error" .= err]
                Right gameAct -> do
                    case updateGame currentState gameAct of
                        Left logicErr -> json $ object ["error" .= logicErr]
                        Right newState -> do
                            liftIO $ atomically $ writeTVar stateVar newState
                            json newState

-- LOGIC PARSING ACTION
parseAction :: TaggedAction -> Either String GameAction
parseAction (TaggedAction t c) = case t of
    "InitPeekAction" -> 
        case A.fromJSON c of
            A.Success [pid, i1, i2] -> Right (InitPeekAction pid i1 i2)
            _ -> Left "InitPeek butuh [pid, i1, i2]"

    "DrawAction" -> 
        case A.fromJSON c of
            A.Success pid -> Right (DrawAction pid)
            _ -> Left "Draw butuh PID"

    "DiscardAction" -> 
        case A.fromJSON c of
            A.Success [pid, idx] -> Right (DiscardAction pid idx)
            _ -> Left "Discard butuh [pid, idx]"

    "TimpaAction" ->
        case A.fromJSON c of
            A.Success [pid, idx] -> Right (TimpaAction pid idx)
            _ -> Left "Timpa butuh [pid, idx]"

    "PassTimpaAction" ->
        case A.fromJSON c of
            A.Success pid -> Right (PassTimpaAction pid)
            _ -> Left "PassTimpa butuh PID"

    "SkipPowerupAction" ->
        case A.fromJSON c of
            A.Success pid -> Right (SkipPowerupAction pid)
            _ -> Left "SkipPowerup butuh PID"

    "KabulAction" ->
        case A.fromJSON c of
            A.Success pid -> Right (KabulAction pid)
            _ -> Left "Kabul butuh PID"

    "FinishTurnAction" ->
        case A.fromJSON c of
            A.Success pid -> Right (FinishTurnAction pid)
            _ -> Left "FinishTurn butuh PID"

    "TargetAction" -> 
        -- Format Backend: TargetAction pid targetPid [indices]
        -- Frontend akan mengirim: [pid, targetPid, [idx1, idx2...]]
        case A.fromJSON c :: A.Result (Int, Int, [Int]) of
            A.Success (pid, tid, indices) -> Right (TargetAction pid tid indices)
            _ -> Left "TargetAction format salah"

    _ -> Left $ "Unknown Action: " ++ t