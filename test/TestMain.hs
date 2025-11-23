{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, decode, object, (.=))
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors (simpleCors)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import System.Random (newStdGen)

-- Import modul game Anda
import Card (shuffleDeck, buildDeck, Card(..), Hand(..))
import GameStates
import GameEngine (updateGame)
import Player (Player(..))

-- Tipe data untuk menerima input Action dari JSON JS
data ActionRequest = ActionRequest {
    actionType :: String,
    targetIdx :: Maybe Int,
    targetIndices :: Maybe [Int]
} deriving (Show)

instance FromJSON ActionRequest where
    parseJSON = A.withObject "ActionRequest" $ \v -> ActionRequest
        <$> v A..: "actionType"
        <*> v A..:? "targetIdx"
        <*> v A..:? "targetIndices"

main :: IO ()
main = do
    deck <- shuffleDeck buildDeck
    -- Pastikan initialState di GameStates.hs sudah dikembalikan ke 2 player
    let state0 = initialState deck
    
    stateVar <- newTVarIO state0

    putStrLn "Server berjalan di http://localhost:3000"

    scotty 3000 $ do
        middleware simpleCors 

        -- 1. Rute Utama (localhost:3000/) -> membuka index.html
        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "index.html"

        -- 2. Rute CSS -> membuka style.css
        get "/style.css" $ do
            setHeader "Content-Type" "text/css"
            file "style.css"

        -- 3. Rute JS -> membuka script.js
        get "/script.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "script.js"

        -- ===================================================

        get "/game/state" $ do
            currentState <- liftIO $ readTVarIO stateVar
            json currentState

        post "/game/action" $ do
            req <- jsonData :: ActionM ActionRequest
            currentState <- liftIO $ readTVarIO stateVar
            let pid = currentTurn currentState

            -- === PERBAIKAN DI SINI ===
            -- Kita tambahkan type signature eksplisit :: Either String GameAction
            let actionOrError :: Either String GameAction
                actionOrError = case actionType req of
                    "draw" -> Right (DrawAction pid)
                    "discard" -> case targetIdx req of
                        Just idx -> Right (DiscardAction pid idx)
                        Nothing  -> Left "Missing index for discard"
                    "target" -> case targetIndices req of
                        Just idxs -> Right (TargetAction pid idxs)
                        Nothing   -> Left "Missing indices for target"
                    "finish" -> Right (FinishGameAction pid)
                    _ -> Left "Unknown action"

            case actionOrError of
                Left err -> json $ object ["error" .= err]
                Right gameAct -> do
                    case updateGame currentState gameAct of
                        Left logicErr -> json $ object ["error" .= logicErr]
                        Right newState -> do
                            liftIO $ atomically $ writeTVar stateVar newState
                            json newState

        post "/game/reset" $ do
            newDeck <- liftIO $ shuffleDeck buildDeck
            let newState = initialState newDeck
            liftIO $ atomically $ writeTVar stateVar newState
            json newState