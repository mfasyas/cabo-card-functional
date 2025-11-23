{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, object, (.=))
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors (simpleCors)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import System.Random (newStdGen)

-- Import modul game Anda
import Card (shuffleDeck, buildDeck)
import GameStates
import GameEngine (updateGame)
import Player (Player(..))

-- Wrapper untuk menerima JSON: { "tag": "...", "contents": ... }
data TaggedAction = TaggedAction { tag :: String, contents :: A.Value } 
    deriving (Show)

instance FromJSON TaggedAction where
    parseJSON = A.withObject "TaggedAction" $ \v -> TaggedAction
        <$> v A..: "tag"
        <*> v A..: "contents"

main :: IO ()
main = do
    -- 1. Setup Awal: Shuffle Deck & State
    deck <- shuffleDeck buildDeck
    let state0 = initialState deck
    
    -- Variable Global (Thread Safe)
    stateVar <- newTVarIO state0

    putStrLn "============================================="
    putStrLn "  SERVER CARD GAME BERJALAN (PORT 3000)      "
    putStrLn "  Buka browser: http://localhost:3000        "
    putStrLn "============================================="

    scotty 3000 $ do
        -- Middleware agar JS bisa akses tanpa error CORS
        middleware simpleCors 

        -- RUTE STATIS (HTML/CSS/JS)
        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "index.html"

        get "/styles.css" $ do
            setHeader "Content-Type" "text/css"
            file "styles.css"

        get "/script.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "script.js"

        -- RUTE API: AMBIL STATE GAME
        get "/game/state" $ do
            currentState <- liftIO $ readTVarIO stateVar
            json currentState

        -- RUTE API: RESET GAME
        post "/game/reset" $ do
            liftIO $ putStrLn "[INFO] Game Reset Requested"
            newDeck <- liftIO $ shuffleDeck buildDeck
            let newState = initialState newDeck
            liftIO $ atomically $ writeTVar stateVar newState
            json newState

        -- RUTE API: TERIMA AKSI (LOGIC UTAMA)
        post "/game/action" $ do
            tagged <- jsonData :: ActionM TaggedAction
            
            -- Debugging: Print aksi ke terminal
            liftIO $ putStrLn $ "[ACTION] Menerima: " ++ show tagged

            currentState <- liftIO $ readTVarIO stateVar
            
            -- Parsing JSON ke GameAction Haskell
            let actionOrError = parseAction tagged

            case actionOrError of
                Left err -> do
                    liftIO $ putStrLn $ "[ERROR] Parsing Gagal: " ++ err
                    json $ object ["error" .= err]
                
                Right gameAct -> do
                    -- Jalankan Update Logic (GameEngine.hs)
                    case updateGame currentState gameAct of
                        Left logicErr -> do
                            liftIO $ putStrLn $ "[LOGIC FAIL] " ++ logicErr
                            json $ object ["error" .= logicErr]
                        
                        Right newState -> do
                            liftIO $ atomically $ writeTVar stateVar newState
                            liftIO $ putStrLn "[SUCCESS] State Updated"
                            json newState

-- Fungsi Helper Parsing yang Lebih Aman
parseAction :: TaggedAction -> Either String GameAction
parseAction (TaggedAction t c) = case t of
    
    "DrawAction" -> 
        case A.fromJSON c :: A.Result Int of
            A.Success pid -> Right (DrawAction pid)
            _             -> Left "Format DrawAction salah (harus integer PID)"

    "DiscardAction" -> 
        case A.fromJSON c :: A.Result [Int] of
            -- JS mengirim [pid, cardIndex]
            A.Success [pid, idx] -> Right (DiscardAction pid idx)
            _                    -> Left "Format DiscardAction salah (harus [pid, idx])"

    "TargetAction" -> 
        -- JS mengirim [pid, [idx1, idx2...]] -> Tuple (Int, [Int]) di Haskell
        case A.fromJSON c :: A.Result (Int, [Int]) of
            A.Success (pid, indices) -> 
                case indices of
                    []       -> Left "TargetAction kosong! Pilih minimal 1 kartu."
                    [i]      -> Right (TargetAction pid (i, i))      -- Jika cuma 1, duplikat (untuk single target)
                    (i1:i2:_) -> Right (TargetAction pid (i1, i2))   -- Jika 2, ambil keduanya
            _ -> Left "Format TargetAction salah (harus [pid, [indices]])"

    "FinishGameAction" ->
        case A.fromJSON c :: A.Result Int of
            A.Success pid -> Right (FinishGameAction pid)
            _             -> Left "Format FinishGameAction salah"

    _ -> Left $ "Action Tag tidak dikenali: " ++ t