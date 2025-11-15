{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Text.Lazy as TL

import Card       (buildDeck)
import Player     (Table(..), table1, shuffleDeck, deal4CardsToEachPlayer)

main :: IO ()
main = do
    putStrLn "Starting web server at http://localhost:3000 ..."
    
    -- Inisialisasi state permainan sekali di awal
    deck <- shuffleDeck buildDeck
    let initialTable = table1 { drawDeck = deck }
        gameTable    = deal4CardsToEachPlayer initialTable

    tableRef <- newIORef gameTable

    -- Jalankan Scotty di port 3000
    scotty 3000 $ do
        -- Serve file HTML utama
        get "/" $ file "static/index.html"

        -- Endpoint untuk ngirim state Table sebagai teks
        get "/state" $ do
            t <- liftIO $ readIORef tableRef
            text (TL.pack (show t))
