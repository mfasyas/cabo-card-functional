{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, get, post, json, file, param, text)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Text.Lazy as TL

import Card       (buildDeck)
import Player     (Table(..), table1, shuffleDeck, deal4CardsToEachPlayer)
import Actions    (drawForHand)  -- kita pakai buat contoh aksi
import View       (toTableView)

-- Inisialisasi permainan sekali (logika sama seperti yang dulu di gameLoop)
initGame :: IO Table
initGame = do
    deck <- shuffleDeck buildDeck
    let initialTable = table1 { drawDeck = deck }
        gameTable    = deal4CardsToEachPlayer initialTable
    return gameTable

main :: IO ()
main = do
    putStrLn "Starting web server at http://localhost:3000 ..."

    -- state awal
    gameTable <- initGame
    tableRef  <- newIORef gameTable

    scotty 3000 $ do
        -- halaman utama
        get "/" $ file "static/index.html"

        -- lihat state terkini
        get "/state" $ do
            t <- liftIO $ readIORef tableRef
            json (TL.pack (show t))

        -- 🔹 1) NEW GAME: reset Table
        post "/new-game" $ do
            t <- liftIO initGame
            liftIO $ writeIORef tableRef t
            text "ok"

        -- 🔹 2) DRAW: draw satu kartu untuk player idx
        --    contoh akses: POST /draw?player=0
        post "/draw" $ do
            playerIdx <- param "player"
            liftIO $ modifyIORef' tableRef (\t -> drawForHand t playerIdx)
            text "ok"
