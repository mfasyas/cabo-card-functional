module Powerups where

import Text.Read (readMaybe)
import Card
import Player


-- helper
promptIndex :: Int -> IO Int
promptIndex maxIdx = do
    putStrLn $ "Pilih posisi kartu (1 -)" ++ show maxIdx ++ "): "
    input <- getLine
    case readMaybe input of
        Just n | n >= 1 && n <= maxIdx -> return n
        _ -> do 
            putStrLn "Input tidak valid, coba lagi"
            promptIndex maxIdx

-- Asumsi sekarang 2 pemain, tapi ini tetap aman untuk >2 pemain:
opponentIndex :: Table -> Int -> Int
opponentIndex table meIdx =
    let numPlayers = length (players table)
    in (meIdx + 1) `mod` numPlayers

removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs = let (l, r) = splitAt idx xs in (head r, l ++ tail r)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = let (l, r) = splitAt idx xs in (l ++ [x] ++ r)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = let (l, _ : r) = splitAt idx xs in (l ++ [x] ++ r)

replaceAt' :: Int -> a -> [a] -> [a]
replaceAt' idx val xs =
    take idx xs ++ [val] ++ drop (idx + 1) xs



peekSelf :: Table -> Int -> IO Table
peekSelf table playerIdx = do
    let
        ps = players table
        p = ps !! playerIdx
        Hand hs = hand p
        n = length hs

    if n == 0 
        then do 
            putStrLn "Tidak ada kartu di tangan untuk diintip."
            return table
        else do 
            putStrLn $ "\n [Powerup PeekSelf] Pemain " ++ show (playerId p)
            putStrLn "Posisi kartu di tangan: "
            mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..n]

            idx <- promptIndex n
            let chosen = hs !! (idx - 1)
            putStrLn $ "Kartu di posisi " ++ show idx ++ " adalah: " ++ showCardRS chosen
            return table

peekOpponent :: Table -> Int -> IO Table
peekOpponent table playerIdx = do
    let ps         = players table
        numPlayers = length ps
        -- me         = ps !! playerIdx

    putStrLn "\n[Powerup PeekSO] Intip satu kartu sendiri dan satu kartu lawan."

    let askTarget = do
            putStr "Masukkan nomor urut pemain target: "
            inputStr <- getLine

            let choice = read inputStr :: Int 
            let idx = choice - 1 -- Konversi ke index 0-based

            if idx >= 0 && idx < numPlayers && idx /= playerIdx
                then return idx
                else do
                    putStrLn "Pilihan tidak valid (tidak boleh diri sendiri/di luar daftar)."
                    askTarget 

    targetIdx <- askTarget
    
    let target     = ps !! targetIdx
        Hand hsOpp = hand target
        n          = length hsOpp

    if n == 0
      then do
        putStrLn "Lawan tidak punya kartu untuk diintip."
        return table
      else do
        putStrLn $ "\nAnda mengintip kartu milik " ++ show (playerId target)
        -- Tampilkan slot kartu tertutup (1..n)
        mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..n]

        idx <- promptIndex n -- Menggunakan promptIndex yang sudah Anda miliki
        let chosen = hsOpp !! (idx - 1)
        putStrLn $ "Kartu lawan di posisi " ++ show idx ++ " adalah: " ++ showCardRS chosen
        
        return table

peekSO :: Table -> Int -> IO Table
peekSO table playerIdx = do
    putStrLn "\n[Powerup PeekSO] Intip satu kartu sendiri dan satu kartu lawan."
    -- Kedua fungsi ini cuma print info, tidak mengubah Table
    _ <- peekSelf table playerIdx
    _ <- peekOpponent table playerIdx
    return table


swapBetween :: Int -> Int -> [a] -> [a] -> ([a], [a])
swapBetween i j xs ys =
    let xi = xs !! i
        yj = ys !! j
        xs' = replaceAt i yj xs
        ys' = replaceAt j xi ys
    in (xs', ys')

switchWithOpponent :: Table -> Int -> IO Table
switchWithOpponent table playerIdx = do
    let ps          = players table
        numPlayers = length ps
        me          = ps !! playerIdx

    putStrLn "\n[Powerup Switch] Intip satu kartu sendiri dan satu kartu lawan."

    let askTarget = do
            putStr "Masukkan nomor urut pemain target: "
            inputStr <- getLine

            let choice = read inputStr :: Int 
            let idx = choice - 1 -- Konversi ke index 0-based

            if idx >= 0 && idx < numPlayers && idx /= playerIdx
                then return idx
                else do
                    putStrLn "Pilihan tidak valid (tidak boleh diri sendiri/di luar daftar)."
                    askTarget 

    oppIdx <- askTarget
    
    let
        opponent    = ps !! oppIdx
        Hand myHs   = hand me
        Hand oppHs  = hand opponent
        myCount     = length myHs
        oppCount    = length oppHs

    if myCount == 0 || oppCount == 0
      then do
        putStrLn "[Powerup Switch] Salah satu pemain tidak punya kartu, tidak bisa tukar."
        return table
      else do
        putStrLn "\n[Powerup Switch] Tukar satu kartu dengan lawan."
        putStrLn $ "Pemain " ++ show (playerId me) ++ " memilih kartunya sendiri:"
        mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..myCount]
        myIdx <- promptIndex myCount

        putStrLn $ "Pilih kartu lawan (Pemain " ++ show (playerId opponent) ++ "):"
        mapM_ (\i -> putStrLn $ show i ++ ". [??]") [1..oppCount]
        oppIdxChoice <- promptIndex oppCount

        let 
            (newMyHs, newOppHs) = swapBetween (myIdx - 1) (oppIdxChoice - 1) myHs oppHs
            me' = me {hand = Hand newMyHs}
            opponent' = opponent {hand = Hand newOppHs}

            psUpdated = replaceAt' oppIdx opponent' $ replaceAt' playerIdx me' ps

        putStrLn "Kartu berhasil ditukar (isi kartu tetap sikrit)."
        return table {players = psUpdated}

peekDouble :: Table -> Int -> IO Table
peekDouble table playerIdx = do
    putStrLn "\n[Powerup PeekDouble] Intip satu kartu sendiri dan satu kartu lawan."
    -- Kedua fungsi ini cuma print info, tidak mengubah Table
    _ <- peekSelf table playerIdx
    _ <- peekSelf table playerIdx
    _ <- peekOpponent table playerIdx
    _ <- peekOpponent table playerIdx
    return table

peekSwitch :: Table -> Int -> IO Table
peekSwitch table playerIdx = do
    _ <- peekSelf table playerIdx
    _ <- peekOpponent table playerIdx

    tablePeekSwitch <- switchWithOpponent table playerIdx
    -- switching tidak harus sama dengan opponent yang diintip

    return tablePeekSwitch

-- Dipanggil setelah pemain membuang kartu ke discardPile
runPowerupOnTopDiscard :: Table -> Int -> IO Table
runPowerupOnTopDiscard table playerIdx =
    case discardPile table of
      [] -> return table
      (topCard : _) ->
        case powerup topCard of
          PeekSelf     -> peekSelf table playerIdx
          PeekOpponent -> peekOpponent table playerIdx
          PeekSO       -> peekSO table playerIdx
          Switch       -> switchWithOpponent table playerIdx
          PeekDouble   -> peekDouble table playerIdx
          PeekSwitch   -> peekSwitch table playerIdx
          _            -> return table

