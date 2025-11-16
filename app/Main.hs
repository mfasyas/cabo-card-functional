{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do

    -- Serve folder static/
    middleware $ staticPolicy (addBase "static")

    -- Serve index.html
    get "/" $ file "static/index.html"
