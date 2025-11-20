{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 9090 $ do
    get "/" $ file "static/index.html"
    get "/assets/:file" $ do
        f <- param "file"
        file ("static/assets/" ++ f)
