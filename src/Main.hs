{-# LANGUAGE OverloadedStrings #-}
module Main where

import CircleCIWrapper (get, Token(Token))

main :: IO ()
main = do
    token <- fmap (Token . convertString) (getEnv "CIRCLECI_TOKEN")
    resp <- get "/me" token
    print $ resp ^? responseBody . key "selected_email"
