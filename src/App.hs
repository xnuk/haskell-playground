{-# LANGUAGE OverloadedStrings #-}
module App (main) where

{-
import CircleCIWrapper (get, Token(Token))

main :: IO ()
main = do
    token <- fmap (Token . convertString) (getEnv "CIRCLECI_TOKEN")
    resp <- get "/me" token
    print $ resp ^? responseBody
-}

import GitConfig (parse)

main :: IO ()
main = do
    res <- parse <$> readFileUtf8 ".git/config"
    print res
