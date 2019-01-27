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
import GitHeck (getBranchName)

main :: IO ()
main = do
    res <- parse <$> readFileUtf8 ".git/config"
    branchName <- getBranchName ".git/"
    print res
    print branchName
