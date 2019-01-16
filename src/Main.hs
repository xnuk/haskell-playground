{-# LANGUAGE OverloadedStrings #-}
module Main where

main :: IO ()
main = do
    resp <- get "https://github.com/xnuk.keys"
    print $ resp ^. responseBody
    let a = "aaa"
    putStrLn $ [i|bbb #{a}Bbb|]
