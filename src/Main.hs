{-# LANGUAGE OverloadedStrings #-}
module Main where

main :: IO ()
main = do
    resp <- httpGet "https://github.com/xnuk.keys"
    print $ resp ^. responseBody
    let a = "aaa" :: String
    putStrLn [i|bbb #{a}Bbb|]
