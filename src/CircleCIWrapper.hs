{-# LANGUAGE OverloadedStrings #-}
module CircleCIWrapper where

newtype Token = Token Text

baseUrl :: String
baseUrl = "https://circleci.com/api/v1.1"

circleOption :: Text -> Options
circleOption token = defaults
    & header "Accept" .~ ["application/json"]
    & param "circle-token" .~ [token]

get, trigger :: String -> Token -> IO (Response LazyByteString)
get     url (Token token) = httpGetWith  (circleOption token) (baseUrl ++ url)
trigger url (Token token) = httpPostWith (circleOption token) (baseUrl ++ url) bempty
