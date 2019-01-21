{-# LANGUAGE OverloadedStrings #-}
module CircleCIWrapper where

baseUrl :: String
baseUrl = "https://circleci.com/api/v1.1"

circleOption :: Text -> Options
circleOption token = defaults & header "Accept" .~ ["application/json"] & param "circle-token" .~ [token]

circleGet token url = getWith (circleOption token) (baseUrl ++ url)
circleTrigger token url = postWith (circleOption token) (baseUrl ++ url) (mempty :: ByteString)
