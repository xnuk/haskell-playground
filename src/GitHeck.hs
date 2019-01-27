{-# LANGUAGE OverloadedStrings #-}
module GitHeck (getBranchName, GitHead(..)) where

import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString (stripPrefix)

data GitHead = RefHead ByteString | CommitHash ByteString
    deriving (Show, Eq)

word0, word9, worda, wordf, wordSpace, wordCr, wordLf :: Word8
word0     = fromIntegral $ ord '0'
word9     = fromIntegral $ ord '9'
worda     = fromIntegral $ ord 'a'
wordf     = fromIntegral $ ord 'f'
wordSpace = fromIntegral $ ord ' '
wordCr    = fromIntegral $ ord '\r'
wordLf    = fromIntegral $ ord '\n'

isHexadecimal :: Word8 -> Bool
isHexadecimal c =  word0 <= c && word9 >= c
                || worda <= c && wordf >= c

parseBranchName, parseCommitHash, parseHEAD :: ByteString -> Maybe GitHead
parseBranchName bs = do
    val <- stripPrefix "ref:" bs
    let trimmedStart = B.dropWhile (== wordSpace) val
    name <- stripPrefix "refs/heads/" trimmedStart
    pure . RefHead $ B.takeWhile (\c -> c /= wordCr && c /= wordLf) name

parseCommitHash bs
    | B.length hash > 8 = Just . CommitHash $ hash
    | otherwise = Nothing
    where hash = B.takeWhile isHexadecimal bs

parseHEAD bs = parseBranchName bs <|> parseCommitHash bs

hasTrailingSlash :: RawFilePath -> Bool
hasTrailingSlash = (== fromIntegral (ord '/')) . B.last

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b
    | B.null a = b
    | B.null b = a
    | hasTrailingSlash a = a <> b
    | otherwise = a <> "/" <> b

getBranchName :: RawFilePath -> IO GitHead
getBranchName dotgit = readFile (dotgit </> "HEAD") >>= ioJust . parseHEAD
    where ioJust = maybe (ioError $ userError "Unable to parse HEAD") return
