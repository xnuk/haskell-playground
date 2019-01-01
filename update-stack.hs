{-# LANGUAGE PackageImports #-}
module UpdateStack (main) where
import "process" System.Process
import "base" Data.List
import "base" Data.Maybe
import "base" Control.Applicative
import "base" Data.Function
import "base" Text.ParserCombinators.ReadP
import "base" Control.Arrow
import "base" System.IO

data ResolverChannel = LTS | Nightly

type Sheet = ([String], [String])

instance Show ResolverChannel where
    show LTS = "lts"
    show Nightly = "nightly"

infixl 2 >&
(>&) :: Applicative m => m a -> (a -> b) -> m b
a >& f = a <**> pure f

infixl 9 |.
(|.) :: (a -> b) -> (b -> c) -> a -> c
(|.) = flip (.)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

dropTo :: Char -> String -> String
dropTo c = dropWhile (/= c) |. dropWhile (== c)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

untilM :: Monad m => m Bool -> m a -> m [a]
untilM m f = step
    where
        step = do
            b <- m
            if b
               then return []
               else do
                   x <- f
                   xs <- step
                   return (x:xs)

parseResolver :: ResolverChannel -> ReadP String
parseResolver res = do
    channel <- string $ show res <> "-"
    version <- munch1 (\c -> isDigit c || c == numberSeparator)
    return $ channel <> version
        where
            numberSeparator = case res of
                LTS -> '.'
                Nightly -> '-'

header :: String -> [(String, String)]
header = dropTo '/' |. readP_to_S (parseResolver LTS <++ parseResolver Nightly)

curl :: ResolverChannel -> IO (Maybe String)
curl res = do
    readProcess "curl" ["--head", "https://www.stackage.org/" <> show res] ""
        >& lines
        |. find (isPrefixOf "Location: ")
        |. (>>= listToMaybe . map fst . header)

resolver :: Handle -> IO (Maybe (ResolverChannel, Sheet))
resolver h = do
    file <- untilM (hIsEOF h) (hGetLine h)

    let line = zip [(0 :: Int)..] file
            & find (isPrefixOf "resolver: " . snd)
            & fmap (second (dropTo ' '))

    return $ case line of
        Just (idx, str)
            | (show LTS) `isPrefixOf` str -> Just (LTS, sheet)
            | (show Nightly) `isPrefixOf` str -> Just (Nightly, sheet)
            where sheet = second safeTail $ splitAt idx file
        _ -> Nothing

write :: Handle -> String -> Sheet -> IO ()
write h res (before, after) = do
    hSetFileSize h 0
    hPutStr h . unlines $ before <> ["resolver: " <> res] <> after

unwrap :: Monad m => m (Maybe a) -> String -> m a
unwrap = flip (fmap . fromMaybe . error)

main :: IO ()
main = do
    withFile "./stack.yaml" ReadWriteMode $ \h -> do
        pos <- hGetPosn h
        (res, sheet) <- unwrapResolver h
        resolver' <- unwrapCurl res
        hSetPosn pos
        write h resolver' sheet
    where
        unwrapResolver h = unwrap (resolver h) $
            "stack.yaml is wrongly configured or doesn't exist"

        unwrapCurl res = unwrap (curl res) $
            "Failed to get latest "
            <> show res
            <> " resolver name from stackage"
