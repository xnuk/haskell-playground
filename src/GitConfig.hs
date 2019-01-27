{-
This was from dogonthehorizon/git-config, which is licensed under BSD3.
https://github.com/dogonthehorizon/git-config/blob/master/src/Text/GitConfig/Parser.hs

Here's the copyright license below.

---

Copyright (c) 2018 Fernando Freire, All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.

* Neither the name of Author name here nor the names of other
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

---
-}
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
module GitConfig (parse) where

import Prelude hiding (takeWhile)
import "attoparsec" Data.Attoparsec.Text hiding (parse)
import qualified Data.Text as T
import Data.Char

between :: Applicative f => f start -> f end -> f a -> f a
between start end middle = start *> middle <* end

comment :: Parser Text
spaces :: Parser [Text]
comment = between (char '#' <|> char ';') (char '\n') (takeWhile (/= '\n'))
spaces = many' (fmap T.singleton space <|> comment)

symbol :: Char ->  Parser Char
symbol = between spaces spaces . char

brackets, quotes :: Parser a -> Parser a
brackets = between (symbol '[') (symbol ']')
quotes = between (symbol '"') (symbol '"')

escape :: Parser Char
escape = char '\\' *> escapeChars
    where
        escapeChars
            =   char '"'
            <|> char '\\'
            <|> char '/'
            <|> (char 'n' $> '\n')
            <|> (char 't' $> '\t')
            <|> (char 'r' $> '\r')
            <|> (char 'b' $> '\b')
            <|> (char 'f' $> '\f')
            <?> "escaped character"

sectionName, sectionHeader :: Parser [Text]
sectionName = (section <|> subSection) `sepBy` spaces
    where
        section = takeWhile1 (\c -> isAlphaNum c || c == '.' || c == '-')
        subSection = fmap T.concat . quotes . many' $
            fmap T.singleton escape <|> takeWhile1 (\x -> x /= '"' && x /= '\\')

sectionHeader = brackets sectionName

varName, varValue :: Parser Text
varName = fmap T.toLower $
    T.cons <$> letter <*> takeWhile1 (\c -> isAlphaNum c || c == '-')

varValue = between spaces endOfLine (takeWhile1 isPrint)

mapping :: Parser (Text, Text)
mapping = do
    name <- varName
    val <- (symbol '=' *> (varValue <|> pure "")) <|> pure "true"
    pure (name, val)

configSection :: Parser ([Text], [(Text, Text)])
configSection = do
    secHeader <- sectionHeader
    spaces
    secValues <- mapping `sepBy` spaces
    pure (secHeader, secValues)

config :: Parser [([Text], [(Text, Text)])]
config = do
    sp <- spaces
    many1 configSection

parse :: Text -> Either String [([Text], [(Text, Text)])]
parse = parseOnly config
