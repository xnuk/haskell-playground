{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
    ( module P
    , ByteString
    , Text
    , LazyByteString
    , LazyText
    , bempty
    , module Conduit
    , module Control.Monad
    , module Control.Arrow
    , module Control.Applicative
    , module Data.Function
    , module Control.Monad.IO.Class
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Functor
    , module Data.Default.Class
    , module Lens.Micro.Platform
    , convertString
    , module Text.Regex.PCRE.Heavy
    , module System.Environment
    , r, i

    , module Network.Wreq
    , module Data.Aeson.Lens
    , httpGet
    , httpGetWith
    , httpPost
    , httpPostWith
    , httpHead
    , httpHeadWith
    , httpOptions
    , httpOptionsWith
    , httpPut
    , httpPutWith
    , httpDelete
    , httpDeleteWith

    , readFile
    , writeFile
    , withFile
    , readFileUtf8

    , module RawFilePath









    ) where

import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Lazy as L (ByteString)
import "text" Data.Text (Text)
import qualified "text" Data.Text.Lazy as L (Text)
import "conduit" Conduit
import "base" Prelude as P hiding (readFile, writeFile)
import "base" Control.Monad
import "base" Control.Arrow
import "base" Control.Applicative
import "base" Data.Function
import "base" Control.Monad.IO.Class
import "base" Data.Functor
import "base" Data.Maybe
import "base" Data.Monoid
import "base" System.Environment
import "data-default-class" Data.Default.Class
import "microlens-platform" Lens.Micro.Platform
import "string-conversions" Data.String.Conversions (convertString)
import "pcre-heavy" Text.Regex.PCRE.Heavy hiding (compileM)
import "raw-strings-qq" Text.RawString.QQ (r)
import "interpolate" Data.String.Interpolate.IsString (i)
import "wreq" Network.Wreq hiding (get, getWith, post, postWith, head_, headWith, options, optionsWith, put, putWith, delete, deleteWith)
import qualified "wreq" Network.Wreq as W
import "wreq" Network.Wreq.Types (type Putable, type Postable)
import "lens-aeson" Data.Aeson.Lens
import qualified "text" Data.Text.IO as T
import "rawfilepath" Data.ByteString.RawFilePath (readFile, writeFile, withFile)
import "rawfilepath" RawFilePath

type LazyByteString = L.ByteString
type LazyText = L.Text

bempty :: ByteString
bempty = B.empty

httpGet,     httpDelete     ::            String -> IO (Response LazyByteString)
httpGetWith, httpDeleteWith :: Options -> String -> IO (Response LazyByteString)

httpPost     :: Postable a =>            String -> a -> IO (Response LazyByteString)
httpPostWith :: Postable a => Options -> String -> a -> IO (Response LazyByteString)

httpHead,     httpOptions     ::            String -> IO (Response ())
httpHeadWith, httpOptionsWith :: Options -> String -> IO (Response ())

httpPut     :: Putable a =>            String -> a -> IO (Response LazyByteString)
httpPutWith :: Putable a => Options -> String -> a -> IO (Response LazyByteString)

httpGet         = W.get
httpGetWith     = W.getWith
httpPost        = W.post
httpPostWith    = W.postWith
httpHead        = W.head_
httpHeadWith    = W.headWith
httpOptions     = W.options
httpOptionsWith = W.optionsWith
httpPut         = W.put
httpPutWith     = W.putWith
httpDelete      = W.delete
httpDeleteWith  = W.deleteWith

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = T.readFile
