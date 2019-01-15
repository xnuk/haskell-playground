{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
    ( module P
    , ByteString
    , Text
    , module Conduit
    , module Control.Monad
    , module Control.Arrow
    , module Control.Applicative
    , module Data.Function
    , module Control.Monad.IO.Class
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Default.Class
    , module Lens.Micro.Platform
    , module Network.Wreq
    , convertString
    , module Text.Regex.PCRE.Heavy
    , r, i
    ) where

import "bytestring" Data.ByteString (ByteString)
import "text" Data.Text (Text)
import "conduit" Conduit
import "base" Prelude as P
import "base" Control.Monad
import "base" Control.Arrow
import "base" Control.Applicative
import "base" Data.Function
import "base" Control.Monad.IO.Class
import "base" Data.Maybe
import "base" Data.Monoid
import "data-default-class" Data.Default.Class
import "microlens-platform" Lens.Micro.Platform
import "wreq" Network.Wreq
import "string-conversions" Data.String.Conversions (convertString)
import "pcre-heavy" Text.Regex.PCRE.Heavy hiding (compileM)
import "raw-strings-qq" Text.RawString.QQ (r)
import "interpolate" Data.String.Interpolate.IsString (i)
