{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Binary.Codec (
    BinaryCodec'
  , BinaryCodec
  , encode
  , decodeOrFail
  , binaryCodec'
  ) where

import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.Binary (Get)
import           Data.Binary.Get (ByteOffset)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (Op (..))

import           P

import           Portmanteau.Core


type BinaryCodec' = Codec' (Op Builder) Get
type BinaryCodec a = BinaryCodec' a a


encode :: BinaryCodec a -> a -> BSL.ByteString
encode (Codec (Op e) _) =
  toLazyByteString . e

decodeOrFail :: BinaryCodec a -> BSL.ByteString -> Either (BSL.ByteString, ByteOffset, [Char]) (BSL.ByteString, ByteOffset, a)
decodeOrFail (Codec _ d) =
  B.runGetOrFail d


binaryCodec' :: (a -> Builder) -> Get a -> BinaryCodec a
binaryCodec' p =
  Codec (Op p)
