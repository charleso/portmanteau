{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Binary.Codec (
    BinaryCodec'
  , BinaryCodec
  , encode
  , decodeOrFail
  , binaryCodec
  , binaryCodec'
  ) where

import           Data.Binary (Binary, Get)
import           Data.Binary.Builder (Builder, toLazyByteString)
import           Data.Binary.Get (ByteOffset)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
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


binaryCodec :: Binary a => BinaryCodec a
binaryCodec =
  binaryCodec' (B.execPut . B.put) B.get

binaryCodec' :: (a -> Builder) -> Get a -> BinaryCodec a
binaryCodec' p =
  Codec (Op p)
