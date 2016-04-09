{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Portmanteau.Binary.Codec (
    BinaryCodec'
  , BinaryCodec
  , encode
  , decodeOrFail
  , binaryCodec
  , binaryCodec'
  ) where

import           Data.Binary (Binary, Get, Put)
import           Data.Binary.Get (ByteOffset)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (Op (..))

import           P

import           Portmanteau.Core


type BinaryCodec' = Codec' (Op Put) Get
type BinaryCodec a = BinaryCodec' a a


-- FIX Required by the Divisible instance of Op
-- The correct fix will be implement our own PutOp which includes the required instances
instance Monoid Put where
  mempty =
    B.putBuilder mempty
  mappend p1 p2 =
    B.putBuilder $ B.execPut p1 <> B.execPut p2


encode :: BinaryCodec a -> a -> BSL.ByteString
encode (Codec (Op e) _) =
  B.runPut . e

decodeOrFail :: BinaryCodec a -> BSL.ByteString -> Either (BSL.ByteString, ByteOffset, [Char]) (BSL.ByteString, ByteOffset, a)
decodeOrFail (Codec _ d) =
  B.runGetOrFail d


binaryCodec :: Binary a => BinaryCodec a
binaryCodec =
  binaryCodec' B.put B.get

binaryCodec' :: (a -> Put) -> Get a -> BinaryCodec a
binaryCodec' p =
  Codec (Op p)
