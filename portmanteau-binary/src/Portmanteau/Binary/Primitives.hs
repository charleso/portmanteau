{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Binary.Primitives (
    byteString
  , word8
  , word16le
  , word32le
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import           Data.ByteString (ByteString)
import qualified Data.Word as W

import           P

import           Portmanteau.Binary.Codec


byteString :: Int -> BinaryCodec ByteString
byteString i =
  binaryCodec' B.putByteString (B.getByteString i)

word8 :: BinaryCodec W.Word8
word8 =
  binaryCodec' B.putWord8 B.getWord8

word16le :: BinaryCodec W.Word16
word16le =
  binaryCodec' B.putWord16le B.getWord16le

word32le :: BinaryCodec W.Word32
word32le =
  binaryCodec' B.putWord32le B.getWord32le
