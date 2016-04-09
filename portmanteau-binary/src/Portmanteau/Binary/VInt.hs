{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Binary.VInt (
    vint
  , vint64

  , getVInt
  , getVInt64

  , putVInt
  , putVInt64
  ) where

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Binary
import           Data.Binary.Put (Put)
import qualified Data.Binary.Put as Binary
import           Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, bit)
import qualified Data.ByteString as B
import           Data.Word (Word8)

import           P

import           Portmanteau.Binary.Codec


b11110000 :: Int8
b11110000 =
  fromIntegral (0xf0 :: Word8)
{-# INLINE b11110000 #-}

b10000000 :: Int8
b10000000 =
  fromIntegral (0x80 :: Word8)
{-# INLINE b10000000 #-}

b00000111 :: Int8
b00000111 =
  fromIntegral (0x07 :: Word8)
{-# INLINE b00000111 #-}

b00001000 :: Int8
b00001000 =
  fromIntegral (0x08 :: Word8)
{-# INLINE b00001000 #-}

vintSingle :: Int8 -> Bool
vintSingle headByte =
  (headByte .&. b11110000) /= b10000000
{-# INLINE vintSingle #-}

vintRemaining :: Int8 -> Int
vintRemaining headByte =
  fromIntegral (complement headByte .&. b00000111) + 1
{-# INLINE vintRemaining #-}

vintNegative :: Int8 -> Bool
vintNegative headByte =
  (complement headByte .&. b00001000) /= 0
{-# INLINE vintNegative #-}

getVInt64 :: Get Int64
getVInt64 = do
  headByte <- Binary.getInt8

  if vintSingle headByte then
    pure $ fromIntegral headByte
  else do
    let
      go :: Int64 -> Word8 -> Int64
      go x b =
        (x `shiftL` 8) .|. fromIntegral b

    x <- B.foldl' go 0 <$> Binary.getByteString (vintRemaining headByte)

    if vintNegative headByte then
      pure $ complement x
    else
      pure x

getVInt :: Get Int
getVInt =
  fromIntegral <$> getVInt64
{-# INLINE getVInt #-}

putVInt64 :: Int64 -> Put
putVInt64 v =
  if v >= -112 && v <= 127 then
    Binary.putWord8 $ fromIntegral v
  else
    let
      (base, value) =
        if v >= 0 then
          (-113, v)
        else
          (-121, complement v)
    in
      if value < bit 8 then do
        Binary.putWord8 base
        Binary.putWord8 . fromIntegral $ value
      else if value < bit 16 then do
        Binary.putWord8 $ base - 1
        Binary.putWord8 . fromIntegral $ value `shiftR` 8
        Binary.putWord8 . fromIntegral $ value
      else if value < bit 24 then do
        Binary.putWord8 $ base - 2
        Binary.putWord8 . fromIntegral $ value `shiftR` 16
        Binary.putWord8 . fromIntegral $ value `shiftR` 8
        Binary.putWord8 . fromIntegral $ value
      else if value < bit 32 then do
        Binary.putWord8 $ base - 3
        Binary.putWord32be . fromIntegral $ value
      else if value < bit 40 then do
        Binary.putWord8 $ base - 4
        Binary.putWord32be . fromIntegral $ value `shiftR` 8
        Binary.putWord8 . fromIntegral $ value
      else if value < bit 48 then do
        Binary.putWord8 $ base - 5
        Binary.putWord32be . fromIntegral $ value `shiftR` 16
        Binary.putWord8 . fromIntegral $ value `shiftR` 8
        Binary.putWord8 . fromIntegral $ value
      else if value < bit 56 then do
        Binary.putWord8 $ base - 6
        Binary.putWord32be . fromIntegral $ value `shiftR` 24
        Binary.putWord8 . fromIntegral $ value `shiftR` 16
        Binary.putWord8 . fromIntegral $ value `shiftR` 8
        Binary.putWord8 . fromIntegral $ value
      else do
        Binary.putWord8 $ base - 7
        Binary.putWord64be . fromIntegral $ value

putVInt :: Int -> Put
putVInt =
  putVInt64 . fromIntegral
{-# INLINE putVInt #-}

vint64 :: BinaryCodec Int64
vint64 =
  binaryCodec' putVInt64 getVInt64
{-# INLINE vint64 #-}

vint :: BinaryCodec Int
vint =
  binaryCodec' putVInt getVInt
{-# INLINE vint #-}
