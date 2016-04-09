{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Binary.Trade where

import           Data.Word (Word16, Word32)

import           P

import           Portmanteau.Binary
import           Portmanteau.Core
import           Portmanteau.Lens

import           System.IO (IO)

import           Test.Portmanteau.Binary
import           Test.QuickCheck (Arbitrary (..), quickCheckAll)
import           Test.QuickCheck.Instances ()


-- FIX This is just a shitty example taken from the binary docs.
-- It would be great to have one (or more) _real_ binary protocol implemented here.

data Trade =
  Trade {
      timestamp :: Word32
    , price :: Word32
    , qty :: Word16
    } deriving (Eq, Show)

makeIso ''Trade


instance Arbitrary Trade where
  arbitrary =
    Trade
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary


prop_codec_trade_binary a =
  binaryCodecTripping a $
    _Trade
      |$| binaryCodec
      |*| binaryCodec
      |*| binaryCodec

prop_codec_trade_primitives a =
  binaryCodecTripping a $
    _Trade
      |$| word32le
      |*| word32le
      |*| word16le


return []
tests :: IO Bool
tests = $quickCheckAll
