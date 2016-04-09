{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Binary.VInt where

import           P

import           Portmanteau.Binary.VInt

import           System.IO (IO)

import           Test.Portmanteau.Binary
import           Test.QuickCheck (quickCheckAll)
import           Test.QuickCheck.Instances ()


prop_codec_vint a =
  binaryCodecTripping a vint

prop_codec_vint64 a =
  binaryCodecTripping a vint64

return []
tests :: IO Bool
tests = $quickCheckAll
