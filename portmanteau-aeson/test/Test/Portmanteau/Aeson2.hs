{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Aeson2 where

import           Control.Arrow ((>>>))

import           P

import           Portmanteau.Core.CodecA
import           Portmanteau.Aeson2

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()


prop_aeson_example_1 (a :: Text) =
  jsonCodecTripping a jsonCodec

prop_aeson_example_object (a :: ((Text, Int), [Bool])) =
  jsonCodecTripping a . (>>>) jsonCodec $
        "a" .| jsonCodec
    |*| "b" .| jsonCodec
    |*| "c" .| jsonCodec

prop_aeson_example_object_2 (a :: ((Text, Int), [Bool])) =
  jsonCodecTripping a . (>>>) jsonCodec $
        (field "a" >>> jsonCodec)
    |*| (field "b" >>> jsonCodec)
    |*| (field "c" >>> jsonCodec)

jsonCodecTripping :: (Eq a, Show a) => a -> JsonCodec a -> Property
jsonCodecTripping a c =
  jsonCodecDecode c (jsonCodecEncode c a) === pure a


return []
tests :: IO Bool
tests = $quickCheckAll
