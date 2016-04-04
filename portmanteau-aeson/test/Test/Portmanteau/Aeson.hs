{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Aeson where

import           P

import           Portmanteau.Core
import           Portmanteau.Aeson

import           System.IO (IO)

import           Test.QuickCheck (Testable, Property, (===), conjoin, quickCheckAll)
import           Test.QuickCheck.Monadic (monadicIO, run, stop)
import           Test.QuickCheck.Instances ()


prop_aeson_example_1 (a :: Text) =
  jsonCodecTripping a jsonCodec

prop_aeson_example_object (a :: ((Text, Int), [Bool])) =
  jsonCodecTripping a . liftJsonObjectCodec $
        "a" .| jsonCodec
    |*| "b" .| jsonCodec
    |*| "c" .| jsonCodec


jsonCodecTripping :: (Eq a, Show a) => a -> JsonCodec a a -> Property
jsonCodecTripping a c =
  jsonCodecDecode c (jsonCodecEncode c a) === pure a


return []
tests :: IO Bool
tests = $quickCheckAll
