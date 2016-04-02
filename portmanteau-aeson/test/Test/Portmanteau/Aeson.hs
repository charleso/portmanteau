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
  jsonCodecTripping jsonCodec

prop_aeson_example_object (a :: (Text, Int)) =
  jsonCodecTripping (liftJsonObjectCodec $ jsonObjectCodec "a" |*| jsonObjectCodec "b")


jsonCodecTripping :: (Eq a, Show a) => JsonCodec a a -> a -> Property
jsonCodecTripping c a =
  jsonCodecDecode c (jsonCodecEncode c a) === pure a


return []
tests :: IO Bool
tests = $quickCheckAll
