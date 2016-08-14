{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Aeson2 where

import           Control.Arrow ((>>>))
import           Control.Category ((.))

import           Data.Aeson.Types (parseEither)

import           P hiding ((.))

import           Portmanteau.Core.CodecA
import           Portmanteau.Aeson2
import           Portmanteau.Lens

import           System.IO (IO)

import           Test.Portmanteau.Core.Codec
import           Test.QuickCheck (Arbitrary (..), Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()


data Foo =
  Foo Text Int [Bool]
  deriving (Show, Eq)
makeIso ''Foo

instance Arbitrary Foo where
  arbitrary =
    Foo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

prop_aeson_example_1 (a :: Text) =
  jsonCodecTripping a jsonCodec

prop_aeson_example_object (a :: ((Text, Int), [Bool])) =
  jsonCodecTripping a . (>>>) jsonCodec $
        "a" .| jsonCodec
    |*| "b" .| jsonCodec
    |*| "c" .| jsonCodec

prop_aeson_example_object_2 (a :: ((Text, Int), [Bool])) =
  jsonCodecTripping a . (>>>) jsonCodec $
        (jsonCodec . field "a")
    |*| (jsonCodec . field "b")
    |*| (jsonCodec . field "c")

prop_aeson_example_object_3 (a :: Foo) =
  jsonCodecTripping a . (>>>) jsonCodec $
    _Foo
      |&| (jsonCodec . field "a")
      |*| (jsonCodec . field "b")
      |*| (jsonCodec . field "c")

jsonCodecTripping :: (Eq a, Show a) => a -> JsonCodec a -> Property
jsonCodecTripping a c =
  codecLawsK (pure a) c parseEither


return []
tests :: IO Bool
tests = $quickCheckAll
