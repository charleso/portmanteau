{-# LANGUAGE NoImplicitPrelude #-}
module Test.Portmanteau.Binary where

import           P

import           Portmanteau.Binary

import           Test.QuickCheck (Property, (===))


binaryCodecTripping :: (Eq a, Show a) => a -> BinaryCodec a -> Property
binaryCodecTripping a c =
  (fmap (\(_, _, x) -> x) . decodeOrFail c . encode c) a === pure a
