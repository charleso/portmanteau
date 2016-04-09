{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Binary.Sequence where

import qualified Data.Binary.Builder as Binary
import qualified Data.Binary.Get as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..), divided)
import           Data.Profunctor (Profunctor (..))
import qualified Data.Text.Encoding as T

import           P

import           Portmanteau.Binary
import           Portmanteau.Binary.VInt
import           Portmanteau.Core
import           Portmanteau.Lens

import           System.IO (IO)

import           Test.Portmanteau.Binary
import           Test.QuickCheck (Arbitrary (..), quickCheckAll, vectorOf)
import           Test.QuickCheck.Instances ()


newtype MD5 =
  MD5 {
      unMD5 :: ByteString
    } deriving (Eq, Ord, Show)

makeIso ''MD5

instance Arbitrary MD5 where
  arbitrary =
    MD5 . B.pack <$> vectorOf 16 arbitrary

data SeqHeader =
  SeqHeader {
      seqKeyType :: !Text
    , seqValueType :: !Text
    , seqMetadata :: ![(Text, Text)]
    , seqSync :: !MD5
    } deriving (Eq, Ord, Show)

makeIso ''SeqHeader

instance Arbitrary SeqHeader where
  arbitrary =
    SeqHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

integral :: (Contravariant f, Functor g, Integral b, Integral a) => Codec f g a -> Codec f g b
integral =
  dimap fromIntegral fromIntegral

manyD :: forall f a. Decidable f => f a -> f [a]
manyD f0 =
  let
    go :: [a] -> Either () (a, [a])
    go = \case
      [] ->
        Left ()
      x : xs ->
        Right (x, xs)

    f1 :: f [a]
    f1 =
      choose go nil cons

    nil :: f ()
    nil =
      conquer

    cons :: f (a, [a])
    cons =
      divided f0 f1
  in
    f1

sizedList :: (Decidable f, Monad g) => Codec f g Int -> Codec f g a -> Codec f g [a]
sizedList (Codec f0 g0) (Codec f1 g1) =
  Codec
    (divide (\xs -> (length xs, xs)) f0 (manyD f1))
    (flip replicateM g1 =<< g0)

vintPrefixedBytes :: BinaryCodec ByteString
vintPrefixedBytes =
  binaryCodec'
    (\bs -> putVInt (B.length bs) <> Binary.fromByteString bs)
    (getVInt >>= Binary.getByteString)

textWritable :: BinaryCodec Text
textWritable =
  dimap T.encodeUtf8 T.decodeUtf8 vintPrefixedBytes

booleanWritable :: BinaryCodec Bool
booleanWritable =
  dimap (bool 0 1) (/= 0) word8

md5 :: BinaryCodec MD5
md5 =
  dimap unMD5 MD5 $ byteString 16

metadata :: BinaryCodec [(Text, Text)]
metadata =
  sizedList (integral word32le) $
    textWritable |*| textWritable

exactly :: (Eq a, Show a, Contravariant f, Monad g) => a -> Codec f g a -> (a -> g ()) -> Codec f g ()
exactly x (Codec f g) onError =
  Codec
    (contramap (const x) f)
    (g >>= \y -> unless (x == y) (onError y))

seqHeader :: BinaryCodec SeqHeader
seqHeader =
  let
    magic =
      exactly "SEQ" (byteString 3) . const $
        fail "not a sequence file"

    version =
      exactly 6 word8 $ \v ->
        fail $ "unknown version: " <> show v

    compression =
      exactly True booleanWritable . const $
        fail "only block compressed files are supported"

    blockCompression =
      exactly True booleanWritable . const $
        fail "only block compressed files are supported"

    compressionType =
      exactly "org.apache.hadoop.io.compress.SnappyCodec" textWritable . const $
        fail "only snappy compressed files are supported"
  in
    _SeqHeader
      |$| magic *| version *| textWritable
      |*| textWritable
      |*| compression *| blockCompression *| compressionType *| metadata
      |*| md5

prop_metadata x =
  binaryCodecTripping x metadata

prop_seqHeader x =
  binaryCodecTripping x seqHeader

return []
tests :: IO Bool
tests = $quickCheckAll
