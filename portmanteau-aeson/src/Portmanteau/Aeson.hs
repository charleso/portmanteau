{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Aeson (
    JsonCodec
  , JsonObjectCodec
  , jsonCodec
  , jsonObjectCodec
  , (.|)
  , liftJsonObjectCodec
  , jsonCodecEncode
  , jsonCodecDecode
  ) where

import           Control.Monad.Trans.Reader (ReaderT (..))

import           Data.Aeson (FromJSON (..), ToJSON (..), Object, Value (..))
import           Data.Aeson.Types (Parser, parseEither, typeMismatch)
import           Data.Functor.Contravariant (Op (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           P

import           Portmanteau.Core


-- | Keep in mind that `Value` is _not_ a `Monoid`, which means most `Codec` operations won't work.
type JsonCodec = ParserCodec Value Parser

-- | `Object` is a `Monoid`, which is required for `Divisible`. `Value` is not.
type JsonObjectCodec = ParserCodec Object Parser


jsonCodec :: (ToJSON a, FromJSON b) => JsonCodec a b
jsonCodec =
  parserCodec toJSON parseJSON

jsonObjectCodec :: Text -> JsonCodec a b -> JsonObjectCodec a b
jsonObjectCodec t c = let
  (e, d) = case c of Codec (Op e') (ReaderT d') -> (e', d')
  in parserCodec
    (HM.singleton t . ($) e)
    (maybe (fail $ "Not found " <> T.unpack t) (($) d) . HM.lookup t)

(.|) :: Text -> JsonCodec a b -> JsonObjectCodec a b
(.|) =
  jsonObjectCodec
infixl 8 .|

liftJsonObjectCodec :: JsonObjectCodec a b -> JsonCodec a b
liftJsonObjectCodec (Codec f g) =
  Codec
    (Op $ Object . getOp f)
    (ReaderT $ \v -> case v of Object o -> runReaderT g o; x -> typeMismatch "Invalid object" x)


jsonCodecEncode :: JsonCodec a b -> a -> Value
jsonCodecEncode c =
  getOp (codecEncoder c)

jsonCodecDecode :: JsonCodec a b -> Value -> Either [Char] b
jsonCodecDecode c =
  parseEither (runReaderT (codecDecoder c))
