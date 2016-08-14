{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Aeson2 (
    JsonCodec
  , JsonObjectCodec
  , jsonCodec
  , jsonObjectCodec
  , (.|)
  , field
  , jsonCodecEncode
  , jsonCodecDecode
  ) where

import           Control.Arrow
import           Control.Monad.Trans.Reader (ReaderT (..))

import           Data.Aeson (FromJSON (..), ToJSON (..), Object, Value (..))
import           Data.Aeson.Types (Parser, parseEither, typeMismatch)
import           Data.Functor.Contravariant (Op (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           P

import           Portmanteau.Core.CodecA


type ParserK = Kleisli Parser

-- | Keep in mind that `Value` is _not_ a `Monoid`, which means most `Codec` operations won't work.
type JsonCodec = Codec (->) ParserK Value

-- | `Object` is a `Monoid`, which is required for `|*|`. `Value` is not.
type JsonObjectCodec = Codec (->) ParserK Object


jsonCodec :: (ToJSON a, FromJSON a) => JsonCodec a
jsonCodec =
  newCodec toJSON parseJSON

field :: Text -> JsonObjectCodec Value
field t =
  newCodec
    (HM.singleton t)
    (maybe (fail $ "Not found " <> T.unpack t) return . HM.lookup t)

jsonObjectCodec :: Text -> JsonCodec a -> JsonObjectCodec a
jsonObjectCodec t c = let
  (e, d) = case c of Codec e' (Kleisli d') -> (e', d')
  in newCodec
    (HM.singleton t . ($) e)
    (maybe (fail $ "Not found " <> T.unpack t) (($) d) . HM.lookup t)

(.|) :: Text -> JsonCodec a -> JsonObjectCodec a
(.|) =
  jsonObjectCodec
infixl 8 .|


jsonCodecEncode :: JsonCodec a -> a -> Value
jsonCodecEncode c =
  codecAEncoder c

jsonCodecDecode :: JsonCodec a -> Value -> Either [Char] a
jsonCodecDecode c =
  parseEither (runKleisli (codecADecoder c))
