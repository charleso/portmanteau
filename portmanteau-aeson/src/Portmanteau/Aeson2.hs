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
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           P

import           Portmanteau.Core.CodecA


type ParserK = Kleisli Parser
type IdentityK = Kleisli Identity

-- | Keep in mind that `Value` is _not_ a `Monoid`, which means most `Codec` operations won't work.
type JsonCodec = Codec IdentityK ParserK Value

-- | `Object` is a `Monoid`, which is required for `|*|`. `Value` is not.
type JsonObjectCodec = Codec IdentityK ParserK Object


parserCodec :: Arrow a => (c -> b) -> (b -> m c) -> Codec a (Kleisli m) b c
parserCodec a b =
  Codec (arr a) (Kleisli b)

jsonCodec :: (ToJSON a, FromJSON a) => JsonCodec a
jsonCodec =
  Codec (arr toJSON) (Kleisli parseJSON)

field :: Text -> JsonObjectCodec Value
field t =
  parserCodec
    (HM.singleton t)
    (maybe (fail $ "Not found " <> T.unpack t) return . HM.lookup t)

jsonObjectCodec :: Text -> JsonCodec a -> JsonObjectCodec a
jsonObjectCodec t c = let
  (e, d) = case c of Codec (Kleisli e') (Kleisli d') -> (e', d')
  in parserCodec
    (HM.singleton t . runIdentity . ($) e)
    (maybe (fail $ "Not found " <> T.unpack t) (($) d) . HM.lookup t)

(.|) :: Text -> JsonCodec a -> JsonObjectCodec a
(.|) =
  jsonObjectCodec
infixl 8 .|


jsonCodecEncode :: JsonCodec a -> a -> Value
jsonCodecEncode c =
  runIdentity . runKleisli (codecAEncoder c)

jsonCodecDecode :: JsonCodec a -> Value -> Either [Char] a
jsonCodecDecode c =
  parseEither (runKleisli (codecADecoder c))
