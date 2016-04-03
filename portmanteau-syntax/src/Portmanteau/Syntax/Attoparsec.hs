{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Syntax.Attoparsec (
    module X
  , AttoparsecCodec
  , attoparsecCodecDecode
  ) where

import qualified Data.Attoparsec.Text as A

import           P

import           Portmanteau.Core
import           Portmanteau.Syntax as X


type AttoparsecCodec a = SyntaxCodec A.Parser a


attoparsecCodecDecode :: SyntaxCodec A.Parser b -> Text -> Either [Char] b
attoparsecCodecDecode c =
  A.parseOnly (codecDecoder c)
