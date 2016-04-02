{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Core.Parser (
    ParserCodec
  , parserCodec
  ) where

import           Control.Monad.Trans.Reader (ReaderT (..))

import           Data.Functor.Contravariant (Op (..))

import           Portmanteau.Core.Codec


-- | A common pattern for parsing to/from something
type ParserCodec s m = Codec' (Op s) (ReaderT s m)


parserCodec :: (a -> s) -> (s -> m b) -> ParserCodec s m a b
parserCodec f g =
  Codec (Op f) (ReaderT g)
