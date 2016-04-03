{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Syntax.Attoparsec where

import           P

import           Portmanteau.Syntax.Attoparsec

import           System.IO (IO)

import           Test.Portmanteau.Syntax.Json
import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()



prop_syntax_json a =
  attoparsecCodecTripping a jsonSyntaxCodec


attoparsecCodecTripping :: (Eq a, Show a) => a -> AttoparsecCodec a -> Property
attoparsecCodecTripping a c =
  attoparsecCodecDecode c (syntaxCodecEncode c a) === pure a


return []
tests :: IO Bool
tests = $quickCheckAll
