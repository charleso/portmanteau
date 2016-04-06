{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Portmanteau.Syntax.Json where

import           Data.Functor.Contravariant (Op (..))
import qualified Data.Text as T

import           P

import           Portmanteau.Core
import           Portmanteau.Lens
import           Portmanteau.Syntax

import           Test.QuickCheck (Arbitrary (..), Gen, oneof)
import qualified Test.QuickCheck as QC

import           Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import qualified Text.PrettyPrint as PP


data JsonValue =
    JsonBool Bool
  | JsonNull
  | JsonNumber Rational
  | JsonString [Char]
  | JsonArray [JsonValue]
  | JsonObject [([Char], JsonValue)]
  deriving (Show, Eq)
makeIso ''JsonValue

instance Arbitrary JsonValue where
  arbitrary =
    oneof [
        pure JsonNull
      , JsonBool <$> arbitrary
      , JsonNumber <$> arbitrary
      , JsonString <$> arbitrary
      , JsonArray <$> smaller arbitrary
      , JsonObject <$> smaller arbitrary
      ]



jsonSyntaxCodec :: (CharParsing m, Monad m) => SyntaxCodec m JsonValue
jsonSyntaxCodec =
  _JsonValue
    |$| jsonBoolSyntax
    ||| jsonNullSyntax
    ||| jsonNumberSyntax
    ||| jsonStringSyntax
    ||| jsonArraySyntax
    ||| jsonObjectSyntax


jsonBoolSyntax :: CharParsing m => SyntaxCodec m Bool
jsonBoolSyntax =
  _Bool
    |$| literal "false"
    ||| literal "true"

jsonNullSyntax :: CharParsing m => SyntaxCodec m ()
jsonNullSyntax =
  literal "null"

-- FIX Doesn't handle exponentials
jsonNumberSyntax :: (CharParsing m, Monad m) => SyntaxCodec m Rational
jsonNumberSyntax =
  Codec (Op numberPrinter) numberParser
  where
    numberPrinter r =
      PP.integer (numerator r) <> "." <> PP.integer (denominator r)
    numberParser =
      maybe (P.unexpected "Invalid number") pure =<<
        (\a b c -> (%) <$> readMaybe (a <> b) <*> readMaybe c)
          <$> P.option "" (P.string "-")
          <*> P.some P.digit
          <*> P.option "" (P.char '.' *> P.some P.digit)

jsonStringSyntax :: CharParsing m => SyntaxCodec m [Char]
jsonStringSyntax =
  between
    (literal "\"")
    (literal "\"")
    -- FIX Not handling hex values and other real things here, just because I'm lazy
    (many' (escaped '\\' ['"']))

jsonArraySyntax :: (CharParsing m, Monad m) => SyntaxCodec m [JsonValue]
jsonArraySyntax =
  betweenSepbyComma '[' ']' jsonSyntaxCodec

jsonObjectSyntax :: (CharParsing m, Monad m) => SyntaxCodec m [([Char], JsonValue)]
jsonObjectSyntax =
  betweenSepbyComma '{' '}' $
    jsonStringSyntax |* literal ":" |*| jsonSyntaxCodec


betweenSepbyComma :: CharParsing m => Char -> Char -> SyntaxCodec m a -> SyntaxCodec m [a]
betweenSepbyComma l r pa =
  between
    (literal $ T.singleton l)
    (literal $ T.singleton r)
    (sepBy pa (literal ","))


smaller :: Gen a -> Gen a
smaller g =
  QC.sized $ \s -> QC.resize (s `div` 2) g
