{-# LANGUAGE NoImplicitPrelude #-}
module Portmanteau.Syntax (
    SyntaxCodec
  , syntaxCodecEncode
  , satisfy
  , symbol
  , many'
  , between
  , literal
  , sepBy
  , escaped
  ) where

import           Data.Functor.Contravariant (Op (..))
import qualified Data.Text as T

import           P

import           Portmanteau.Core

import           Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP


type SyntaxCodec m a = Codec' (Op Doc) m a a


syntaxCodecEncode :: SyntaxCodec s a -> a -> Text
syntaxCodecEncode c =
  T.pack . PP.render . getOp (codecEncoder c)


satisfy :: CharParsing m => (Char -> Bool) -> SyntaxCodec m Char
satisfy f =
  Codec (Op $ \c -> if f c then PP.char c else mempty) (P.satisfy f)

symbol :: CharParsing m => Char -> SyntaxCodec m Char
symbol c =
  satisfy (== c)

many' :: CharParsing m => SyntaxCodec m a -> SyntaxCodec m [a]
many' c =
  let (f, g) = case c of Codec (Op f') g' -> (f', g')
  in Codec (Op $ foldMap f) (many g)

between :: Applicative m => SyntaxCodec m () -> SyntaxCodec m () -> SyntaxCodec m a -> SyntaxCodec m a
between l r a =
  l *| a |* r

literal :: CharParsing m => Text -> SyntaxCodec m ()
literal t =
  Codec (Op $ \() -> PP.text $ T.unpack t) (void $ P.text t)

sepBy :: CharParsing m => SyntaxCodec m a -> SyntaxCodec m () -> SyntaxCodec m [a]
sepBy ca cs = let
  -- Need to expand like this to allow for recursive definitions of codec
  (ae, ad, se, sd) = case (ca, cs) of (Codec (Op ae') ad', Codec (Op se') sd') -> (ae', ad', se', sd')
  in
    Codec (Op $ \as -> mconcat $ PP.punctuate (se ()) (fmap ae as)) (P.sepBy ad sd)

escaped :: CharParsing m => Char -> [Char] -> SyntaxCodec m Char
escaped esc special =
  Codec
    (Op $ \c -> if elem c (esc : special) then PP.char esc <> PP.char c else PP.char c)
    ((P.char esc *> P.anyChar) <|> P.noneOf (esc : special))
