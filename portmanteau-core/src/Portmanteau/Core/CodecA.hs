{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Portmanteau.Core.CodecA (
    Codec (..)
  , (|*|)
  , (*|)
  , (|*)
  , (|+|)
  , newCodec
  ) where

import           Control.Arrow (Arrow (..), ArrowChoice (..), Kleisli (..), (>>>), (^>>), (>>^))
import           Control.Category (Category (..))

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..), chosen, divided)
import           Data.Profunctor (Profunctor (..))

import           P hiding ((.), id)


data Codec f g a b =
  Codec {
      codecAEncoder :: f b a
    , codecADecoder :: g a b
    }


instance (Category f, Category g) => Category (Codec f g) where
  id =
    Codec id id
  (.) (Codec cb bc) (Codec ba ab) =
    Codec (ba . cb) (bc . ab)


(|*|) :: (Arrow f, Arrow g, Monoid c) => Codec f g c a -> Codec f g c b -> Codec f g c (a, b)
(|*|) (Codec f0 g0) (Codec f1 g1) =
  Codec (f0 ### f1) (g0 &&& g1)
infixl 5 |*|

(*|) :: (Arrow f, Arrow g, Monoid c) => Codec f g c () -> Codec f g c b -> Codec f g c b
(*|) (Codec f0 g0) (Codec f1 g1) =
  Codec ((\b -> ((), b)) ^>> f0 ### f1) (g0 &&& g1 >>^ snd)
infixl 6 *|

(|*) :: (Arrow f, Arrow g, Monoid c) => Codec f g c a -> Codec f g c () -> Codec f g c a
(|*) (Codec f0 g0) (Codec f1 g1) =
  Codec ((\a -> (a, ())) ^>> f0 ### f1) (g0 &&& g1 >>^ fst)
infixl 6 |*

(|+|) :: (ArrowChoice f, ArrowChoice g) => Codec f g c a -> Codec f g c b -> Codec f g c (Either a b)
(|+|) (Codec f0 g0) (Codec f1 g1) =
  -- TODO The decoder doesn't look right (ho ho) here
  Codec (f0 ||| f1) (arr Right >>> g0 +++ g1)
infixl 4 |+|

-- FIX Should this live in Arrow?
(###) :: (Arrow a, Monoid c) => a b c -> a b' c -> a (b, b') c
(###) f g =
  f *** g >>> arr (\(a, b) -> a <> b)
infixl 3 ###

newCodec :: Arrow a => (c -> b) -> (b -> m c) -> Codec a (Kleisli m) b c
newCodec a b =
  Codec (arr a) (Kleisli b)
