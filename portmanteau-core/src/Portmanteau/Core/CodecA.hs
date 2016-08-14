{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Portmanteau.Core.CodecA (
    Codec (..)
  , CodecM
  , (|*|)
  , (*|)
  , (|*)
  , (|+|)
  , cmap
  , newCodec
  , mapDecoding
  , runDecoderK
  ) where

import           Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), Kleisli (..), (>>>), (^>>), (>>^), (<+>))
import           Control.Category (Category (..))

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..), chosen, divided)
import           Data.Profunctor (Profunctor (..))

import           P hiding ((.), id)


data Codec f g a b =
  Codec {
      codecEncoder :: f b a
    , codecDecoder :: g a b
    }

-- | The most common form of Codec
type CodecM m = Codec (->) (Kleisli m)


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

(|+|) :: (ArrowChoice f, ArrowPlus g) => Codec f g c a -> Codec f g c b -> Codec f g c (Either a b)
(|+|) (Codec f0 g0) (Codec f1 g1) =
  Codec (f0 ||| f1) (arr Left . g0 <+> arr Right . g1)
infixl 4 |+|

-- FIX Should this live in Arrow?
(###) :: (Arrow a, Monoid c) => a b c -> a b' c -> a (b, b') c
(###) f g =
  f *** g >>> arr (\(a, b) -> a <> b)
infixl 3 ###

cmap :: (Arrow f, Arrow g) => (a -> c) -> (c -> a) -> Codec f g b c -> Codec f g b a
cmap f g (Codec e d) =
  Codec (f ^>> e) (d >>^ g)

newCodec :: Arrow a => (c -> b) -> (b -> m c) -> Codec a (Kleisli m) b c
newCodec a b =
  Codec (arr a) (Kleisli b)

mapDecoding :: (forall x. m x -> n x) -> Codec f (Kleisli m) a b -> Codec f (Kleisli n) a b
mapDecoding f (Codec e (Kleisli d)) =
  Codec e (Kleisli . fmap f $ d)

runDecoderK :: Codec f (Kleisli g) a b -> a -> g b
runDecoderK (Codec _ (Kleisli g)) =
  g
