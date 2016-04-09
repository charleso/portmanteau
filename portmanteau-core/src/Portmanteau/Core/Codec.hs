{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Portmanteau.Core.Codec (
    Codec' (..)
  , Codec
  , (|*|)
  , (*|)
  , (|*)
  , (|||)
  ) where

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..), chosen, divided)
import           Data.Profunctor (Profunctor (..))

import           P


data Codec' f g a b =
  Codec {
      codecEncoder :: f a
    , codecDecoder :: g b
    } deriving (Functor)

type Codec f g a = Codec' f g a a


instance (Contravariant f, Functor g) => Profunctor (Codec' f g) where
  dimap a b (Codec f g) =
    Codec (contramap a f) (fmap b g)


(|*|) :: (Divisible f, Applicative g) => Codec' f g a c -> Codec' f g b d -> Codec' f g (a, b) (c, d)
(|*|) (Codec f0 g0) (Codec f1 g1) =
  Codec (divided f0 f1) ((,) <$> g0 <*> g1)
infixl 5 |*|

(*|) :: (Divisible f, Applicative g) => Codec' f g () b -> Codec' f g c d -> Codec' f g c d
(*|) (Codec f0 g0) (Codec f1 g1) =
  Codec (divide ((),) f0 f1) (g0 *> g1)
infixl 6 *|

(|*) :: (Divisible f, Applicative g) => Codec' f g a b -> Codec' f g () d -> Codec' f g a b
(|*) (Codec f0 g0) (Codec f1 g1) =
  Codec (divide (,()) f0 f1) (g0 <* g1)
infixl 6 |*

(|||) :: (Decidable f, Alternative g) => Codec' f g a c -> Codec' f g b d -> Codec' f g (Either a b) (Either c d)
(|||) (Codec f0 g0) (Codec f1 g1) =
  Codec (f0 `chosen` f1) (Left <$> g0 <|> Right <$> g1)
infixl 4 |||
