{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Portmanteau.Core.Codec (
    codecLaws
  , codecLaws'
  , codecLawsK
  ) where

import           Control.Arrow (Kleisli (..))

import           P

import           Portmanteau.Core.CodecA

import           System.IO (IO)

import           Test.QuickCheck (Arbitrary (..), Gen, Property, (===), forAll, quickCheckAll)


-- FIX Add category laws as well?

codecLaws ::
  (Applicative h, Arbitrary b, Eq (h b), Show b, Show (h b)) =>
  Codec f g a b ->
  (f b a -> b -> a) ->
  (g a b -> a -> h b) ->
  Property
codecLaws c e' d' =
  codecLaws' arbitrary c e' d'

codecLaws' ::
  (Applicative h, Eq (h b), Show b, Show (h b)) =>
  Gen b ->
  Codec f g a b ->
  (f b a -> b -> a) ->
  (g a b -> a -> h b) ->
  Property
codecLaws' g (Codec e d) e' d' =
  forAll g $ \b ->
    d' d (e' e b) === pure b

codecLawsK ::
  (Applicative f, Eq (f b), Show b, Show (f b)) =>
  Gen b ->
  Codec (->) (Kleisli g) a b ->
  ((a -> g b) -> a -> f b) ->
  Property
codecLawsK g c f =
  codecLaws' g c ($) (f . runKleisli)


return []
tests :: IO Bool
tests = $quickCheckAll
