{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Portmanteau.Lens (
    module X
  , (|$|)
  , (|&|)
  , _Bool
  ) where

import          Control.Arrow (Arrow, (^>>), (>>^))
import          Control.Lens (AnIso, cloneIso, view, from, under)

import          Data.Profunctor (Profunctor (..))

import          P

import          Portmanteau.Core.CodecA
import          Portmanteau.Lens.TH as X


-- FIX Is this defined in `lens` already?!?
(|$|) :: Profunctor p => AnIso c c a a -> p c c -> p a a
(|$|) i =
  dimap (view . from $ i) (view . cloneIso $ i)
infixl 3 |$|

(|&|) :: (Arrow f, Arrow g) => AnIso c c a a -> Codec f g b c -> Codec f g b a
(|&|) i (Codec e d) =
  Codec
    ((view . from $ i) ^>> e)
    (d >>^ (view . cloneIso $ i))
infixl 3 |&|

-- FIX Expose more of these probably
makeIso ''Bool
