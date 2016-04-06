{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Portmanteau.Lens (
    module X
  , (|$|)
  , _Bool
  ) where

import          Control.Lens (AnIso, cloneIso, view, from)

import          Data.Profunctor (Profunctor (..))

import          P

import          Portmanteau.Lens.TH as X


-- FIX Is this defined in `lens` already?!?
(|$|) :: Profunctor p => AnIso c c a a -> p c c -> p a a
(|$|) i =
  dimap (view . from $ i) (view . cloneIso $ i)
infixl 3 |$|

-- FIX Expose more of these probably
makeIso ''Bool
