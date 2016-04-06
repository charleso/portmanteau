-- This file is part of portmanteau.  It contains modified code from
-- fresnel, which contains modified code from roundtrip, which
-- contains modified code from Tillmann Rendel's
-- partial-isomorphisms and invertible-syntax packages.
--
-- Copyright (c) 2010-11 University of Marburg
-- Copyright (c) 2011 factis research GmbH
-- Copyright (C) 2015 Fraser Tweedale
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of factis research GmbH nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Portmanteau.Lens.TH (
    makeIso
  , makeIsoWith
  -- | Re-exports from 'Control.Lens'
  , Iso'
  , iso
  ) where

import          Control.Lens

import          Data.List (replicate, zipWith)
import          Data.String (String)

import          Language.Haskell.TH

import          P hiding (exp)


-- | Make an 'Iso' for the named type.
--
-- Product types are isomorphic to left-leaning nested tuples.
-- Sum types are isomorphic to a left-leaning nested 'Either'
-- structure.
--
-- The name of the 'Iso' is the type name prepended with a '_'.
--
makeIso :: Name -> Q [Dec]
makeIso =
  makeIsoWith ('_':)

-- | Make an 'Iso' with an explicit function to derive its name.
--
makeIsoWith :: (String -> String) -> Name -> Q [Dec]
makeIsoWith f d = do
  info <- reify d

  (dname, cs) <-
    case info of
      TyConI (DataD _ name _ cs' _) ->
        pure (name, cs')
      TyConI (NewtypeD _ name _ c _) ->
        pure (name, [c])
      _ ->
        fail $ show d <> " neither denotes a data or newtype declaration. Found: " <> show info

  let
    name =
      mkName . f $ nameBase dname

  sig <- signatureOfData name dname cs
  body <- bodyOfData name cs

  return [sig, body]

signatureOfData :: Name -> Name -> [Con] -> Q Dec
signatureOfData name dname cs =
  sigD name $
    conT (mkName "Iso'") `appT` typeOfData cs `appT` conT dname

typeOfData :: [Con] -> TypeQ
typeOfData cs =
  case unsnoc cs of
    Nothing ->
      fail "makeIso not available for zero-constructor types"
    Just ([], x) ->
      typeOfConstructor x
    Just (xs, x) ->
      conT (mkName "Either") `appT` typeOfData xs `appT` typeOfConstructor x

typeOfConstructor :: Con -> TypeQ
typeOfConstructor = \case
  NormalC _ fields ->
    return $ typeOfFields (fmap (view _2) fields)
  RecC _ fields ->
    return $ typeOfFields (fmap (view _3) fields)
  InfixC (_,t1) _ (_,t2) ->
    return $ typeOfFields [t1, t2]
  ForallC {} ->
    fail "makeIso not available for existential data constructors"

typeOfFields :: [Type] -> Type
typeOfFields ts =
  case unsnoc ts of
    Nothing ->
      TupleT 0
    Just ([], x) ->
      x
    Just (xs, x) ->
      TupleT 2 `AppT` typeOfFields xs `AppT` x

bodyOfData :: Name -> [Con] -> Q Dec
bodyOfData name cs = do
  let
    n =
      length cs

    exp =
      letE [
          fun (mkName "decode") . lamCaseE $
            zipWith (decodeOfConstructor n) [0..] cs

        , fun (mkName "encode") . lamCaseE $
            zipWith (encodeOfConstructor n) [0..] cs
        ] $
      varE (mkName "iso") `appE`
        varE (mkName "decode") `appE`
        varE (mkName "encode")

  fun name exp

decodeOfConstructor :: Int -> Int -> Con -> MatchQ
decodeOfConstructor n i con = do
  (pats, exps) <- newNamesPE =<< sizeOfConstructor con

  let
    pat =
      nestedSum (\s -> conP (mkName s) . (:[])) n i (nestedProduct tupP pats)

    exp =
      foldl appE (conE (nameOfConstructor con)) exps

  match pat (normalB exp) []

encodeOfConstructor :: Int -> Int -> Con -> MatchQ
encodeOfConstructor n i con = do
  (pats, exps) <- newNamesPE =<< sizeOfConstructor con

  let
    pat =
      conP (nameOfConstructor con) pats

    exp =
      nestedSum (appE . conE . mkName) n i (nestedProduct tupE exps)

  match pat (normalB exp) []

nestedSum :: (String -> t -> t) -> Int -> Int -> t -> t
nestedSum f n i pat =
  if n == 1 then
    pat
  else
    foldr (.) id (replicate (n - i - 1) (f "Left")) $
      if i > 0 then
        f "Right" pat
      else
        pat

nestedProduct :: ([t] -> t) -> [t] -> t
nestedProduct tup ts =
  case unsnoc ts of
    Nothing ->
      tup []
    Just ([], x) ->
      x
    Just (xs, x) ->
      tup [nestedProduct tup xs, x]

nameOfConstructor :: Con -> Name
nameOfConstructor = \case
  NormalC cname _ ->
    cname
  RecC cname _ ->
    cname
  InfixC _ cname _ ->
    cname
  ForallC _ _ con ->
    nameOfConstructor con

sizeOfConstructor :: Con -> Q Int
sizeOfConstructor = \case
  NormalC _ fields ->
    pure $ length fields
  RecC _ fields ->
    pure $ length fields
  InfixC _ _ _ ->
    pure 2
  ForallC _ _ _ ->
    fail "makeIso not available for existential data constructors"

newNamesPE :: Int -> Q ([PatQ], [ExpQ])
newNamesPE number = do
  ids <- replicateM number (newName "x")
  return (fmap varP ids, fmap varE ids)

fun :: Name -> ExpQ -> DecQ
fun name exp =
  funD name [clause [] (normalB exp) []]
