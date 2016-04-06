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

import          Data.List (replicate, init, last, unzip)
import          Data.String (String)

import          Language.Haskell.TH

import          P hiding (error)
import          Prelude (error)


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
  let
    (n, cs) = case info of
      TyConI (DataD _ n' _ cs' _) -> (n', cs')
      TyConI (NewtypeD _ n' _ c _) -> (n', [c])
      _ -> error $ show d
        <> " neither denotes a data or newtype declaration. Found: "
        <> show info
  let n' = rename f n
  sig <- sigFromName n' n cs
  body <- defFromName n' cs
  return [sig, body]

sigFromName :: Name -> Name -> [Con] -> Q Dec
sigFromName n' n cs = sigD n'
  (conT (mkName "Iso'") `appT` sigFromCons cs `appT` conT n)

sigFromCons :: [Con] -> TypeQ
sigFromCons = \case
  [] ->
    error "makeIso not available for zero-constructor types"
  [x] ->
    sigFromCon x
  xs -> do
    -- FIX foldr???
    let
      ys = init xs
      y = last xs
      eitherT = conT (mkName "Either")
    appT (appT eitherT (sigFromCons ys)) (sigFromCon y)

sigFromCon :: Con -> TypeQ
sigFromCon = \case
  NormalC _ fields ->
    return $ sigFromTypes (fmap (view _2) fields)
  RecC _ fields ->
    return $ sigFromTypes (fmap (view _3) fields)
  InfixC (_,t1) _ (_,t2) ->
    return $ sigFromTypes [t1, t2]
  ForallC {} ->
    fail "makeIso not available for existential data constructors"

sigFromTypes :: [Type] -> Type
sigFromTypes = \case
  [] ->
    TupleT 0
  [t] ->
    t
  ts ->
    let
      ys = init ts
      y = last ts
    in
      AppT (AppT (TupleT 2) (sigFromTypes ys)) y

defFromName :: Name -> [Con] -> Q Dec
defFromName n' cs = do
  let
    expr = varE (mkName "iso") `appE` varE (mkName "f") `appE` varE (mkName "g")
  wheres <- whereFromCons cs
  funD n' [clause [] (normalB expr) (fmap return wheres)]

whereFromCons :: [Con] -> Q [Dec]
whereFromCons cs = do
  (fs, gs) <- unzip <$> zipWithM (fgFromCon (length cs)) [0..] cs
  fDec <- funD (mkName "f") (fmap return fs)
  gDec <- funD (mkName "g") (fmap return gs)
  return [fDec, gDec]

fgFromCon :: Int -> Int -> Con -> Q (Clause, Clause)
fgFromCon nCons i (NormalC n fields) = fgClauses nCons i (length fields) n
fgFromCon nCons i (RecC n fields) = fgClauses nCons i (length fields) n
fgFromCon nCons i (InfixC _ n _) = fgClauses nCons i 2 n
fgFromCon _ _ (ForallC {}) =
  error "makeIso not available for existential data constructors"

fgClauses :: Int -> Int -> Int -> Name -> Q (Clause, Clause)
fgClauses nCons i nFields n = do
  (pats, exprs) <- genPE nFields
  let
    fPat = nestedSum (\s -> conP (mkName s) . (:[])) nCons i (nested tupP pats)
    gExp = nestedSum (appE . conE . mkName) nCons i (nested tupE exprs)
  fClause <- clause [fPat] (normalB [| $(foldl appE (conE n) exprs) |]) []
  gClause <- clause [conP n pats] (normalB gExp) []
  return (fClause, gClause)

rename :: (String -> String) -> Name -> Name
rename f = mkName . f . nameBase

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE number = do
  ids <- replicateM number (newName "x")
  return (fmap varP ids, fmap varE ids)

nestedSum :: (String -> t -> t) -> Int -> Int -> t -> t
nestedSum _ 1 _ pat = pat
nestedSum f n i pat =
  foldr (.) id
  (replicate (n - i - 1) (f "Left"))
  (if i > 0 then f "Right" pat else pat)

nested :: ([t] -> t) -> [t] -> t
nested tup = \case
  [] ->
    tup []
  [x] ->
    x
  xs ->
    let
      ys = init xs
      y = last xs
    in
      tup [nested tup ys, y]
