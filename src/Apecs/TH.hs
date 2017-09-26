{-# LANGUAGE TemplateHaskell #-}

module Apecs.TH where

import Control.Monad.Reader
import Language.Haskell.TH

import Apecs.Types (Has(..))
import Debug.Trace

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)

  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where
    mkCurryDec ith = do
      cury <- curryN ith
      let name = mkName $ "curry" ++ show ith
      return $ FunD name [Clause [] (NormalB cury) []]

{-mapN :: Int -> Q Dec-}
{-mapN n-}
  {-| n >= 1 = funD name [cl1, cl2]-}
  {-| otherwise = fail "AAAA"-}
  {-where-}
    {-name = mkName $ "map" ++ show n-}
    {-cl1 = do-}
      {-f <- newName "f"-}
      {-xs <- replicateM n (newName "x")-}
      {-ys <- replicateM n (newName "ys")-}
      {-let argPatts = varP f : consPatts-}
          {-consPatts = [ [p| $(varP x) : $(varP ys) |] | (x,ys) <- zip xs ys]-}
          {-apply = foldl (\ f x -> [| $f $(varE x) |])-}
          {-first = apply (varE f) xs-}
          {-rest = apply (varE name) (f:ys)-}
      {-clause argPatts (normalB [| $first : $rest |]) []-}
    {-cl2 = clause (replicate (n+1) wildP) (normalB (conE '[])) []-}

makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = do

  cTypesNames <- mapM (\t -> do rec <- newName "rec"; return (ConT t, rec)) cTypes

  let wld = mkName worldName
      has = mkName "Has"
      sys = mkName "System"
      wdecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t,n) = (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
      records = makeRecord <$> cTypesNames

      makeInstance (t,n) =
        InstanceD Nothing [] ((ConT has `AppT` ConT wld) `AppT` t)
          [ FunD (mkName "getStore") [Clause []
              (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
            [] ]
          ]

      hasDecl = makeInstance <$> cTypesNames

  return $ wdecl : hasDecl
