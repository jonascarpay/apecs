{-# LANGUAGE TemplateHaskell #-}

module Apecs.THTuples where

import Language.Haskell.TH

import Control.Monad

tupleInstances :: Int -> Q [Dec]
tupleInstances n = do
  let vars = [ VarT . mkName $ "t_" ++ show i | i <- [0..n-1]]
      tupleUpT = foldl AppT (TupleT n)
      varTuple = tupleUpT vars
      tuplN = tupleDataName n
      tuplE = ConE tuplN
      pureTuplE = AppE (VarE (mkName "pure")) tuplE

      compN = mkName "Component"
      compT var = ConT compN `AppT` var
      strgN = mkName "Storage"
      strgT var = ConT strgN `AppT` var
      compI = InstanceD Nothing (fmap compT vars) (compT varTuple)
        [ TySynInstD strgN $
          TySynEqn [varTuple] (tupleUpT . fmap strgT $ vars)
        ]

      hasN = mkName "Has"
      hasT var = ConT hasN `AppT` VarT (mkName "w") `AppT` var
      getStoreN = mkName "getStore"
      getStoreE = VarE getStoreN
      apN = mkName "<*>"
      apE = VarE apN
      hasI = InstanceD Nothing (hasT <$> vars) (hasT varTuple)
        [ FunD getStoreN
          [Clause [] (NormalB$ foldl (\t s -> AppE (AppE apE t) s) pureTuplE (replicate n $ getStoreE )) [] ]
        , PragmaD$ InlineP getStoreN Inline FunLike AllPhases
        ]

  return [compI, hasI]
