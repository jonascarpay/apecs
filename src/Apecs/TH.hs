{-# LANGUAGE TemplateHaskell #-}

module Apecs.TH
  ( makeWorld, makeWorldWithCounter
  )where

import Language.Haskell.TH
import Control.Monad

import Apecs.Util (EntityCounter)

genName :: String -> Q Name
genName s = mkName . show <$> newName s

{-|

> makeWorld "WorldName" [''Component1, ''Component2]

turns into

> data WorldName = WorldName ...
> instance WorldName `Has` Component1 where ...
> instance WorldName `Has` Component2 where ...

|-}
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = do
  cTypesNames <- mapM (\t -> do rec <- genName "rec"; return (ConT t, rec)) cTypes

  let wld = mkName worldName
      has = mkName "Has"
      sys = mkName "System"
      wldDecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t,n) = (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
      records = makeRecord <$> cTypesNames

      makeInstance (t,n) =
        InstanceD Nothing [] ((ConT has `AppT` ConT wld) `AppT` t)
          [ FunD (mkName "getStore") [Clause []
              (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
            [] ]
          ]

      initDecl = FunD (mkName $ "init" ++ worldName) [Clause []
        (NormalB$ iterate (\wE -> AppE (AppE (VarE $ mkName "(<*>)") wE) (VarE $ mkName "initStore")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
        [] ]

      hasDecl = makeInstance <$> cTypesNames

  return $ wldDecl : initDecl : hasDecl

-- | Same as 'makeWorld', but adds an 'EntityCounter'
makeWorldWithCounter :: String -> [Name] -> Q [Dec]
makeWorldWithCounter worldName cTypes = makeWorld worldName (cTypes ++ [''EntityCounter])

tupleInstances :: Int -> Q [Dec]
tupleInstances n = do
  vars <- replicateM n (newName "x")
  let strgT var = ConT (mkName "Storage") `AppT` VarT var
      storageInst = InstanceD Nothing (strgT <$> vars)
  undefined

