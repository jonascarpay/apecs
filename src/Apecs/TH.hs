{-# LANGUAGE TemplateHaskell #-}

module Apecs.TH
  ( makeWorld, makeWorldNoEC, makeWorldAndComponents
  ) where

import           Language.Haskell.TH
import           Control.Monad

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util          (EntityCounter)

genName :: String -> Q Name
genName s = mkName . show <$> newName s

-- | Same as 'makeWorld', but has no 'EntityCounter'
makeWorldNoEC :: String -> [Name] -> Q [Dec]
makeWorldNoEC worldName cTypes = do
  cTypesNames <- forM cTypes $ \t -> do
    rec <- genName "rec"
    return (ConT t, rec)

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

      initWorldName = mkName $ "init" ++ worldName
      initSig = SigD initWorldName (AppT (ConT (mkName "IO")) (ConT wld))
      initDecl = FunD initWorldName [Clause []
        (NormalB$ iterate (\wE -> AppE (AppE (VarE $ mkName "<*>") wE) (VarE $ mkName "initStore")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
        [] ]

      hasDecl = makeInstance <$> cTypesNames

  return $ wldDecl : initSig : initDecl : hasDecl

makeComponent :: Name -> Q Dec
makeComponent comp = do
  let ct = return$ ConT comp
  head <$> [d| instance Component $ct where type Storage $ct = Map $ct |]
  
-- | Same as makeWorld, but also makes a component instance:
makeWorldAndComponents :: String -> [Name] -> Q [Dec]
makeWorldAndComponents worldName cTypes = do
  wdecls <- makeWorld worldName cTypes
  cdecls <- mapM makeComponent cTypes
  return $ wdecls ++ cdecls
  
{-|

> makeWorld "WorldName" [''Component1, ''Component2, ...]

turns into

> data WorldName = WorldName Component1 Component2 ... EntityCounter
> instance WorldName `Has` Component1 where ...
> instance WorldName `Has` Component2 where ...
> ...
> instance WorldName `Has` EntityCounter where ...
>
> initWorldName :: IO WorldName
> initWorldName = WorldName <$> initStore <*> initStore <*> ... <*> initStore

|-}
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = makeWorldNoEC worldName (cTypes ++ [''EntityCounter])
