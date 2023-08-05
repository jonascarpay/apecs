{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Apecs.TH
  ( makeWorld
  , makeWorldNoEC
  , makeWorldAndComponents
  , makeMapComponents
  , makeMapComponentsFor
  ) where

import           Control.Monad
import           Control.Monad.State (modify)
import           Language.Haskell.TH

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util          (EntityCounter)
import Control.Monad.Identity (Identity(runIdentity))

genName :: String -> Q Name
genName s = mkName . show <$> newName s

-- | Same as 'makeWorld', but does not include an 'EntityCounter'
--   You don't typically want to use this, but it's exposed in case you know what you're doing.
makeWorldNoEC :: String -> [Name] -> Q [Dec]
makeWorldNoEC worldName cTypes = do
  cTypesNames <- forM cTypes $ \t -> do
    rec <- genName "rec"
    return (ConT t, rec)

  let wld = mkName worldName
      has = mkName "Has"
      sys = mkName "SystemT"
      m = VarT $ mkName "m"
      wldDecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t,n) = (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
      records = makeRecord <$> cTypesNames

      makeInstance (t,n) =
        InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` t)
          [ FunD (mkName "getStore") [Clause []
              (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
            [] ]
          , FunD (mkName "setStore") [let sN = mkName "s" in Clause [VarP sN]
              -- modify (\w -> w { n = s })
              (NormalB$ AppE
                (VarE 'modify)
                (let wN = mkName "w" in LamE [VarP wN] (RecUpdE (VarE wN) [(n, VarE sN)]))
              )
            [] ]
          ]




      -- initMyWorldM :: forall m . (ExplInit m FieldA, ExplInit m FieldB, ...) => m MyWorld
      initSig' functionName typeVars monadT f = SigD functionName
        (let explInitT = ConT ''ExplInit
          in ForallT
            typeVars
            (ConT ''Monad `AppT` monadT : [explInitT `AppT` monadT `AppT` rT | (_, _, rT) <- records])
            (f (ConT wld))
        )
      -- initMyWorldM = return MyWorld <*> explInit <*> ... <*> explInit
      initDecl' functionName f = FunD functionName [Clause []
        (NormalB$ f $ iterate (\wE -> AppE (AppE (VarE $ mkName "<*>") wE) (VarE $ mkName "explInit")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
        [] ]



      initWorldMName = mkName $ "init" ++ worldName ++ "M"
      initMSig = let mN = mkName "m" in initSig' initWorldMName [PlainTV mN] (VarT mN) (AppT (VarT mN))
      initMDecl = initDecl' initWorldMName id

      initWorldName = mkName $ "init" ++ worldName
      initSig = initSig' initWorldName [] (ConT ''Identity) id
      initDecl = initDecl' initWorldName (AppE (VarE 'runIdentity))

      hasDecl = makeInstance <$> cTypesNames


  return
    $ wldDecl
    : initMSig
    : initMDecl
    : initSig
    : initDecl
    : hasDecl

-- | Creates 'Component' instances with 'Map' stores
makeMapComponents :: [Name] -> Q [Dec]
makeMapComponents = mapM makeMapComponent

makeMapComponent :: Name -> Q Dec
makeMapComponent = makeMapComponentFor ''Map

-- | Allows customization of the store to be used. For example, the base 'Map' or an STM Map.
makeMapComponentFor :: Name -> Name -> Q Dec
makeMapComponentFor store comp = do
  let ct = pure $ ConT comp
      st = pure $ ConT store
  head <$> [d| instance Component $ct where type Storage $ct = $st $ct |]

makeMapComponentsFor :: Name -> [Name] -> Q [Dec]
makeMapComponentsFor store = mapM (makeMapComponentFor store)

-- | Calls 'makeWorld' and 'makeMapComponents', i.e. makes a world and also defines 'Component' instances with a 'Map' store.
makeWorldAndComponents :: String -> [Name] -> Q [Dec]
makeWorldAndComponents worldName cTypes = do
  wdecls <- makeWorld worldName cTypes
  cdecls <- makeMapComponents cTypes
  return $ wdecls ++ cdecls

{-|

The typical way to create a @world@ record, associated 'Has' instances, and initialization function.

> makeWorld "MyWorld" [''Component1, ''Component2, ...]

turns into

> data MyWorld = MyWorld Component1 Component2 ... EntityCounter
> instance MyWorld `Has` Component1 where ...
> instance MyWorld `Has` Component2 where ...
> ...
> instance MyWorld `Has` EntityCounter where ...
>
> initMyWorld :: IO MyWorld
> initMyWorld = MyWorld <$> initStore <*> initStore <*> ... <*> initStore

-}
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = makeWorldNoEC worldName (cTypes ++ [''EntityCounter])
