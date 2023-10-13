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
import           Control.Monad.Reader (asks)
import           Language.Haskell.TH

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util           (EntityCounter)

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
      has = ''Has
      sys = 'SystemT
      m = VarT $ mkName "m"
      wldDecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t,n) = (n, Bang NoSourceUnpackedness SourceStrict, ConT ''Storage `AppT` t)
      records = makeRecord <$> cTypesNames

      makeInstance (t,n) =
        InstanceD Nothing [ConT ''Monad `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` t)
          [ FunD 'getStore [Clause []
              (NormalB$ ConE sys `AppE` (VarE 'asks `AppE` VarE n))
            [] ]
          ]

      initWorldName = mkName $ "init" ++ worldName
      initSig = SigD initWorldName (AppT (ConT ''IO) (ConT wld))
      initDecl = FunD initWorldName [Clause []
        (NormalB$ iterate (\wE -> AppE (AppE (VarE '(<*>)) wE) (VarE 'explInit)) (AppE (VarE 'return) (ConE wld)) !! length records)
        [] ]

      hasDecl = makeInstance <$> cTypesNames

  return $ wldDecl : initSig : initDecl : hasDecl

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
