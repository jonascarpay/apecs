{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Apecs.TH
  ( makeWorld
  , makeWorldNoEC
  , makeWorldAndComponents
  , makeMapComponents
  , makeMapComponentsFor
  , hasStoreInstance
  , makeInstanceFold
  , mkFoldT
  , mkTupleT
  , mkEitherT
  ) where

import           Control.Monad        (filterM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (asks)
import           Data.Traversable     (for)
import qualified Data.Kind as TL
import           Language.Haskell.TH

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util           (EntityCounter)

type family WorldInitConstraints cs (m :: TL.Type -> TL.Type) :: TL.Constraint where
  WorldInitConstraints () m = ()
  WorldInitConstraints (c, cs) m = (ExplInit m (Storage c), WorldInitConstraints cs m)

-- | Same as 'makeWorld', but does not include an 'EntityCounter'
--   You don't typically want to use this, but it's exposed in case you know what you're doing.
makeWorldNoEC :: String -> [Name] -> Q [Dec]
makeWorldNoEC worldName cTypes = do
  let world         = mkName worldName
      initWorldName = mkName $ "init" ++ worldName
  -- Data type decl
  data_decl <- do
    let fields = [ bangType (bang noSourceUnpackedness sourceStrict)
                     [t| Storage $(conT ty) |]
                 | ty <- cTypes
                 ]
    dataD (pure []) world [] Nothing [normalC world fields] []
  -- World initialization
  init_world <- do
    m <- newName "m"
    let mkNestedTupleT [] = ConT ''()
        mkNestedTupleT (x:xs) = AppT (AppT (TupleT 2) (ConT x)) (mkNestedTupleT xs)
        compTupleTy = mkNestedTupleT cTypes
    let constraints =
          [ AppT (ConT ''MonadIO) (VarT m)
          , AppT (AppT (ConT ''WorldInitConstraints) compTupleTy) (VarT m)
          ]
    sig <- sigD initWorldName $  forallT [] (pure constraints) [t| $(varT m) $(conT world) |]
    decl <- funD initWorldName
      [ clause []
        (normalB $ foldl (\e _ -> [| $e <*> explInit |])
                         [| pure $(conE world) |]
                         cTypes)
        []
      ]
    pure [sig, decl]
  -- Has instances
  instances <- for (enumerate cTypes) $ \(i,t) -> do
    x <- newName "x"
    let pat = conP world [ if j==i then varP x else wildP
                         | (j,_) <- enumerate cTypes
                         ]
    [d| instance Monad m => Has $(conT world) m $(conT t) where
          getStore = let field $pat = $(varE x) in asks field
      |]

  -- World-wide collections for particular types
  let skip = ["Global", "ReadOnly"]
  let m = ConT ''IO
  destructible <- filterM (hasStoreInstance skip ''ExplDestroy m) cTypes
  destructible_decl <- makeInstanceFold mkTupleT (worldName ++ "Destructible") destructible

  enumerable <- filterM (hasStoreInstance skip ''ExplMembers m) cTypes
  enumerable_decl <- makeInstanceFold mkEitherT (worldName ++ "Enumerable") enumerable

  pure $ data_decl : destructible_decl : enumerable_decl : concat (init_world : instances)
  where
    enumerate :: [a] -> [(Int,a)]
    enumerate = zip [0..]

mkTupleT :: [Type] -> Type
mkTupleT [] = ConT ''()
mkTupleT [t] = t
mkTupleT ts
  | len <= 8 = foldl AppT (TupleT len) ts
  | otherwise = foldl AppT (TupleT 8) (take 7 ts ++ [mkTupleT (drop 7 ts)])
  where
    len = length ts

mkEitherT :: [Type] -> Type
mkEitherT = mkFoldT ''Either ''()

mkFoldT :: Name -> Name -> [Type] -> Type
mkFoldT _con nil [] = ConT nil
mkFoldT _con _nil [t] = t
mkFoldT con nil (t : ts) = AppT (AppT (ConT con) t) (mkFoldT con nil ts)

makeInstanceFold :: ([Type] -> Type) -> String -> [Name] -> Q Dec
makeInstanceFold foldT synName cTypes =
  tySynD (mkName synName) [] . pure $
    foldT $ map ConT cTypes

-- | Resolve storage type and check for an instance like @ExplThis m (Map Position)@
--
-- Can be used to pre-filter component lists for 'makeInstanceFold'.
hasStoreInstance
  :: [String] -- ^ Skip those stores
  -> Name -- ^ Class name (ExplThis)
  -> Type -- ^ @m@ var like @ConT ''IO@
  -> Name -- ^ component type name
  -> Q Bool
hasStoreInstance skip cls mType cType = do
  storageT <- resolveStorageType cType
  case storageT of
    Just (AppT (ConT store) _stored)
      | nameBase store `elem` skip -> pure False
    Just resolved -> isInstance cls [mType, resolved]
    Nothing -> pure False

-- | Resolve the @Storage@ type family for a component type.
--
-- On GHC < 9.2, @isInstance@ does not reduce type family applications,
-- so we need to resolve @Storage ty@ before passing it to @isInstance@.
resolveStorageType :: Name -> Q (Maybe Type)
resolveStorageType ty = do
  insts <- reifyInstances ''Storage [ConT ty]
  pure $ case insts of
#if MIN_VERSION_template_haskell(2,15,0)
    [TySynInstD (TySynEqn _ _ rhs)] -> Just rhs
#else
    [TySynInstD _ (TySynEqn _ rhs)] -> Just rhs
#endif
    _ -> Nothing

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
