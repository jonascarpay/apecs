{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Apecs.TH.Tags
  ( makeTaggedComponents
  , makeComponentTags
  , makeComponentSum
  , makeTagLookup
  , makeTagFromSum
  , makeGetTags
  , makeCountComponents
  , makeHasTagsInstance
  ) where

import           Control.Monad        (filterM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed  as U
import           Language.Haskell.TH

import Apecs.Core
import Apecs.TH (hasStoreInstance)

makeTaggedComponents :: String -> [Name] -> Q [Dec]
makeTaggedComponents worldName cTypes = do
  tags <- makeComponentTags tagType tagPrefix cTypes
  sums <- makeComponentSum sumType sumPrefix cTypes
  lookupFun <- makeTagLookup (Just ''Maybe) lookupFunName worldName tagType tagPrefix sumType sumPrefix cTypes
  getFun <- makeTagLookup Nothing getFunName worldName tagType tagPrefix sumType sumPrefix cTypes
  toTag <- makeTagFromSum tagFromSumFunName tagType tagPrefix sumType sumPrefix cTypes

  let skip = ["Global", "ReadOnly"]
  let m = ConT ''IO
  existing <- filterM (hasStoreInstance skip ''ExplGet m) cTypes

  getTags <- makeGetTags getTagsFunName worldName tagType tagPrefix existing
  hasTagsInst <- makeHasTagsInstance worldName tagType getTagsFunName existing

  enumerable <- filterM (hasStoreInstance skip ''ExplMembers m) cTypes
  countComps <- makeCountComponents countCompsFunName worldName tagType tagPrefix enumerable

  pure $ concat
    [ tags
    , sums
    , lookupFun
    , getFun
    , toTag
    , getTags
    , hasTagsInst
    , countComps
    ]
  where
    tagType = worldName ++ "Tag"
    tagPrefix = "T"
    sumType = worldName ++ "Sum"
    sumPrefix = "S"
    lookupFunName = "lookup" ++ worldName ++ "Tag"
    getFunName = "get" ++ worldName ++ "Tag"
    tagFromSumFunName = "tag" ++ sumType
    getTagsFunName = "get" ++ worldName ++ "Tags"
    countCompsFunName = "count" ++ worldName ++ "Components"

-- | Creates an Enum of component tags
makeComponentTags :: String -> String -> [Name] -> Q [Dec]
makeComponentTags typeName consPrefix cTypes = pure [decl]
  where
    decl = DataD [] (mkName typeName) [] Nothing cons derivs
    cons = map (\c -> NormalC (mkName $ consPrefix ++ nameBase c) []) cTypes
    derivs = [ DerivClause Nothing (map ConT [''Eq, ''Ord, ''Show, ''Enum, ''Bounded]) ]

-- | Creates a sum type of components
makeComponentSum :: String -> String -> [Name] -> Q [Dec]
makeComponentSum typeName consPrefix cTypes = pure [decl]
  where
    decl = DataD [] (mkName typeName) [] Nothing cons derivs
    cons = map (\c -> NormalC (mkName $ consPrefix ++ nameBase c) [(Bang NoSourceUnpackedness NoSourceStrictness, ConT c)]) cTypes
    derivs = [ DerivClause Nothing [ConT ''Show] ]

makeTagLookup :: Maybe Name -> String -> String -> String -> String -> String -> String -> [Name] -> Q [Dec]
makeTagLookup lookupWrapper funName worldName tagType tagPrefix sumType sumPrefix cTypes = do
  m <- newName "m"
  sig <- forallCompClsSig fName ''Get worldN m cTypes
    [t|
      Entity ->
      $(conT (mkName tagType)) ->
      SystemT $(conT worldN) $(varT m) $(wrapResult $ conT (mkName sumType))
    |]
  e <- newName "e"
  matches <- mapM (makeMatch e) cTypes
  t <- newName "t"
  let body = caseE (varE t) (map pure matches)
  decl <- funD fName [clause [varP e, varP t] (normalB body) []]
  pure [sig, decl]
  where
    (wrapResult, wrapCons) =
      case lookupWrapper of
        Nothing -> (id, id)
        Just funType ->
          ( appT (conT funType)
          , \sumConstr -> [| fmap $sumConstr |]
          )

    makeMatch e cType = match (conP tagCon []) (normalB matchBody) []
      where
        matchBody = [| $(wrapCons $ conE sumCon) <$> get $(varE e) |]
        tagCon = mkName (tagPrefix ++ nameBase cType)
        sumCon = mkName (sumPrefix ++ nameBase cType)
    fName = mkName funName
    worldN = mkName worldName

makeTagFromSum :: String -> String -> String -> String -> String -> [Name] -> Q [Dec]
makeTagFromSum funName tagType tagPrefix sumType sumPrefix cTypes = do
  s <- newName "s"

  sig <- sigD fName [t| $(conT sumN) -> $(conT tagN) |]

  matches <- mapM makeMatch cTypes
  let body = caseE (varE s) (map pure matches)
  decl <- funD fName [clause [varP s] (normalB body) []]
  pure [sig, decl]
  where
    makeMatch cType = match (conP sumCon [wildP]) (normalB (conE tagCon)) []
      where
        tagCon = mkName (tagPrefix ++ nameBase cType)
        sumCon = mkName (sumPrefix ++ nameBase cType)
    fName = mkName funName
    tagN  = mkName tagType
    sumN  = mkName sumType

-- | For each component type, get store and use explExists on the given entity
makeGetTags :: String -> String -> String -> String -> [Name] -> Q [Dec]
makeGetTags funName worldName tagType tagPrefix cTypes = do
  m <- newName "m"
  sig <- forallCompClsSig fName ''Get worldN m cTypes
    [t|
      Entity ->
      SystemT $(conT worldN) $(varT m) [$(conT $ mkName tagType)]
    |]
  e <- newName "e"
  stmts <- mapM (makeStmt m e) cTypes
  decl <- funD fName [clause [varP e] (bodyS stmts) []]
  pure [sig, decl]
  where
    fName = mkName funName
    worldN = mkName worldName
    makeStmt m e cType = bindS (varP tagName) body
      where
        tagName = mkName ("tag_" ++ nameBase cType)
        tagCon = mkName (tagPrefix ++ nameBase cType)
        body = [|
          do
            s <- getStore :: SystemT $(conT worldN) $(varT m) (Storage $(conT cType))
            has <- lift $ explExists s (unEntity $(varE e))
            pure [$(conE tagCon) | has]
          |]
    bodyS stmts = normalB . doE $ map pure stmts ++ [resultE]
      where
        tagNames = map (varE . mkName . ("tag_" ++) . nameBase) cTypes
        resultE = noBindS . appE (varE 'pure) $ appE (varE 'concat) $ listE tagNames

-- | Generates a standalone @type instance WTag World = WorldTag@ and a
--   @HasTags World m@ instance delegating @entityTags@ to the generated
--   @getWorldTags@ function.
makeHasTagsInstance :: String -> String -> String -> [Name] -> Q [Dec]
makeHasTagsInstance worldName tagType getTagsFunName cTypes = do
  m <- newName "m"
  instDec <- instanceD
    ((:) <$> [t| Monad $(varT m) |] <*> worldConstraints ''Get worldN m cTypes)
    [t| HasTags $(conT worldN) $(varT m) |]
    [ valD
          (varP 'entityTags)
          (normalB . varE $ mkName getTagsFunName)
          []
      ]

  let tySynDec =
#if MIN_VERSION_template_haskell(2,15,0)
        TySynInstD $ TySynEqn Nothing (ConT ''WTag `AppT` ConT worldN) (ConT tagN)
#else
        TySynInstD ''WTag $ TySynEqn [ConT worldN] (ConT tagN)
#endif
  pure [tySynDec, instDec]
  where
    worldN = mkName worldName
    tagN = mkName tagType

-- | For each component type with ExplMembers, count the number of entities that have that component.
makeCountComponents :: String -> String -> String -> String -> [Name] -> Q [Dec]
makeCountComponents funName worldName tagType tagPrefix cTypes = do
  m <- newName "m"
  sig <- forallCompClsSig fName ''Members worldN m cTypes
    [t| SystemT $(conT worldN) $(varT m) [($(conT tagN), Int)] |]
  stmts <- mapM (makeStmt m) cTypes
  decl <- funD fName [clause [] (bodyS stmts) []]
  pure [sig, decl]
  where
    fName = mkName funName
    tagN = mkName tagType
    worldN = mkName worldName
    makeStmt m cType = bindS (varP countName) body
      where
        countName = mkName ("count_" ++ nameBase cType)
        tagCon = mkName (tagPrefix ++ nameBase cType)
        body = [|
          do
            s <- getStore :: SystemT $(conT (mkName worldName)) $(varT m) (Storage $(conT cType))
            members <- lift $ explMembers s
            pure ($(conE tagCon), U.length members)
          |]
    bodyS stmts = normalB . doE $ map pure stmts ++ [resultE]
      where
        countNames = map (varE . mkName . ("count_" ++) . nameBase) cTypes
        resultE = noBindS . appE (varE 'pure) $ listE countNames

-- | Build a @f :: forall m. (Cls World m C1, ...) => body@ type signature
forallCompClsSig :: Name -> Name -> Name -> Name -> [Name] -> Q Type -> Q Dec
forallCompClsSig fName cls worldN m cTypes mkBody =
  sigD fName $ forallT [mkPlainTV m] (worldConstraints cls worldN m cTypes) mkBody

worldConstraints :: Name -> Name -> Name -> [Name] -> Q Cxt
worldConstraints cls worldN m = traverse $ \c ->
  [t| $(conT cls) $(conT worldN) $(varT m) $(conT c) |]

#if MIN_VERSION_template_haskell(2,17,0)
mkPlainTV :: Name -> TyVarBndr Specificity
mkPlainTV n = PlainTV n SpecifiedSpec
#else
mkPlainTV :: Name -> TyVarBndr
mkPlainTV = PlainTV
#endif
