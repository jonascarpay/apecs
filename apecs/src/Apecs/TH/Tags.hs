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
  getter <- makeTagLookup lookupFunName worldName tagType tagPrefix sumType sumPrefix cTypes
  toTag <- makeTagFromSum tagFromSumFunName tagType tagPrefix sumType sumPrefix cTypes

  let skip = ["Global", "ReadOnly"]
  let m = ConT ''IO
  existing <- filterM (hasStoreInstance skip ''ExplGet m) cTypes

  getTags <- makeGetTags getTagsFunName worldName tagType tagPrefix existing

  enumerable <- filterM (hasStoreInstance skip ''ExplMembers m) cTypes
  countComps <- makeCountComponents countCompsFunName worldName tagType tagPrefix enumerable

  pure $ tags ++ sums ++ getter ++ toTag ++ getTags ++ countComps
  where
    tagType = worldName ++ "Tag"
    tagPrefix = "T"
    sumType = worldName ++ "Sum"
    sumPrefix = "S"
    lookupFunName = "lookup" ++ worldName ++ "Tag"
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
    derivs = [ DerivClause Nothing (map ConT [''Show]) ]

makeTagLookup :: String -> String -> String -> String -> String -> String -> [Name] -> Q [Dec]
makeTagLookup funName worldName tagType tagPrefix sumType sumPrefix cTypes = do
  e <- newName "e"
  matches <- mapM (makeMatch e) cTypes
  t <- newName "t"
  let body = caseE (varE t) (map pure matches)
  sig <- sigD fName [t| Entity -> $(conT tagN) -> SystemT $(conT worldN) IO (Maybe $(conT sumN)) |]
  decl <- funD fName [clause [varP e, varP t] (normalB body) []]
  pure [sig, decl]
  where
    makeMatch e cType = match (conP tagCon []) (normalB [| fmap $(conE sumCon) <$> get $(varE e) |]) []
      where
        tagCon = mkName (tagPrefix ++ nameBase cType)
        sumCon = mkName (sumPrefix ++ nameBase cType)
    fName = mkName funName
    tagN  = mkName tagType
    sumN  = mkName sumType
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
  sig <- sigD fName [t| Entity -> SystemT $(conT worldN) IO [$(conT tagN)] |]
  e <- newName "e"
  stmts <- mapM (makeStmt e) cTypes
  decl <- funD fName [clause [varP e] (bodyS stmts) []]
  pure [sig, decl]
  where
    fName = mkName funName
    tagN = mkName tagType
    worldN = mkName worldName
    makeStmt e cType = bindS (varP tagName) body
      where
        tagName = mkName ("tag_" ++ nameBase cType)
        tagCon = mkName (tagPrefix ++ nameBase cType)
        body = [|
          do
            s <- getStore :: SystemT $(conT (mkName worldName)) IO (Storage $(conT cType))
            has <- lift $ explExists s (unEntity $(varE e))
            pure [$(conE tagCon) | has]
          |]
    bodyS stmts = normalB . doE $ map pure stmts ++ [resultE]
      where
        tagNames = map (varE . mkName . ("tag_" ++) . nameBase) cTypes
        resultE = noBindS . appE (varE 'pure) $ appE (varE 'concat) $ listE tagNames

-- | For each component type with ExplMembers, count the number of entities that have that component.
makeCountComponents :: String -> String -> String -> String -> [Name] -> Q [Dec]
makeCountComponents funName worldName tagType tagPrefix cTypes = do
  sig <- sigD fName [t| SystemT $(conT worldN) IO [($(conT tagN), Int)] |]
  stmts <- mapM makeStmt cTypes
  decl <- funD fName [clause [] (bodyS stmts) []]
  pure [sig, decl]
  where
    fName = mkName funName
    tagN = mkName tagType
    worldN = mkName worldName
    makeStmt cType = bindS (varP countName) body
      where
        countName = mkName ("count_" ++ nameBase cType)
        tagCon = mkName (tagPrefix ++ nameBase cType)
        body = [|
          do
            s <- getStore :: SystemT $(conT (mkName worldName)) IO (Storage $(conT cType))
            members <- lift $ explMembers s
            pure ($(conE tagCon), U.length members)
          |]
    bodyS stmts = normalB . doE $ map pure stmts ++ [resultE]
      where
        countNames = map (varE . mkName . ("count_" ++) . nameBase) cTypes
        resultE = noBindS . appE (varE 'pure) $ listE countNames
