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
  ) where

import           Control.Monad        (filterM)
import           Control.Monad.Trans.Reader (asks)
import           Data.Traversable     (for)
import           Language.Haskell.TH

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util           (EntityCounter)

makeTaggedComponents :: String -> [Name] -> Q [Dec]
makeTaggedComponents worldName cTypes = do
  tags <- makeComponentTags tagType tagPrefix cTypes
  sums <- makeComponentSum sumType sumPrefix cTypes
  getter <- makeTagLookup lookupFunName worldName tagType tagPrefix sumType sumPrefix cTypes
  toTag <- makeTagFromSum tagFromSumFunName tagType tagPrefix sumType sumPrefix cTypes
  pure $ tags ++ sums ++ getter ++ toTag
  where
    tagType = worldName ++ "Tag"
    tagPrefix = "T"
    sumType = worldName ++ "Sum"
    sumPrefix = "S"
    lookupFunName = "lookup" ++ worldName ++ "Tag"
    tagFromSumFunName = "tag" ++ sumType

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
  sig <- sigD fName [t| Entity -> $(conT tagN) -> System $(conT worldN) (Maybe $(conT sumN)) |]
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
