{-# LANGUAGE TemplateHaskell #-}

module Apecs.THTuples where

import qualified Data.Vector.Unboxed as U
import           Language.Haskell.TH

-- | Generate tuple instances for the following tuple sizes.
makeInstances :: [Int] -> Q [Dec]
makeInstances is = concat <$> traverse tupleInstances is

{--
instance (Component a, Component b) => Component (a,b) where
  type Storage (a,b) = (Storage a, Storage b)

instance (Has w a, Has w b) => Has w (a,b) where
  {-# INLINE getStore #-}
  getStore = (,) <$> getStore <*> getStore

instance (Store a, Store b) => Store (a,b) where
  type Elem (a, b) = (Elem a, Elem b)
  type SafeRW (a, b) = (SafeRW a, SafeRW b)
  initStore = (,) <$> initStore <*> initStore

  explSet       (sa,sb) ety (wa,wb) = explSet sa ety wa >> explSet sb ety wb
  explReset     (sa,sb) = explReset sa >> explReset sb
  explDestroy   (sa,sb) ety = explDestroy sa ety >> explDestroy sb ety
  explExists    (sa,sb) ety = explExists sa ety >>= \case False -> return False
                                                          True  -> explExists sb ety
  explMembers   (sa,sb) = explMembers sa >>= U.filterM (explExists sb)
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explMembers #-}
  {-# INLINE explReset #-}
  {-# INLINE explDestroy #-}
  {-# INLINE explExists #-}
--}
tupleInstances :: Int -> Q [Dec]
tupleInstances n = do
  let vars = [ VarT . mkName $ "t_" ++ show i | i <- [0..n-1]]
      tupleUpT :: [Type] -> Type
      tupleUpT = foldl AppT (TupleT n)
      varTuple :: Type
      varTuple = tupleUpT vars
      tupleName :: Name
      tupleName = tupleDataName n
      tuplE :: Exp
      tuplE = ConE tupleName

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
          [Clause [] (NormalB$ liftAll tuplE (replicate n $ getStoreE )) [] ]
        , PragmaD$ InlineP getStoreN Inline FunLike AllPhases
        ]

      liftAll f mas = foldl (\a x -> AppE (AppE apE a) x) (AppE (VarE (mkName "pure")) f) mas
      sequenceAll :: [Exp] -> Exp
      sequenceAll = foldl1 (\a x -> AppE (AppE (VarE$ mkName ">>") a) x)

      strN  = mkName "Store"
      strsN = mkName "Elem"

      strT  var = ConT strN  `AppT` var
      strsT var = ConT strsN `AppT` var

      sNs = [ mkName $ "s_" ++ show i | i <- [0..n-1]]
      sPat = ConP tupleName (VarP <$> sNs)
      sEs = VarE <$> sNs
      etyN = mkName "ety"
      etyE = VarE etyN
      etyPat = VarP etyN
      wNs = [ mkName $ "w_" ++ show i | i <- [0..n-1]]
      wPat = ConP tupleName (VarP <$> wNs)
      wEs = VarE <$> wNs

      explSetN     = mkName "explSet"
      explResetN   = mkName "explReset"
      explDestroyN = mkName "explDestroy"
      explExistsN  = mkName "explExists"
      explMembersN = mkName "explMembers"
      explGetN     = mkName "explGet"
      initStoreN   = mkName "initStore"

      explSetE     = VarE explSetN
      explResetE   = VarE explResetN
      explDestroyE = VarE explDestroyN
      explExistsE  = VarE explExistsN
      explMembersE = VarE explMembersN
      explGetE     = VarE explGetN

      explSetF sE wE = AppE explSetE sE `AppE` etyE `AppE` wE
      explResetF sE = AppE explResetE sE
      explDestroyF sE = AppE explDestroyE sE `AppE` etyE
      explExistsF sE = AppE explExistsE sE
      explMembersF sE = AppE explMembersE sE
      explGetF sE = AppE explGetE sE `AppE` etyE

      explExistsAnd va vb = AppE (AppE (VarE '(>>=)) va)
                                 (LamCaseE [ Match (ConP 'False []) (NormalB$ AppE (VarE 'return) (ConE 'False)) []
                                           , Match (ConP 'True []) (NormalB vb) []
                                           ])

      explMembersFold va vb = AppE (VarE '(>>=)) va `AppE` AppE (VarE 'U.filterM) vb

      strI = InstanceD Nothing (strT <$> vars) (strT varTuple)
        [ TySynInstD strsN $ TySynEqn [varTuple] (tupleUpT $ fmap strsT vars)

        , FunD explSetN [Clause [sPat, etyPat, wPat]
            (NormalB$ sequenceAll (zipWith explSetF sEs wEs)) [] ]
        , PragmaD$ InlineP explSetN Inline FunLike AllPhases

        , FunD explResetN [Clause [sPat]
            (NormalB$ sequenceAll (explResetF <$> sEs)) [] ]
        , PragmaD$ InlineP explResetN Inline FunLike AllPhases

        , FunD explDestroyN [Clause [sPat, etyPat]
            (NormalB$ sequenceAll (explDestroyF <$> sEs)) [] ]
        , PragmaD$ InlineP explDestroyN Inline FunLike AllPhases

        , FunD explExistsN [Clause [sPat, etyPat]
            (NormalB$ foldr explExistsAnd (AppE (VarE 'pure) (ConE 'True)) ((`AppE` etyE) . explExistsF <$> sEs)) [] ]
        , PragmaD$ InlineP explExistsN Inline FunLike AllPhases

        , FunD explMembersN [Clause [sPat]
            (NormalB$ foldl explMembersFold (explMembersF (head sEs)) (explExistsF <$> tail sEs)) [] ]
        , PragmaD$ InlineP explMembersN Inline FunLike AllPhases

        , FunD explGetN [Clause [sPat, etyPat]
            (NormalB$ liftAll tuplE (explGetF <$> sEs)) [] ]
        , PragmaD$ InlineP explGetN Inline FunLike AllPhases

        , FunD initStoreN [Clause []
            (NormalB$ liftAll tuplE (VarE initStoreN <$ sEs)) [] ]
        , PragmaD$ InlineP initStoreN Inline FunLike AllPhases

        ]

  return [compI, hasI, strI]
