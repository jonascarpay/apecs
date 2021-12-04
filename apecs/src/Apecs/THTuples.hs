{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Apecs.THTuples where

import qualified Data.Vector.Unboxed as U
import           Language.Haskell.TH

{--
instance (Component a, Component b) => Component (a, b) where
  type Storage (a,b) = (Storage a, Storage b)

instance (Has w a, Has w b) => Has w (a,b) where
  getStore = liftM2 (,) getStore getStore

type instance Elem (a,b) = (Elem a, Elem b)

instance (ExplGet a, ExplGet b) => ExplGet (a, b) where
  explExists (sa, sb) ety = liftM2 (&&) (explExists sa ety) (explExists sb ety)
  explGet (sa, sb) ety = liftM2 (,) (explGet sa ety) (explGet sb ety)

instance (ExplSet a, ExplSet b) => ExplSet (a, b) where
  explSet (sa,sb) ety (a,b) = explSet sa ety a >> explSet sb ety b

instance (ExplDestroy a, ExplDestroy b) => ExplDestroy (a, b) where
  explDestroy (sa, sb) ety = explDestroy sa ety >> explDestroy sb ety

instance (ExplMembers a, ExplGet b) => ExplMembers (a, b) where
  explMembers (sa, sb) = explMembers sa >>= U.filterM (explExists sb)
--}

-- | Generate tuple instances for the following tuple sizes.
makeInstances :: [Int] -> Q [Dec]
makeInstances is = concat <$> traverse tupleInstances is

tupleInstances :: Int -> Q [Dec]
tupleInstances n = do
  let vars = [ VarT . mkName $ "t_" ++ show i | i <- [0..n-1]]
      m = VarT $ mkName "m"

      -- [''a,''b] -> ''(a,b)
      tupleUpT :: [Type] -> Type
      tupleUpT = foldl AppT (TupleT n)
      -- ''(t_0, t_1, .. )
      varTuple :: Type
      varTuple = tupleUpT vars

      tupleName :: Name
      tupleName = tupleDataName n
      tuplE :: Exp
      tuplE = ConE tupleName

      -- Component
      compN = mkName "Component"
      compT var = ConT compN `AppT` var
      strgN = mkName "Storage"
      strgT var = ConT strgN `AppT` var
      compI = InstanceD Nothing (fmap compT vars) (compT varTuple) [
#if MIN_VERSION_template_haskell(2,15,0)
          TySynInstD $ TySynEqn Nothing (strgT varTuple) (tupleUpT . fmap strgT $ vars)
#else
          TySynInstD strgN $ TySynEqn [varTuple] (tupleUpT . fmap strgT $ vars)
#endif
        ]

      -- Has
      hasN = mkName "Has"
      hasT var = ConT hasN `AppT` VarT (mkName "w") `AppT` m `AppT` var
      getStoreN = mkName "getStore"
      getStoreE = VarE getStoreN
      apN = mkName "<*>"
      apE = VarE apN
      hasI = InstanceD Nothing (hasT <$> vars) (hasT varTuple)
        [ FunD getStoreN
          [Clause [] (NormalB$ liftAll tuplE (replicate n getStoreE )) [] ]
        , PragmaD$ InlineP getStoreN Inline FunLike AllPhases
        ]

      liftAll f mas = foldl (\a x -> AppE (AppE apE a) x) (AppE (VarE (mkName "pure")) f) mas
      sequenceAll :: [Exp] -> Exp
      sequenceAll = foldl1 (\a x -> AppE (AppE (VarE$ mkName ">>") a) x)

      -- Elem
      elemN = mkName "Elem"
      elemT var = ConT elemN `AppT` var
#if MIN_VERSION_template_haskell(2,15,0)
      elemI = TySynInstD $ TySynEqn Nothing (elemT varTuple) (tupleUpT $ fmap elemT vars)
#else
      elemI = TySynInstD elemN $ TySynEqn [varTuple] (tupleUpT $ fmap elemT vars)
#endif

      -- s, ety, w arguments
      sNs = [ mkName $ "s_" ++ show i | i <- [0..n-1]]
      sEs = VarE <$> sNs
      etyN = mkName "ety"
      etyE = VarE etyN
      etyPat = VarP etyN
      wNs = [ mkName $ "w_" ++ show i | i <- [0..n-1]]
#if MIN_VERSION_template_haskell(2,18,0)
      sPat = ConP tupleName [] (VarP <$> sNs)
      wPat = ConP tupleName [] (VarP <$> wNs)
#else
      sPat = ConP tupleName (VarP <$> sNs)
      wPat = ConP tupleName (VarP <$> wNs)
#endif
      wEs = VarE <$> wNs

      getN     = mkName "ExplGet"
      setN     = mkName "ExplSet"
      membersN = mkName "ExplMembers"
      destroyN = mkName "ExplDestroy"

      getT     s = ConT getN     `AppT` m `AppT` s
      setT     s = ConT setN     `AppT` m `AppT` s
      membersT s = ConT membersN `AppT` m `AppT` s
      destroyT s = ConT destroyN `AppT` m `AppT` s

      explSetN     = mkName "explSet"
      explDestroyN = mkName "explDestroy"
      explExistsN  = mkName "explExists"
      explMembersN = mkName "explMembers"
      explGetN     = mkName "explGet"

      explSetE     = VarE explSetN
      explDestroyE = VarE explDestroyN
      explExistsE  = VarE explExistsN
      explMembersE = VarE explMembersN
      explGetE     = VarE explGetN

      explSetF sE wE  = AppE explSetE sE `AppE` etyE `AppE` wE
      explDestroyF sE = AppE explDestroyE sE `AppE` etyE
      explExistsF sE  = AppE explExistsE sE
      explMembersF sE = AppE explMembersE sE
      explGetF sE     = AppE explGetE sE `AppE` etyE

      explExistsAnd va vb =
        AppE (AppE (VarE '(>>=)) va)
#if MIN_VERSION_template_haskell(2,18,0)
          (LamCaseE [ Match (ConP 'False [] []) (NormalB$ AppE (VarE 'return) (ConE 'False)) []
                    , Match (ConP 'True [] []) (NormalB vb) []
#else
          (LamCaseE [ Match (ConP 'False []) (NormalB$ AppE (VarE 'return) (ConE 'False)) []
                    , Match (ConP 'True []) (NormalB vb) []
#endif
                    ])

      explMembersFold va vb = AppE (VarE '(>>=)) va `AppE` AppE (VarE 'U.filterM) vb

      getI = InstanceD Nothing (getT <$> vars) (getT varTuple)
        [ FunD explGetN [Clause [sPat, etyPat]
            (NormalB$ liftAll tuplE (explGetF <$> sEs)) [] ]
        , PragmaD$ InlineP explGetN Inline FunLike AllPhases

        , FunD explExistsN [Clause [sPat, etyPat]
            (NormalB$ foldr explExistsAnd (AppE (VarE 'pure) (ConE 'True)) ((`AppE` etyE) . explExistsF <$> sEs)) [] ]
        , PragmaD$ InlineP explExistsN Inline FunLike AllPhases
        ]

      setI = InstanceD Nothing (setT <$> vars) (setT varTuple)
        [ FunD explSetN [Clause [sPat, etyPat, wPat]
            (NormalB$ sequenceAll (zipWith explSetF sEs wEs)) [] ]
        , PragmaD$ InlineP explSetN Inline FunLike AllPhases
        ]

      destroyI = InstanceD Nothing (destroyT <$> vars) (destroyT varTuple)
        [ FunD explDestroyN [Clause [sPat, etyPat]
            (NormalB$ sequenceAll (explDestroyF <$> sEs)) [] ]
        , PragmaD$ InlineP explDestroyN Inline FunLike AllPhases
        ]

      membersI = InstanceD Nothing (membersT (head vars) : (getT <$> tail vars)) (membersT varTuple)
        [ FunD explMembersN [Clause [sPat]
            (NormalB$ foldl explMembersFold (explMembersF (head sEs)) (explExistsF <$> tail sEs)) [] ]
        , PragmaD$ InlineP explMembersN Inline FunLike AllPhases
        ]

  return [compI, hasI, elemI, getI, setI, destroyI, membersI]
