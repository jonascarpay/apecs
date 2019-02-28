{-|
Stability: experimtal

Containment module for stores that are experimental/too weird for @Apecs.Stores@.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Apecs.Stores.Extra
  ( Pushdown(..), Stack(..)
  , ReadOnly(..), setReadOnly, destroyReadOnly
  ) where

import Control.Monad.Reader
import Data.Proxy

import Apecs.Components (MaybeStore (..))
import Apecs.Core

-- | Overrides a store to have history/pushdown semantics.
--   Setting this store adds a new value on top of the stack.
--   Destroying pops the stack.
--   You can view the entire stack using the 'Stack' wrapper.
newtype Pushdown s c = Pushdown (s (Stack c))
newtype Stack c = Stack {getStack :: [c]} deriving (Eq, Show, Functor, Applicative, Monad, Foldable, Monoid, Semigroup)

type instance Elem (Pushdown s c) = c

instance (Functor m, ExplInit m (s (Stack c))) => ExplInit m (Pushdown s c) where
  explInit = Pushdown <$> explInit

pattern StackList :: c -> [c] -> Maybe (Stack c)
pattern StackList x xs = Just (Stack (x:xs))

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplGet m (Pushdown s c) where
    explExists (Pushdown s) ety = f <$> explGet (MaybeStore s) ety
      where
        f (StackList _ _) = True
        f _               = False
    explGet (Pushdown s) ety = head . getStack <$> explGet s ety

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , ExplSet m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplSet m (Pushdown s c) where
    explSet (Pushdown s) ety c = do
      ms <- explGet (MaybeStore s) ety
      let tail (StackList _ cs) = cs
          tail _                = []
      explSet s ety (Stack (c:tail ms))

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , ExplSet m (s (Stack c))
  , ExplDestroy m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplDestroy m (Pushdown s c) where
    explDestroy (Pushdown s) ety = do
      mscs <- explGet (MaybeStore s) ety
      case mscs of
        StackList _ cs' -> explSet s ety (Stack cs')
        _               -> explDestroy s ety

instance
  ( Monad m
  , ExplMembers m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplMembers m (Pushdown s c) where
    explMembers (Pushdown s) = explMembers s

instance (Storage c ~ Pushdown s c, Component c) => Component (Stack c) where
  type Storage (Stack c) = StackStore (Storage c)

newtype StackStore s = StackStore s
type instance Elem (StackStore s) = Stack (Elem s)

instance (Storage c ~ Pushdown s c, Has w m c) => Has w m (Stack c) where
  getStore = StackStore <$> getStore

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplGet m (s (Stack c))
  ) => ExplGet m (StackStore (Pushdown s c)) where
  explExists (StackStore s) = explExists s
  explGet (StackStore (Pushdown s)) = explGet s

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplSet     m (s (Stack c))
  , ExplDestroy m (s (Stack c))
  ) => ExplSet m (StackStore (Pushdown s c)) where
  explSet (StackStore (Pushdown s)) ety (Stack []) = explDestroy s ety
  explSet (StackStore (Pushdown s)) ety st         = explSet s ety st

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplDestroy m (s (Stack c))
  ) => ExplDestroy m (StackStore (Pushdown s c)) where
  explDestroy (StackStore (Pushdown s)) = explDestroy s

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplMembers m (s (Stack c))
  ) => ExplMembers m (StackStore (Pushdown s c)) where
  explMembers (StackStore (Pushdown s)) = explMembers s

-- | Wrapper that makes a store read-only. Use @setReadOnly@ and @destroyReadOnly@ to override.
newtype ReadOnly s = ReadOnly s
type instance Elem (ReadOnly s) = Elem s

instance (Functor m, ExplInit m s) => ExplInit m (ReadOnly s) where
  explInit = ReadOnly <$> explInit

instance ExplGet m s => ExplGet m (ReadOnly s) where
  explExists (ReadOnly s) = explExists s
  explGet    (ReadOnly s) = explGet s
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplMembers m s => ExplMembers m (ReadOnly s) where
  {-# INLINE explMembers #-}
  explMembers (ReadOnly s) = explMembers s

setReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplSet m s
  ) => Entity -> c -> SystemT w m ()
setReadOnly (Entity ety) c = do
  ReadOnly s <- getStore
  lift $ explSet s ety c

destroyReadOnly :: forall w m s c.
  ( Has w m c
  , Storage c ~ ReadOnly s
  , Elem s ~ c
  , ExplDestroy m s
  ) => Entity -> Proxy c -> SystemT w m ()
destroyReadOnly (Entity ety) _ = do
  ReadOnly s :: Storage c <- getStore
  lift $ explDestroy s ety
