{-|
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Apecs.Experimental.Stores
  ( Pushdown(..), Stack(..)
  ) where

import Control.Monad.Reader
import Data.Proxy
import Data.Semigroup

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
