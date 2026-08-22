{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Apecs.Stores
  ( Map
  , Cache
  , UCache
  , SCache
  , GCache
  , Unique
  , Global
  , Cachable
  , ReadOnly
  , setReadOnly
  , destroyReadOnly
  ) where

import Apecs.Stores.Internal
