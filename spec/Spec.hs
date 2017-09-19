{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Linear
import Apecs
import Apecs.Types
import qualified Apecs.Stores as S

randomVector = V2 <$> arbitrary <*> arbitrary

newtype Position = Position (V2 Float)
instance Arbitrary Position where
  arbitrary = Position <$> randomVector

newtype Velocity = Velocity (V2 Float)
instance Arbitrary Velocity where
  arbitrary = Velocity <$> randomVector

data Flag = Flag
instance Arbitrary Flag where arbitrary = return Flag
instance S.Flag Flag where flag = Flag

data Write c = Write (Entity c) c
instance Arbitrary c => Arbitrary (Write c) where
  arbitrary = do e <- arbitrary
                 c <- arbitrary
                 return $ Write (Entity e) c

data SafeWr c = SafeWr (Entity c) (SafeRW (Storage c))
instance Arbitrary (SafeRW (Storage c)) => Arbitrary (SafeWr c) where
  arbitrary = do e <- arbitrary
                 c <- arbitrary
                 return $ SafeWr (Entity e) c

instance Component Flag where
  type Storage Flag = S.Set Flag

instance Component Position where
  type Storage Position = S.Map Position

instance Component Velocity where
  type Storage Velocity = S.Map Velocity

someprop = monadicIO $ run f >>= assert
  where
    f = do

      return True


main = quickCheck someprop
