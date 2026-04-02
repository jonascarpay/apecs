{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Apecs
import Apecs.Experimental.Reactive
import Data.Bits (xor)

data Position = Position { x :: Word, y :: Word } deriving (Show, Eq)

instance Ord Position where
  -- Sort on "most significant difference" for better locality.
  -- `Map Position a` then gives you a sparse quad-tree like index.
  compare (Position a b) (Position c d)
    | ac < bd && ac < xor ac bd = compare b d
    | otherwise = compare a c
    where
      ac = xor a c
      bd = xor b d

-- We use an OrdMap for the Position component
instance Component Position where type Storage Position = Reactive (OrdMap Position) (Map Position)

-- We also demonstrate EnumMap
data Status = Offline | Online | Away deriving (Show, Eq, Enum)
instance Component Status where type Storage Status = Reactive (EnumMap Status) (Map Status)

makeWorld "World" [''Position, ''Status]

main :: IO ()
main = initWorld >>= runSystem do
  -- Create some entities
  newEntity (Position 0 0, Online)
  newEntity (Position 1 1, Offline)
  newEntity (Position 2 2, Online)
  newEntity (Position 0 0, Offline)
  newEntity (Position 0 0, Away)
  newEntity (Position 2 2, Away)

  -- Look up entities by position using the reactive OrdMap
  originEntities <- withReactive $ ordLookup (Position 0 0)
  twotwoEntities <- withReactive $ ordLookup (Position 2 2)
  oneoneEntities <- withReactive $ ordLookup (Position 1 1)

  liftIO $ putStrLn $ "Entities at (0,0): " ++ show originEntities
  liftIO $ putStrLn $ "Entities at (2,2): " ++ show twotwoEntities
  liftIO $ putStrLn $ "Entities at (1,1): " ++ show oneoneEntities

  -- Look up entities by status using the reactive EnumMap
  onlineEntities <- withReactive $ enumLookup Online
  offlineEntities <- withReactive $ enumLookup Offline
  awayEntities <- withReactive $ enumLookup Away

  liftIO $ putStrLn ""
  liftIO $ putStrLn $ "Online entities: " ++ show onlineEntities
  liftIO $ putStrLn $ "Offline entities: " ++ show offlineEntities
  liftIO $ putStrLn $ "Away entities: " ++ show awayEntities

  -- Move entities at (0,0) to (3,3), and change Away to Online
  cmap $ \(Position px py) -> if px == 0 && py == 0 then Position 3 3 else Position px py
  cmap $ \s -> if s == Away then Online else s

  -- Look up entities by position and status again
  originEntities' <- withReactive $ ordLookup (Position 0 0)
  threethreeEntities <- withReactive $ ordLookup (Position 3 3)
  onlineEntities' <- withReactive $ enumLookup Online
  awayEntities' <- withReactive $ enumLookup Away

  liftIO $ putStrLn "\nAfter moving (0,0) to (3,3), and changing Away to Online:"
  liftIO $ putStrLn $ "Entities at (0,0): " ++ show originEntities'
  liftIO $ putStrLn $ "Entities at (3,3): " ++ show threethreeEntities
  liftIO $ putStrLn $ "Online entities: " ++ show onlineEntities'
  liftIO $ putStrLn $ "Away entities: " ++ show awayEntities'
