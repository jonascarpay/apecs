{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Apecs.Core

import Data.Maybe (maybe)

newtype Position = Position Double deriving Show
newtype Velocity = Velocity Double deriving Show
data Flying = Flying deriving Show

makeWorldAndComponents "World"
  [ ''Position
  , ''Velocity
  , ''Flying
  ]

makeTable :: System World [(Entity,String,String,String)]
makeTable = forM [0..9] $ \e -> do
  (mp,mv,mf) :: (Maybe Position, Maybe Velocity, Maybe Flying) <- get (Entity e)
  return (Entity e, mshow mp, mshow mv, mshow mf)
  where
    mshow = maybe "-" show

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

main :: IO ()
main = initWorld >>= runSystem game
