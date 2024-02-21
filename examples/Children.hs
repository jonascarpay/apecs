-- | Demonstrates simple usage of child components.
--
-- In this example, entities with a @Pos@ component are parent entities, and
-- these parent entities may have multiple child @Hitbox@ components. @Pos@ is
-- defined in world space, while @Hitbox@ is defined in local space (i.e. local
-- to the parent entity's position). The code shows a few ways of accessing the
-- @Hitbox@ components and transforming them from local space to world space by
-- leveraging the parent-child relationship of the entities.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

import Apecs
import Apecs.Experimental.Children (Child(..), ChildList(..), ChildValue(..))
import Linear (V2 (..))
import Linear.Affine (Point(..), Affine ((.+^)))
import Text.Printf (printf)
import Data.Foldable (for_)

newtype Pos = Pos (Point V2 Int) deriving Show
instance Component Pos where type Storage Pos = Map Pos

newtype Hitbox = Hitbox AABB deriving Show
instance Component Hitbox where type Storage Hitbox = Map Hitbox

-- | A type alias solely for TH quoting's sake in the call to @makeWorld@.
type ChildHitbox = Child Hitbox

-- | Stores bounding box min point in local coordinates and extents.
data AABB = AABB !(Point V2 Int) !(V2 Int) deriving Show

makeWorld "World" [''Pos, ''ChildHitbox]

game :: System World ()
game = do
  player1 <- newEntity (Pos $ P $ V2 3 0)
  player1Hitbox1 <- newEntity $ Child player1 $ Hitbox $ AABB (P $ V2 (-1) 1) $ V2 1 1
  newEntity_ $ Child player1 $ Hitbox $ AABB (P $ V2 0 0) $ V2 1 1
  newEntity_ $ Child player1 $ Hitbox $ AABB (P $ V2 1 1) $ V2 1 1

  player2 <- newEntity (Pos $ P $ V2 7 0)
  newEntity_ $ Child player2 $ Hitbox $ AABB (P $ V2 (-1) 2) $ V2 1 2

  -- Iterate over all child hitboxes, fetch each hitbox's parent world space
  -- position, and transform each hitbox to world space:
  liftIO $ putStrLn "Hitboxes in world space:"
  cmapM_ $ \(Child parent (Hitbox bboxLocal)) -> do
    Pos posWorld <- get parent
    let bboxWorld = fromLocalSpace posWorld bboxLocal
    liftIO $ putStrLn $ display posWorld bboxLocal bboxWorld

  -- Move player 2 towards player 1 a little bit:
  modify player2 $ \(Pos posWorld) -> Pos $ posWorld .+^ V2 (-1) 0

  -- Child components may be reparented. This reparents one of player 1's
  -- hitboxes under player 2.
  modify player1Hitbox1 $ \(ChildValue hitbox) -> Child @Hitbox player2 hitbox

  -- Cascading deletes from parent to child are also supported. This removes all
  -- of player 1's hitboxes:
  destroy player1 $ Proxy @(ChildList Hitbox)

  -- Transform hitboxes from local space to world space again, but this time
  -- iterate over the parent positions instead of the child hitboxes:
  liftIO $ putStrLn "Hitboxes in world space (again):"
  cmapM_ $ \(Pos posWorld, ChildList children :: ChildList Hitbox) -> do
    for_ children $ \child -> do
      ChildValue (Hitbox bboxLocal) <- get child
      let bboxWorld = fromLocalSpace posWorld bboxLocal
      liftIO $ putStrLn $ display posWorld bboxLocal bboxWorld

fromLocalSpace :: Point V2 Int -> AABB -> AABB
fromLocalSpace (P worldOffset) (AABB min extent) =
  AABB (min .+^ worldOffset) extent

display :: Point V2 Int -> AABB -> AABB -> String
display posWorld bboxLocal bboxWorld =
  printf
    "  posWorld: %s, bboxLocal: %s, bboxWorld: %s"
    (show posWorld)
    (show bboxLocal)
    (show bboxWorld)

main :: IO ()
main = initWorld >>= runSystem game
