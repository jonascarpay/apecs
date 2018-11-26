{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Apecs.Gloss
  ( module Apecs.Gloss
  , module G
  ) where

import           Data.Semigroup                   (Semigroup (..))
import           Graphics.Gloss.Interface.IO.Game as G
import           Linear.V2
import           Linear.Vector

import           Apecs

data Camera = Camera
  { camOffset :: V2 Double
  , camScale  :: Double
  } deriving (Eq, Show, Read)

instance Semigroup Camera where
  Camera x1 r1 <> Camera x2 r2 = Camera (x1 + x2) (r1 * r2)
instance Monoid Camera where
  mempty = Camera 0 1
  mappend = (<>)

-- | Apply a camera transformation to a picture
cameraTransform :: Camera -> Picture -> Picture
cameraTransform (Camera (V2 cx cy) cs) =
    Scale (f cs) (f cs) .  Translate (f . negate $ cx) (f . negate $ cy)
  where f = realToFrac

windowToWorld :: Camera -> (Float,Float) -> V2 Double
windowToWorld (Camera cx cs) (x,y) = v ^/ cs - cx
  where v = V2 (realToFrac x) (realToFrac y)

instance Component Camera where
  type Storage Camera = Global Camera

play
  :: (Has w IO Camera)
  => Display            -- ^ Display mode
  -> Color              -- ^ Background color
  -> Int                  -- ^ Desired FPS
  -> System w Picture -- ^ Drawing function
  -> (Event -> System w ()) -- ^ Event handling function
  -> (Double  -> System w ()) -- ^ Stepping function, with a time delta argument.
  -> System w ()
play disp col fps draw handle step = do
  w <- ask
  liftIO$ playIO disp col fps w draw' handle' step'
    where
      handle' event = runSystem $ handle event >> ask
      step'   dT    = runSystem $ step (realToFrac dT) >> ask
      draw'         = runSystem $ do
        cam <- get global
        cameraTransform cam <$> draw

-- | Renders a picture given a component rendering function.
foldDraw :: (Get w IO c, Members w IO c)
         => (c -> Picture) -- ^ Component render function.
         -> System w Picture
foldDraw fn = cfold (\pic -> mappend pic . fn) mempty

-- | Monadically renders a picture given a component rendering function.
foldDrawM :: (Get w IO c, Members w IO c)
          => (c -> System w Picture) -- ^ Component render function.
          -> System w Picture
foldDrawM fn = cfoldM (\pic -> fmap (mappend pic) . fn) mempty
