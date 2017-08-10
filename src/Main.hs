import Control.ECS

data Vec2        = Vec Float Float deriving (Eq, Show)
newtype Position = Pos Vec2 deriving (Eq, Show)
newtype Velocity = Vel Vec2 deriving (Eq, Show)
data Enemy       = Enemy deriving (Eq, Show)
{-data MyWld       = MyWld { positions  :: !(M.IntMap Position)-}
                         {-, velocities :: !(M.IntMap Velocity)-}
                         {-, enemies    :: !S.IntSet-}
                         {-, eCounter   :: Int-}
                         {-} deriving (Eq, Show)-}

main = undefined
