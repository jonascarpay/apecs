:set -XTypeFamilies -XScopedTypeVariables -XTypeOperators -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances

import Apecs
import Apecs.Stores
import Apecs.Util

newtype Health = Health Int deriving (Eq, Show)
instance Component Health where type Storage Health = Map Health

newtype Position = Position Float deriving (Eq, Show)
instance Component Position where type Storage Position = Map Position

newtype Player = Player Int deriving (Eq, Show)
instance Component Player where type Storage Player = Unique Player

data World = World { hps :: Storage Health, positions :: Storage Position, players :: Storage Player, ec :: Storage EntityCounter}
instance Has World Health where getStore = System $ asks hps
instance Has World Position where getStore = System $ asks positions
instance Has World Player where getStore = System $ asks players
instance Has World EntityCounter where getStore = System $ asks ec
type System' a = System World a

instance Show (Safe Position) where show (Safe mp) = "Safe (" ++ show mp ++ ")"
instance Show (Safe Health) where show (Safe mh) = "Safe (" ++ show mh ++ ")"
instance Show (Safe Player) where show (Safe mpl) = "Safe (" ++ show mpl ++ ")"

w <- World <$> initStore <*> initStore <*> initStore <*> initCounter
run = runWith w

printPositions = run$ (cimapM_ (\(e,p :: Position) -> liftIO $ print (e,p)) :: System' ())
printHealths   = run$ (cimapM_ (\(e,p :: Health) -> liftIO $ print (e,p)) :: System' ())
printPlayer    = run$ (cimapM_ (\(e,p :: Player) -> liftIO $ print (e,p)) :: System' ())
printEntity e  = run$ (do Safe r <- get e; liftIO (print r) :: System' ())

