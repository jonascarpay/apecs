module Apecs.Experimental.Components where

-- | Pseudocomponent that when written to, actually writes @c@ to its entity argument.
--   Can be used to write to other entities in a 'cmap'.
data Redirect c = Redirect Entity c deriving (Eq, Show)
instance Component c => Component (Redirect c) where
  type Storage (Redirect c) = RedirectStore (Storage c)

newtype RedirectStore s = RedirectStore s
type instance Elem (RedirectStore s) = Redirect (Elem s)

instance Has w m c => Has w m (Redirect c) where
  getStore = RedirectStore <$> getStore

instance (ExplSet m s) => ExplSet m (RedirectStore s) where
  explSet (RedirectStore s) _ (Redirect (Entity ety) c) = explSet s ety c
