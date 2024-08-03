module Ush.Util.Collector where

-- | A 'free monad' of sorts whose side effect is to collect values.
--
-- Designed to get a list of statements in a `do` block.
data Collector a b = Collector [a] b
  deriving (Functor)

instance Applicative (Collector a) where
  pure = Collector []

  (Collector xs a) <*> (Collector ys b) = Collector (ys <> xs) (a b)

instance Monad (Collector a) where
  (Collector xs a) >>= f =
    let Collector ys b = f a
     in Collector (ys <> xs) b

collectedValues :: Collector a b -> [a]
collectedValues (Collector xs _) = reverse xs

asCollector :: a -> Collector a ()
asCollector v = Collector [v] ()