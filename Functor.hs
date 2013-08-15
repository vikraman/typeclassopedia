module Functor where

import Prelude (Either (..), Int, Maybe (..))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap _ [] = []
  fmap g (x:xs) = g x : fmap g xs

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just a) = Just (g a)

instance Functor (Either e) where
  fmap _ (Left a) = Left a
  fmap g (Right b) = Right (g b)

instance Functor ((->) e) where
  fmap g h x = g (h x)

instance Functor ((,) e) where
  fmap g (a, b) = (a, g b)

data Pair a = Pair a a

instance Functor Pair where
  fmap g (Pair a b) = Pair (g a) (g b)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf h) = Leaf (\x -> g (h x))
  fmap g (Node ts) = Node (fmap (fmap g) ts)

data Foo a = Foo (a -> Int)

fmap' :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
fmap' g = fmap (fmap g)
