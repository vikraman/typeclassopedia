module Applicative where

import Functor
import Prelude (Maybe (..), repeat, zipWith, ($))

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap g (ZipList xs) = ZipList (fmap g xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

instance Applicative [] where
  pure x = [x]
  gs <*> xs = [ g x | g <- gs, x <- xs ]

instance Applicative Maybe where
  pure = Just
  Just g <*> Just x = Just (g x)
  _ <*> _ = Nothing

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

pure' :: Monoidal f => a -> f a
pure' a = fmap (\_ -> a) unit

(<*>.) :: Monoidal f => f (a -> b) -> f a -> f b
(<*>.) g f = fmap (\(h, x) -> h x) ((**) g f)
