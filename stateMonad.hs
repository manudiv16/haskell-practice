{-# LANGUAGE TupleSections #-}

module State where

import Control.Applicative
import Prelude hiding (read)

newtype S s a = S {runS :: s -> (a, s)}

instance Functor (S s) where
  fmap f (S g) = S $ \s ->
    let (a, s') = g s
     in (f a, s')

instance Applicative (S s) where
  pure a = S (a,)
  (S f) <*> (S x) = S $ \s ->
    let (f', s') = f s
        (a, s'') = x s'
     in (f' a, s'')

instance Monad (S s) where
  -- return a = S $ \m -> (a, m)
  return a = S (a,)

  -- (>>=) :: S s a -> (a -> S s b) -> S s b
  S f >>= k =
    S $ \s ->
      let (a, s') = f s
       in runS (k a) s' -- s -> (b, s)

-- f :: s -> (a, s)
-- k :: a -> S s b
-- k a :: S s b
-- runS (k a) :: s -> (b, s)
-- runS (k a) s' :: (b, s)

read :: S s s
read = S $ \s -> (s, s)

write :: s -> S s ()
write s = S $ const ((), s)

foo :: S Int Int
foo = read >>= (\x -> write (x + 1) >>= const read)

fooDo :: S Int Int
fooDo = do
  x <- read
  write (x + 1)
  x <- read
  write (x + 2)
  x <- read
  return $ x + 2

test :: (Int, Int)
test = runS fooDo 0
