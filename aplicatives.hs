module TestAplicatives where

data Failure a = Fail | Ok a deriving (Show)

-- class Functor f where
--   fmap :: (a -> b) -> (f a -> f b)

instance Functor Failure where
  fmap f (Ok x) = Ok $ f x
  fmap f Fail = Fail

mapFailure :: (a -> b) -> [Failure a] -> [Failure b]
mapFailure = fmap . fmap

(<$$>) :: (a -> b) -> [Failure a] -> [Failure b]
(<$$>) = mapFailure

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Failure where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok $ f x
  Fail <*> _ = Fail
  _ <*> Fail = Fail

safeDivideMap :: Integral a => a -> [Failure a] -> [Failure a]
safeDivideMap 0 x = [Fail | _ <- x]
safeDivideMap a x = (`div` a) <$$> x

oldSafeDivideMap :: Integral a => a -> [Failure a] -> [Failure a]
oldSafeDivideMap 0 x = [Fail | _ <- x]
oldSafeDivideMap a (x : xs) = (Ok (`div` a) <*> x) : safeDivideMap a xs
oldSafeDivideMap a [] = []

-- class (Applicative m) => Monad m where
--   return :: a -> m a
--   return = pure
--   (>>=) :: m a -> (a -> m b) -> m b

instance Monad Failure where
  (Ok x) >>= f = f x
  Fail >>= f = Fail

safeDivide :: Failure Int -> Failure Int -> Failure Int
safeDivide xm ym = do
  x <- xm
  y <- ym
  if y == 0
    then Fail
    else return $ div x y

oldSafeDivide :: Failure Int -> Failure Int -> Failure Int
oldSafeDivide xm ym =
  xm
    >>= ( \x ->
            ym
              >>= ( \y ->
                      if y == 0
                        then Fail
                        else return (x `div` y)
                  )
        )
