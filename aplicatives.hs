-- class Functor f where
--   fmap :: (a -> b) -> (f a -> f b)
module TestAplicatives where

data Failure a = Fail | Ok a deriving (Show)

instance Functor Failure where
  fmap f (Ok x) = Ok $ f x
  fmap f Fail = Fail

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Failure where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok $ f x
  Fail <*> _ = Fail
  _ <*> Fail = Fail

safeDivideMap :: Integral a => a -> [Failure a] -> [Failure a]
safeDivideMap a (x : xs) =
  if a == 0
    then Fail : [Fail | _ <- [xs]]
    else (Ok (`div` a) <*> x) : safeDivideMap a xs
safeDivideMap a [] = []

-- class (Applicative m) => Monad m where
--   return :: a -> m a
--   return = pure
--   (>>=) :: m a -> (a -> m b) -> m b

instance Monad Failure where
  (Ok x) >>= f = f x
  Fail >>= f = Fail

safeDivide :: Failure Int -> Failure Int -> Failure Int
safeDivide xm ym =
  xm
    >>= ( \x ->
            ym
              >>= ( \y ->
                      if y == 0
                        then Fail
                        else return (x `div` y)
                  )
        )
