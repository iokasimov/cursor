module Data.Cursor.There (There (..)) where

data There a = Deadend | There a deriving Show

instance Functor There where
	fmap _ Deadend = Deadend
	fmap f (There x) = There $ f x

instance Applicative There where
	pure = There
	There f <*> x = f <$> x
	Deadend <*> _ = Deadend

instance Monad There where
	There x >>= k = k x
	Deadend >>= _ = Deadend
