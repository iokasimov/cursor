module Data.Cursor.Here (Here (..)) where

data Here a = Logjam | Here a deriving Show

instance Functor Here where
	fmap _ Logjam = Logjam
	fmap f (Here x) = Here $ f x

instance Applicative Here where
	pure = Here
	Here f <*> x = f <$> x
	Logjam <*> _ = Logjam

instance Monad Here where
	Here x >>= k = k x
	Logjam >>= _ = Logjam
