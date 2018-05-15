module Data.Cursor.Here (Here (..)) where

import Control.Applicative (Applicative (..), Alternative (..))

data Here a = Logjam | Here a deriving Show

instance Functor Here where
	fmap _ Logjam = Logjam
	fmap f (Here x) = Here $ f x

instance Applicative Here where
	pure = Here
	Here f <*> x = f <$> x
	Logjam <*> _ = Logjam

instance Alternative Here where
	empty = Logjam
	Logjam <|> r = r
	l <|> _ = l

instance Monad Here where
	Here x >>= k = k x
	Logjam >>= _ = Logjam
