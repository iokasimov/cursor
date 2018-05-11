module Data.Cursor.Here (Here (..)) where

data Here a = Logjam | Here a deriving Show

instance Functor Here where
	fmap _ Logjam = Logjam
	fmap f (Here x) = Here $ f x
