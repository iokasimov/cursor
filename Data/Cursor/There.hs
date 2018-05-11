module Data.Cursor.There (There (..)) where

data There a = Deadend | There a deriving Show

instance Functor There where
	fmap _ Deadend = Deadend
	fmap f (There x) = There $ f x
