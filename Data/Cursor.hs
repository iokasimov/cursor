module Data.Cursor (CursorT (..)) where

import Control.Comonad (Comonad (..), (=>>))
import Control.Comonad.Cofree (Cofree (..), coiter)
import Data.Functor (($>))

import Data.Cursor.Here (Here (..))
import Data.Cursor.There (There (..))

data CursorT w a = CursorT (Cofree Here a) (w a) (Cofree There a)

instance Functor w => Functor (CursorT w) where
	fmap f (CursorT h x t) = CursorT (f <$> h) (f <$> x) (f <$> t)

instance Comonad w => Comonad (CursorT w) where
	extract (CursorT _ x _) = extract x
	duplicate el@(CursorT h x t) = CursorT
		(coiter here el) (x =>> (\c -> CursorT h c t)) (coiter there el)

there :: Comonad w => CursorT w a -> There (CursorT w a)
there e@(CursorT _ _ (_ :< Deadend)) = Deadend
there (CursorT h x (u :< There t)) = There $ CursorT
	(extract x :< Here h) (x $> u) t

here :: Comonad w => CursorT w a -> Here (CursorT w a)
here e@(CursorT (_ :< Logjam) _ _) = Logjam
here (CursorT (d :< Here h) x t) = Here $ CursorT
	h (x $> d) (extract x :< There t)
