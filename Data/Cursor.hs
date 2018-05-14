module Data.Cursor
	(CursorT (..), Here (..), There (..)
	, focus, heres, theres, gohere, gothere) where

import Control.Comonad (Comonad (..), (=>>))
import Control.Comonad.Cofree (Cofree (..), coiter)
import Control.Lens (Lens')
import Data.Functor (($>))

import Data.Cursor.Here (Here (..))
import Data.Cursor.There (There (..))

data CursorT w a = CursorT (Cofree Here a) (w a) (Cofree There a)

focus :: Lens' (CursorT w a) (w a)
focus f (CursorT h x t) = (\new -> CursorT h new t) <$> f x

heres :: Lens' (CursorT w a) (Cofree Here a)
heres f (CursorT h x t) = (\new -> CursorT new x t) <$> f h

theres :: Lens' (CursorT w a) (Cofree There a)
theres f (CursorT h x t) = (\new -> CursorT h x new) <$> f t

instance Functor w => Functor (CursorT w) where
	fmap f (CursorT h x t) = CursorT (f <$> h) (f <$> x) (f <$> t)

instance Comonad w => Comonad (CursorT w) where
	extract (CursorT _ x _) = extract x
	duplicate el@(CursorT h x t) = CursorT
		(coiter (gohere False id) el)
		(x =>> (\c -> CursorT h c t))
		(coiter (gothere False id) el)

gothere :: Comonad w => Bool -> (w a -> w a) -> CursorT w a -> There (CursorT w a)
gothere _ _ e@(CursorT _ _ (_ :< Deadend)) = Deadend
gothere squash inner (CursorT h x (u :< There t)) =
	if squash then There $ CursorT h (inner $ x $> u) t -- squash focus
	else There $ CursorT (extract x :< Here h) (inner $ x $> u) t

gohere :: Comonad w => Bool -> (w a -> w a) -> CursorT w a -> Here (CursorT w a)
gohere _ _ e@(CursorT (_ :< Logjam) _ _) = Logjam
gohere squash inner (CursorT (d :< Here h) x t) =
	if squash then Here $ CursorT h (inner $ x $> d) t -- squash focus
	else Here $ CursorT h (inner $ x $> d) (extract x :< There t)
