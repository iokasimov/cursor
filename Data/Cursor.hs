module Data.Cursor (CursorT (..), Here (..), There (..), gohere, gothere) where

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
		(coiter (gohere False) el) (x =>> (\c -> CursorT h c t)) (coiter (gothere False) el)

gothere :: Comonad w => Bool -> CursorT w a -> There (CursorT w a)
gothere _ e@(CursorT _ _ (_ :< Deadend)) = Deadend
gothere squash (CursorT h x (u :< There t)) =
	if squash then There $ CursorT h (x $> u) t -- squash focus
	else There $ CursorT (extract x :< Here h) (x $> u) t

gohere :: Comonad w => Bool -> CursorT w a -> Here (CursorT w a)
gohere _ e@(CursorT (_ :< Logjam) _ _) = Logjam
gohere squash (CursorT (d :< Here h) x t) =
	if squash then Here $ CursorT h (x $> d) t -- squash focus
	else Here $ CursorT h (x $> d) (extract x :< There t)
