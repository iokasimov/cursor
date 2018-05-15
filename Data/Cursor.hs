--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Cursor
-- Copyright   :  (C) 2018 Murat Kasimov
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Murat Kasimov <iokasimov.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------------------

module Data.Cursor
	(CursorT (..), Here (..), There (..)
	, focus, heres, theres, gohere, gothere) where

import Control.Comonad (Comonad (..), (=>>))
import Control.Comonad.Cofree (Cofree (..), coiter, unwrap)
import Control.Lens (Lens')
import Data.Functor (($>))
import Data.These (These (..))

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
		(coiter (undefined Nothing id) el)
		(x =>> (\c -> CursorT h c t))
		(coiter (gothere Nothing id) el)

gohere :: Comonad w
	=> Maybe (These a a) -- ^ Squashing focus, here's focus, both or nothing
	-> (w a -> w a) -- ^ Do something with lower comonad
	-> CursorT w a -- ^ The cursor to move
	-> Here (CursorT w a)
gohere _ _ e@(CursorT (_ :< Logjam) _ _) = Logjam
gohere Nothing inner (CursorT (d :< Here h) x t) =
	Here $ CursorT h (inner $ x $> d) (extract x :< There t)
gohere (Just (This _x)) inner (CursorT (d :< Here h) x t) =
	Here $ CursorT h (inner $ x $> d) t
gohere (Just (That _d)) inner (CursorT (d :< Here h) x t) =
	(\h' -> CursorT h' (inner x) t) <$> (unwrap h)
gohere (Just (These _x _u)) inner (CursorT (d :< Here h) x t) =
	 (\h' -> CursorT h' (inner $ x $> extract h) t) <$> (unwrap h)

gothere :: Comonad w
	=> Maybe (These a a) -- ^ Squashing focus, there's focus, both or nothing
	-> (w a -> w a) -- ^ Do something with lower comonad
	-> CursorT w a  -- ^ The cursor to move
	-> There (CursorT w a)
gothere _ _ e@(CursorT _ _ (_ :< Deadend)) = Deadend
gothere Nothing inner (CursorT h x (u :< There t)) =
	There $ CursorT (extract x :< Here h) (inner $ x $> u) t
gothere (Just (This _x)) inner (CursorT h x (u :< There t)) =
	There $ CursorT h (inner $ x $> u) t
gothere (Just (That _u)) inner (CursorT h x (u :< There t)) =
	CursorT h (inner x) <$> (unwrap t)
gothere (Just (These _x _u)) inner (CursorT h x (u :< There t)) =
	CursorT h (inner $ x $> extract t) <$> (unwrap t)
