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
	, turnforward, turnback, focus, heres, theres, gohere, gothere) where

import Control.Comonad (Comonad (..), (=>>))
import Control.Comonad.Cofree (Cofree (..), coiter, unwrap)
import Control.Comonad.Trans.Class (ComonadTrans (..))
import Control.Natural ((:~>) (..))
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
		(coiter (undefined False) el)
		(x =>> (\c -> CursorT h c t))
		(coiter (gothere False) el)

instance ComonadTrans CursorT where
	lower (CursorT _ x _) = x

turnforward :: Here :~> There
turnforward = NT $ \case
	Here x -> There x
	Logjam -> Deadend

turnback :: There :~> Here
turnback = NT $ \case
	There x -> Here x
	Deadend -> Logjam

gohere :: Comonad w => Bool -> CursorT w a -> Here (CursorT w a)
gohere _ e@(CursorT (_ :< Logjam) _ _) = Logjam
gohere False (CursorT (d :< Here h) x t) = Here $ CursorT h (x $> d) (extract x :< There t)
gohere True (CursorT (d :< Here h) x t) = Here $ CursorT h (x $> d) t

gothere :: Comonad w => Bool -> CursorT w a -> There (CursorT w a)
gothere _ e@(CursorT _ _ (_ :< Deadend)) = Deadend
gothere False (CursorT h x (u :< There t)) = There $ CursorT (extract x :< Here h) (x $> u) t
gothere True (CursorT h x (u :< There t)) = There $ CursorT h (x $> u) t
