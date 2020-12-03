module AOC.BSTree.Strict where

import Prelude hiding (elem)
import qualified Data.List as DL
import Data.Foldable (toList)

data BSTree a
	= Empty
	| Branch !(BSTree a) !a !(BSTree a)
	deriving (Show, Eq)

instance Foldable BSTree where
	foldMap f Empty = mempty
	foldMap f (Branch l o r) = foldMap f l <> f o <> foldMap f r

insert :: Ord a => a -> BSTree a -> BSTree a
insert a Empty = Branch Empty a Empty
insert a (Branch l o r)
	| a <= o = Branch (insert a l) o r
	| otherwise = Branch l o (insert a r)

elem :: Ord a => a -> BSTree a -> Bool
elem _ Empty = False
elem a (Branch l o r)
	| a == o = True
	| a < o = elem a l
	| otherwise = elem a r

fromList :: Ord a => [a] -> BSTree a
fromList = DL.foldl' (flip insert) Empty

merge :: Ord a => BSTree a -> BSTree a -> BSTree a
merge Empty b = b
merge a Empty = a
merge a@Branch {} b@Branch {} = DL.foldl' (flip insert) a (toList b)

btTails :: Ord a => BSTree a -> [(a, BSTree a)]
btTails Empty = []
btTails (Branch l o r) = (o, merge l r) : btTails (merge l r)
