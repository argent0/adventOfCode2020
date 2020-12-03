module AOC.BSTree.Lazy where

import Prelude hiding (elem)
import qualified Data.List as DL

data BSTree a
	= Empty
	| Branch (BSTree a) a (BSTree a)
	deriving (Show, Eq)

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
fromList = DL.foldr insert Empty
