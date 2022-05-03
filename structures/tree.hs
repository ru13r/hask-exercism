{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Foldable as F
import qualified Data.Tree as T
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Node x lt rt) = Node (f x) (fmap f lt) (fmap f rt)


instance Foldable Tree where
  foldMap _ Nil = mempty
  foldMap f (Node x l r) =
    F.foldMap f l `mappend`
    f x           `mappend`
    F.foldMap f r


instance Show a => Show (Tree a)  where
  show = T.drawTree . toDataTree
    where
      toDataTree Nil = T.Node "" []
      toDataTree (Node a l Nil) = T.Node (Prelude.show a) [toDataTree l]
      toDataTree (Node a Nil r) = T.Node (Prelude.show a) [toDataTree r]
      toDataTree (Node b l r) = T.Node (Prelude.show b) [toDataTree r, toDataTree l]


treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Nil = Node x Nil Nil
treeInsert x (Node a left right)
  | x <=  a = Node a (treeInsert x left) right
  | x >   a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Nil = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right


nums :: [Integer]
nums = [8, 6, 4, 1, 7, 3, 5, 11, 14, 4, 7, 9, 2]
numsTree :: Tree Integer
numsTree = foldr treeInsert Nil nums


words' :: [[Char]]
words' = ["cat", "car", "cool", "abra", "cadabra", "show", "hask"]
wordsTree :: Tree [Char]
wordsTree = foldr treeInsert Nil words'
