data Trie a  = Leaf a | Node a [Trie a] deriving Show

is :: (Eq a) => Trie a -> a -> Bool
is (Leaf a) c   = a == c
is (Node a _) c = a == c

insert :: Eq a => [a] -> Trie a -> Trie a
insert [] t = t
insert [_] t = t
insert (_:y:ys) (Leaf a)   = Node a [insert (y:ys) (Leaf y)]
insert (_:y:ys) (Node a l)
  | any (`is` y) l = Node a
                     (insert (y:ys) (head $ filter (`is` y) l) : filter (not . (`is` y)) l)
  | otherwise      = Node a (insert (y:ys) (Leaf y) : l)


l :: Trie Char
l = foldl (flip insert) (Leaf 'c') ["cat", "car", "cool"]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x)   = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t





-- qElem :: (Ord a) => a -> Trie a -> Bool
-- trieElem x Nil = False
-- trieElem x (Node a left right)
--     | x == a = True
--     | x < a  = trieElem x left
--     | x > a  = trieElem x right
