module Bfs where

data Tree a = Node a (Tree a) (Tree a) | Leaf
  deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

dfs :: Tree a -> [a]
dfs Leaf = []
dfs (Node a l r) = a : (dfs l) ++ (dfs r)

bfs :: Tree a -> [a]
bfs t = reverse $ fst $ bfs_ ([], [t])

bfs_ :: ([a], [Tree a]) -> ([a], [Tree a])
bfs_ (xs, []) = (xs, [])
bfs_ (xs, (Leaf:ts)) = bfs_ (xs, ts)
bfs_ (xs, ((Node x l r):ts)) = bfs_ (x:xs, ts ++ [l, r])

labelDfs :: Tree Int -> Tree Int
labelDfs t = t2
  where (_, t2) = labelDfs_ ([0..], t)

labelDfs_ :: ([a], Tree b) -> ([a], Tree a)
labelDfs_ (xs, Leaf) = (xs, Leaf)
labelDfs_ (x:xs1, Node _ l r) = (xs3, Node x ll rr)
  where (xs2, ll) = labelDfs_ (xs1, l)
        (xs3, rr) = labelDfs_ (xs2, r)

labelBfs :: Tree b -> Tree Int
labelBfs t = t2
  where (xs, t2) = labelBfs_ (0 : xs, t)

labelBfs_ :: ([Int], Tree b) -> ([Int], Tree Int)
labelBfs_ (xs, Leaf) = (xs, Leaf)
labelBfs_ (x:xs1, Node _ l r) = (x+1 : xs3, Node x ll rr)
  where (xs2, ll) = labelBfs_ (xs1, l)
        (xs3, rr) = labelBfs_ (xs2, r)

tree01 :: Tree Int
tree01 =
  Node 0
    (Node 1 Leaf Leaf)
    (Node 2 Leaf Leaf)

tree02 :: Tree Int
tree02 =
  Node 0 Leaf $
    Node 1 Leaf $
      Node 2 Leaf $
        Node 3 Leaf Leaf

tree03 :: Tree Int
tree03 =
  Node 0
    (Node 1
      (Node 3 Leaf Leaf)
      (Node 4 Leaf Leaf))
    (Node 2
      (Node 5 Leaf Leaf)
      (Node 6 Leaf Leaf))

tree05 :: Tree Int
tree05 =
  Node 0
    (Node 0
      (Node 0 Leaf Leaf)
      (Node 0 Leaf Leaf))
    (Node 0
      (Node 0 Leaf Leaf)
      (Node 0 Leaf Leaf))

tree06 :: Tree Int
tree06 =
  Node 0
    (Node 1
      (Node 2 Leaf Leaf)
      (Node 3 Leaf Leaf))
    (Node 4
      (Node 5 Leaf Leaf)
      (Node 6 Leaf Leaf))

test01 = labelBfs tree05 == tree03

test02 = labelDfs tree05 == tree06
