data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

t :: Tree Int
t = Node 5 (Node 2 (Node 10 Empty Empty)
                   (Node 4 (Node 3 Empty Empty)
                           Empty))
           (Node 3 (Node 1 Empty Empty)
                    Empty)


maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty                  = 0
maxSumPath (Node val Empty Empty) = val
maxSumPath (Node val Empty r)     = val + maxSumPath r
maxSumPath (Node val l Empty)     = val + maxSumPath l
maxSumPath (Node val l r)         = val + max (maxSumPath l) (maxSumPath r)


prune :: Tree a -> Tree a
prune Empty                     = Empty
prune (Node val Empty Empty)    = Empty
prune (Node val l r)            = Node val (prune l)
                                            (prune r)


bloom :: Tree a -> Tree a
bloom Empty                  = Empty
bloom (Node val Empty Empty) = Node val (Node val Empty Empty)
                                         (Node val Empty Empty)
bloom (Node val l r)         = Node val (bloom l)
                                         (bloom r)


isLeaf :: Tree a -> Bool
isLeaf Empty                = False
isLeaf (Node _ Empty Empty) = True
isLeaf (Node  _ _ _)        = False

{-
rightRotation :: Tree a -> Tree a
rightRotation (Node val (Node p a b) c) = Node p a (Node q b c)

leftRotation :: Tree a -> Tree a
leftRotation (Node q (Node p a b) c) = Node p a (q b c)
-}

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty           = Empty
treeMap  f (Node val l r) = Node (f val) (treeMap f l)
                                          (treeMap f r)


instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Node val l r) = Node (f val) (fmap f l)
                                         (fmap f r)


data BST a = BEmpty | BNode a (BST a) (BST a) deriving Show

bst :: BST Int
bst = BNode 10 (BNode 5 (BNode 4 BEmpty BEmpty)
                        (BNode 7 (BNode 6 BEmpty BEmpty)
                                 BEmpty))
               (BNode 20 (BNode 11 BEmpty BEmpty)
                        BEmpty)


bstInsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstInsert val BEmpty           = BNode val BEmpty BEmpty
bstInsert val (BNode root l r) = if root <= val then bstInsert val l 
                                 else bstInsert val r


bstValues :: BST a -> [a]

bstSize :: BST a -> Int 

bstSort :: Ord a => [a] -> [a]