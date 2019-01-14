data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

t :: Tree Int
t = Node 5 (Node 2 (Node 10 Empty Empty)
                   (Node 4 (Node 3 Empty Empty)
                            Empty))
           (Node 3 (Node 1 Empty Empty)
                    Empty)


isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False


maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty                  = 0
maxSumPath (Node val Empty Empty) = val
maxSumPath (Node val Empty r)     = val + maxSumPath r
maxSumPath (Node val l Empty)     = val + maxSumPath l
maxSumPath (Node val l r)         = val + max (maxSumPath l) (maxSumPath r)


prune :: Tree a -> Tree a
prune Empty                   = Empty
prune (Node _ Empty Empty)    = Empty
prune (Node val l r)          = Node val (prune l) (prune r)


bloom :: Tree a -> Tree a
bloom Empty                  = Empty
bloom (Node val Empty Empty) = Node val (Node val Empty Empty)
                                        (Node val Empty Empty)
bloom (Node val l r)         = Node val (bloom l) (bloom r)


isLeaf :: Tree a -> Bool
isLeaf Empty                = False
isLeaf (Node _ Empty Empty) = True
isLeaf (Node  _ _ _)        = False

{-
rightRotation :: Tree a -> Tree a
rightRotation (Node q (Node p a b) c) = Node p a (Node q b c)

leftRotation :: Tree a -> Tree a
leftRotation (Node p a (Node q b c)) = Node q (p a b) c
-}


data VTree a = VEmpty | VNode a [VTree a]
instance Functor VTree where
    fmap f VEmpty = VEmpty
    fmap f (VNode val subtrees) = VNode (f val) (map (fmap f) subtrees)



treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty           = Empty
treeMap  f (Node val l r) = Node (f val) (treeMap f l)
                                         (treeMap f r)


instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Node val l r) = Node (f val) (fmap f l)
                                         (fmap f r)



data BST a = BEmpty | BNode a (BST a) (BST a)

instance Show a => Show (BST a) where
    show t = show' 0 t
        where show' pad BEmpty = replicate pad ' ' ++ "#"
              show' pad (BNode val l r) = show' (pad+2) l
                                   ++ "\n"
                                   ++ replicate pad ' ' ++ show val
                                   ++ "\n"
                                   ++ show' (pad+2) r

bst :: BST Int
bst = BNode 10 (BNode 5 (BNode 4 BEmpty BEmpty)
                        (BNode 7 (BNode 6 BEmpty BEmpty)
                                 BEmpty))
               (BNode 20 (BNode 11 BEmpty BEmpty) 
                          BEmpty)


bstInsert :: Ord a => a -> BST a -> BST a
bstInsert val BEmpty           = BNode val BEmpty BEmpty
bstInsert val (BNode root l r) = if root >= val then BNode root (bstInsert val l) r
                                 else BNode root l (bstInsert val r)


bstFromList :: Ord a => [a] -> BST a
bstFromList = foldr bstInsert BEmpty


bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ BEmpty             = False
bstSearch val (BNode root l r) = if root == val then True
                                 else if root > val then bstSearch val l
                                 else bstSearch val r


bstValues :: BST a -> [a]
bstValues BEmpty           = []
bstValues (BNode root l r) = bstValues l ++ [root] ++ bstValues r
 

bstSize :: BST a -> Int
bstSize BEmpty           = 0
bstSize (BNode root l r) = 1 + bstSize l + bstSize r 


bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . bstFromList


data Map k v = MEmpty | MNode k v (Map k v) (Map k v) deriving Show

dictionary :: Map Int String

dictionary = MNode 2 "abc" (MNode 1 "def" MEmpty MEmpty) 
                           (MNode 3 "ghi" MEmpty MEmpty)

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert k v MEmpty            = MNode k v MEmpty MEmpty
mapInsert k v (MNode rk rv l r) = if k == rk then MNode k v l r
                                  else if k < rk then MNode rk rv (mapInsert k v l) r
                                  else MNode rk rv l (mapInsert k v r)


mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch _ MEmpty            = Nothing
mapSearch key (MNode k v l r) = if key == k then Just v
                                else if key > k then mapSearch key r
                                else mapSearch key l


data Direction = L | R deriving Show


bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath _ BEmpty             = Nothing
bstPath val (BNode root l r) = if val == root then Just []
                               else if val < root then (L:) <$> (bstPath val l)
                               else (R:) <$> (bstPath val r)


data Currency = BGN | USD | EUR deriving (Eq, Show)

exchangeRate :: Currency -> Currency -> Float
exchangeRate USD BGN = 1.6
exchangeRate BGN USD = 1 / exchangeRate USD BGN
exchangeRate EUR BGN = 1.95
exchangeRate BGN EUR = 1 / exchangeRate EUR BGN
exchangeRate USD EUR = (exchangeRate USD BGN) * (exchangeRate BGN EUR)
exchangeRate EUR USD = 1 / exchangeRate USD EUR

type Sum = (Float, Currency)

exchange :: Sum -> Currency -> Sum
exchange (s, c) curr = if c == curr then (s, c)
                       else (s * r, curr)
                       where r = exchangeRate c curr


data Expr = Const Double 
    | X
    | Expr :+: Expr
    | Expr :-: Expr
    | Expr :*: Expr
    | Expr :/: Expr
    | Expr   :^^: Double
    | Double :**: Expr
    deriving Show

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^^:
infixr 8 :**:

eval :: Expr -> Double -> Double
eval (Const a) _   = a
eval X x           = x
eval (e1 :+: e2) x = eval e1 x + eval e2 x
eval (e1 :-: e2) x = eval e1 x - eval e2 x
eval (e1 :*: e2) x = eval e1 x * eval e2 x
eval (e1 :/: e2) x = eval e1 x / eval e2 x
eval (e :^^: c)  x = (eval e x) ** c
eval (c :**: e)  x = c ** (eval e x)

derive :: Expr -> Expr
derive (Const _)   = Const 0
derive X           = Const 1
derive (e1 :+: e2) = derive e1 :+: derive e2
derive (e1 :-: e2) = derive e1 :-: derive e2
derive (e1 :*: e2) = derive e1 :*: e2 :+: e1 :*: derive e2
derive (e1 :^^: c) = Const c :*: e1 :^^: (c - 1)
derive (e1 :/: e2) = (derive e1 :*: e2 :-: e1 :*: derive e2) :/: (e2 :^^: 2)
