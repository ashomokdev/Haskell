import Prelude

-- Lab6 Бєляєвої Юлії 1 гр

data OrdTree a = OrdTree a [OrdTree a]  deriving (Show)

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)


-- This is a simple tree used for test purposes
t = OrdTree 5 [OrdTree 3 [OrdTree 1 [], OrdTree 4[]], OrdTree 7 []] 
h = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))


--1. sumTree tr - обраховує суму всіх елементів, що знаходяться в вузлах впорядкованого дерева.

sumTree :: (Num a) =>  OrdTree a -> a
sumTree (OrdTree a b) = a + sum(fmap sumTree b)

--2. dfsTreeList lt – повертає всі вершини списку впорядкованих дерев lt в порядку обходу дерев в глибину.

dfsTree :: OrdTree a ->  [a]
dfsTree (OrdTree a b ) = a : concat[dfsTree n|n <- b]

--3. bfsTreeList lt – повертає всі вершини списку впорядкованих дерев lt в порядку обходу дерев в ширину.
getListOfTrees (OrdTree a []) = []
getListOfTrees (OrdTree _ b) = b

getRootOfTree (OrdTree a _) = a

bfsTree :: OrdTree a -> [a]
bfsTree a = bfs [a] 
	where 
	bfs [] = []
	bfs tree = (map getRootOfTree tree) ++ bfs (concat (map getListOfTrees tree))


--4. equalTree tr1 tr2 - перевіряє чи рівні два впорядковані дерева tr1 і tr2.

equelTree :: (Eq a) => OrdTree a -> OrdTree a -> Bool
equelTree (OrdTree n1 xs1) (OrdTree n2 xs2) = n1 == n2 && listTreeEquals xs1 xs2
    where
    listTreeEquals [] [] = True
    listTreeEquals (x1 : xs1) (x2 : xs2) = equelTree x1 x2 && listTreeEquals xs1 xs2
    listTreeEquals _ _ = False
treeEquals _ _ = False

--5.isInTree tr1 tr2 - перевіряє чи є впорядковане дерево tr1 піддеревом іншого впорядкованого дерева tr2.

isInTree :: (Eq a) => OrdTree a -> OrdTree a -> Bool
isInTree a b 
	|equelTree a b = True
	|True `elem` (map (isInTree a) (getListOfTrees b)) = True
	|otherwise = False

--6. toBinTree lt – будує за списком впорядкованих дерев lt відповідне бінарне дерево.

toBinTree ::  [OrdTree a] ->  BinTree a
toBinTree [] = Empty
toBinTree (x:xs) = Node (getRootOfTree x) (toBinTree (getListOfTrees x)) (toBinTree xs)

--7. toOrdTree bt – будує за бінарним деревом bt відповідний список впорядкованих дерев.

toOrdTree ::  BinTree a -> [OrdTree a]
toOrdTree Empty = []
toOrdTree (Node a left right) = OrdTree a (toOrdTree (left)) : (toOrdTree (right))
