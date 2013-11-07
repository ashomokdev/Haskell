import Prelude
import Data.Maybe

-- Lab5 Бєляєвої Юлії 1 гр


data BinTree a = Empty | Node a (BinTree a) (BinTree a)  deriving (Show)

data Balance = Equal | Less | Greater  deriving (Show, Eq)

data AVLTree a = EmptyAVL  | NodeAVL a Balance (AVLTree a) (AVLTree a) deriving (Show)

-- допоміжна функція - стверення дерева, що складається лише з кореня

singleton :: a -> BinTree a
singleton x = Node x Empty Empty

--допоміжна функція - повертає корінь дерева для непустого дерева

getNode::BinTree a -> a
getNode (Node a _ _) = a

--допоміжна функція - перевірка чи пусте дерево
isEmpty :: (Ord a) => BinTree a -> Bool
isEmpty Empty = True
isEmpty _   = False

--1. Написати предикат isSearch tr,  котрий перевіряє чи є бінарне дерево tr деревом пошуку.

isSearch       :: (Ord a) =>  BinTree a -> Bool
isSearch Empty = True
isSearch (Node a left right)
	|(isEmpty(left)) && (isEmpty(right)) = True
	|(isEmpty(left)) && (a>getNode right) = False
	|(isEmpty(left)) && (a<getNode right) =isSearch(right)
	|(isEmpty(right)) && (a<getNode left) = False
	|(isEmpty(right)) && (a>getNode left) = isSearch(left)
	|a<getNode left = False
	|a>getNode right = False
	|a>getNode left = isSearch(left)
	|a<getNode right = isSearch(right)


--2. Написати функцію insSearch tr v – вставки елемента v в дерево пошуку tr.

insSearch     :: (Ord a) =>  BinTree a ->  a -> BinTree a
insSearch Empty x = singleton x
insSearch (Node a left right) x 
	|x==a = Node x left right
	|x<a = Node a (insSearch left x) right
	|x>a = Node a left (insSearch right x)

--3. Написати функцію delSearch tr v – вилучення елемента v з дерева пошуку tr

delSearch     :: (Ord a) =>  BinTree a ->  a -> BinTree a
delSearch Empty x = error "No instance"
delSearch (Node a left right) x 
    | x < a     = Node a (delSearch left x) right
    | x > a     = Node a left (delSearch right x)
    | otherwise = keepBalanced left right
    where  keepBalanced :: (Ord a) => BinTree a -> BinTree a -> BinTree a
           keepBalanced Empty right = right
           keepBalanced left Empty = left
           keepBalanced left right = Node a' left' right
               where a' = findRightMost left
                     left' = delSearch left a' 
                     
                     findRightMost :: (Ord a) => BinTree a -> a
                     findRightMost (Node rm _ Empty) = rm
                     findRightMost (Node _  _ right)   = findRightMost right

--4. Написати функцію sortListl , що сортує список l, використовуючи дерево пошуку:

listToTree :: (Ord a) => [a] -> BinTree a
listToTree xs = foldl insSearch Empty xs

sortList' :: BinTree a -> [a]
sortList' Empty = []
sortList' (Node a left right) = sortList' left ++ [a] ++ sortList' right

sortList       :: (Ord a) => [ a] -> [a ]
sortList [] = []
sortList xs =  sortList'(listToTree xs)

--5. Написати предикат isAVLTree tr,  котрий перевіряє чи є бінарне дерево tr  AVL-деревом.

--допоміжна функція - повертає корінь дерева для непустого AVL дерева

getNodeAVL::AVLTree a -> a
getNodeAVL (NodeAVL a _ _ _) = a

--допоміжна функція - перевірка чи пусте AVL дерево

isEmptyAVL :: (Ord a) => AVLTree a -> Bool
isEmptyAVL EmptyAVL = True
isEmptyAVL _   = False

--допоміжна функція для AVL дерева - перевірка чи є бінарне дерево tr деревом пошуку.
isSearchAVL       :: (Ord a) =>  AVLTree a -> Bool
isSearchAVL EmptyAVL = True
isSearchAVL (NodeAVL a _ left right)
	|(isEmptyAVL(left)) && (isEmptyAVL(right)) = True
	|(isEmptyAVL(left)) && (a>getNodeAVL right) = False
	|(isEmptyAVL(left)) && (a<getNodeAVL right) =isSearchAVL(right)
	|(isEmptyAVL(right)) && (a<getNodeAVL left) = False
	|(isEmptyAVL(right)) && (a>getNodeAVL left) = isSearchAVL(left)
	|a<getNodeAVL left = False
	|a>getNodeAVL right = False
	|a>getNodeAVL left = isSearchAVL(left)
	|a<getNodeAVL right = isSearchAVL(right)

--допоміжна функція - висота AVL дерева
height :: AVLTree a ->Int
height EmptyAVL = 0
height (NodeAVL _ _ left right) = (max (height left) (height right)) + 1

--перевіряю: 
--1. Чи є бінарне дерево деревом пошуку. 
--2. Чи правильно задані параметри Equal, Less, Greater. 
--3. Чи відповідають ці параметри різниці висот лівого і правого піддерев.
--тільки тоді висновок, що дерево є AVL

isAVLTree   :: (Ord a) =>  AVLTree a -> Bool
isAVLTree EmptyAVL = True
isAVLTree (NodeAVL a balance left right)
	|(isSearchAVL (NodeAVL a balance left right)) && (balance==Equal) && (height(left)==height(right)) = isAVLTree (left) && isAVLTree (right)
	|(isSearchAVL (NodeAVL a balance left right)) && (balance==Less) && ((height(right))-(height(left))==1) = isAVLTree (left) && isAVLTree (right)
	|(isSearchAVL (NodeAVL a balance left right)) && (balance==Greater) && ((height(left))-(height(right))==1) = isAVLTree (left) && isAVLTree (right)
	|otherwise = False

--6. Написати функцію insAVLTree tr v – вставки елемента v в AVL-дерево tr.

-- допоміжна функція - стверення дерева, що складається лише з кореня

singletonAVL :: a -> AVLTree a
singletonAVL x = (NodeAVL x Equal EmptyAVL EmptyAVL)

--допоміжні функції

balanceLL (NodeAVL a balance (NodeAVL al balancel tl ul) u)              = (NodeAVL al balancel  tl (NodeAVL a balance ul u))
balanceLR (NodeAVL a balance (NodeAVL al balancel tl (NodeAVL alr balancelr tlr ulr)) u) = (NodeAVL alr balancelr (NodeAVL al balancel tl tlr) (NodeAVL a balance ulr u))
balanceRL (NodeAVL a balance t (NodeAVL ar balancer (NodeAVL arl balancerl trl url) ur)) = (NodeAVL arl balancerl (NodeAVL a balance t trl) (NodeAVL ar balancer url ur)) 
balanceRR (NodeAVL a balance t (NodeAVL ar balancer tr ur))              = (NodeAVL ar balancer (NodeAVL a balance t tr) ur)


insAVLTree :: (Ord a) =>  AVLTree a ->  a -> AVLTree a
insAVLTree EmptyAVL x = singletonAVL x
insAVLTree (NodeAVL a balance left right) x
	|x==a = NodeAVL a balance left right
	|(x<a) && (balance==Greater) && (x < getNodeAVL (left)) = balanceLL (NodeAVL a Greater lx right)
	|(x<a) && (balance==Greater) && (x > getNodeAVL (left)) = balanceLR (NodeAVL a Greater lx right)
	|(x>a) && (balance==Less) && (x < getNodeAVL (right)) = balanceRL (NodeAVL a Less left rx)
	|(x>a) && (balance==Less) && (x > getNodeAVL (right)) = balanceRR (NodeAVL a Less left rx)
	|(x<a) = (NodeAVL a Equal lx right)
	|(x>a) = (NodeAVL a Equal left rx)
	where lx = insAVLTree left x
	      rx = insAVLTree right x
   