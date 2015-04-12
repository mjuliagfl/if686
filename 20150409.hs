import Data.Char

--TRABALHO 6
data Grafo t = NillG | NodeGraph t [(Grafo t, Int)] deriving (Eq, Show)

g1 :: Grafo Int
g1 = NodeGraph (1) [(NodeGraph (2) [(NodeGraph (3) [(NillG,0)], 10)], 4),
					(NodeGraph (5) [((g1),10)], 2)]

isMarked :: Eq t => [t] -> Grafo t -> Bool
isMarked [] _ = False
isMarked (f:e) NillG = False
isMarked (f:e) (NodeGraph a (f1:e1))
	| f == a = True
	| otherwise = isMarked (e) ((NodeGraph a (f1:e1)))


doDFS :: Eq t => Grafo t -> t -> [t] -> Bool
doDFS NillG _ _ = False
doDFS (NodeGraph a (f:e)) (val) []
	| a == val = True
	| otherwise = hasTrue /= []
		where dfs = [(doDFS (fst (x)) (val) ([a]++([])))| x <- (f:e), not(isMarked ([]) (fst(f)))];
				hasTrue = [x | x <- dfs, x]
doDFS (NodeGraph a (f:e)) (val) (f2:e2)
	| a == val = True
	| otherwise = hasTrue /= []
		where dfs = [(doDFS (fst (x)) (val) ([a]++(f2:e2)))| x <- (f:e), not(isMarked (f2:e2) (fst(f)))];
				hasTrue = [x | x <- dfs, x]


dfsGraph :: Eq t => Grafo t -> t -> Bool
dfsGraph NillG _ = False
dfsGraph (NodeGraph a (f:e)) (val) = doDFS ((NodeGraph a (f:e))) (val) ([])



--EXERCICIOS DA AULA

--1)
map1 :: (t -> u) -> [t] -> [u]
map1 f [] = []
map1 f (a:as) = f a : map1 f as

sqrt1 :: Float -> Float
sqrt1 x = sqrt x

sqrtList x = map1 sqrt1 x

--2)
posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto x = map1 posic x

posic :: Char -> Int
posic x = (ord x) - 96

--3)

map2 :: (t -> u) -> [t] -> [u]
map2 f []= []
map2 f a = [f x | x <- a]

--4)
foldr2 :: (t -> u -> u) -> u -> [t] -> u
foldr2 f s [] = s
foldr2 f s (a:as) = f a (foldr2 f s as)

member :: Eq t => t -> [t] -> Bool
member n l = foldr2 (||) False (map (== n) l)

--5)
--union :: Eq t => [t] -> [t] -> [t]
--union x l = x ++ foldr2 (me 


--6)
somaC :: String -> Int
somaC s = foldr2 (+) 0 (posicaoAlfabeto s)

somaCaracteres :: [String] -> [Int]
somaCaracteres l = [somaC x | x <- l] 


--7)
data Tree t = NilT | NodeT t (Tree t) (Tree t) deriving(Eq, Show, Ord)

insert :: Ord t => t -> Tree t -> Tree t
insert n NilT = NodeT n (NilT) (NilT)
insert n (NodeT x (left) (right))
 |n == x = (NodeT x (left) (right))
 |(n > x) = NodeT x (left) (insert n right)
 |otherwise = NodeT x (insert n left) (right)
--NodeT n (NodeT n (NilT)(NilT)) (NodeT n (NilT)(NilT))


--8)
criarArvore :: Ord t => [t] -> (t -> Tree t -> Tree t) -> Tree t
criarArvore l f = foldr2 f NilT (reverse l) 

criarArvoreInt :: [Int] -> Tree Int
criarArvoreInt l = criarArvore l insert


----------------
--filter
filter2 :: (t -> Bool) -> [t] -> [t]
filter2 f l = [x | x <- l, f x]
--9)
positives l = filter2 positive l
 where positive n = (n >= 0)

--10) (3)
inter :: Eq t => [t] -> [t] -> [t]
inter l1 l2 = filter2 both l1
 where both n = (member n l1) && (member n l2)
--11) (4)

diff :: Eq t => [t] -> [t] -> [t]
diff l1 l2 = (filter2 diffe l1 ) ++ (filter2 diffe l2 )
 where diffe n =  not ((member n l1) && (member n l2))