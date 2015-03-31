--TRABALHO 4:
--Questão 1:
{-
A vantagem da abordagem usada em haskell é o fato de ser mais segura, pois o quando há erros, ele é apontado em
tempo de compilação.
O polimorfismo em java, usando generics, é bem mais geral, pois como não restringe os dados, está mais
sujeito a erros, pois a verificação de tipo é feita somente em tempo de execcução.

-}

--Questão 2:

lookAndSay :: Int -> String
lookAndSay n
 |n == 0 = ""
 |n == 1 = "d" -- no caso é "d" pois estou fazendo começando com um char. Mas se fosse fazer 
               --começando com um int, seria "1"
 |otherwise= say (n-1) "d" -- mesma coisa

say :: Int -> String -> String
say n s
 |n == 0 = s
 |otherwise = say (n-1) (transform s 1)

quant :: String -> Int-> String
quant s n
 |s == "" = ""
 |tail s == "" = show n
 |head s == head(tail s) = quant (tail s) (n+1)
 |otherwise = (show n) ++ quant (tail s) 1

removeRep :: String -> String
removeRep l
 |l == "" = ""
 |tail l == "" = l
 |head l == head(tail l) = removeRep (tail l)
 |otherwise = [head l] ++ removeRep (tail l)

intercala :: String -> String -> String
intercala a b
 |a == "" = ""
 |otherwise = ([head a] ++ [head b]) ++ intercala (tail a) (tail b)

transform :: String -> Int -> String
transform s n = intercala (quant s n) (removeRep s)


--Questão 3:

baseGrafo :: Graph Int
baseGrafo = [(3,[1]), (1,[3,2,4,6]), (2,[1]), (4,[1,5,6]), (6,[1,4,5]), (5,[4,6]), (7,[])]

--Adj representa uma lista de adjacencia
type Adj a = [a]
--Ver representa um vertice com sua lista de adjacencia
type Ver a = (a, Adj a);
--Graph represena uma lista de Vértices (com suas listas de adjacencia)
type Graph a = [Ver a];

--Custo de remover um vértice O(n)
removeVertex:: Eq a => Graph a -> a -> Graph a
removeVertex grafo target = [x | x <- grafo, fst(x) /= target]

--Custo de pesquisar se é membro O(n)
isMember:: Eq a => Graph a -> a -> Bool
isMember grafo vertex = [x | x <- grafo, fst(x) == vertex] /= []

--Custo de conseguir a lista de adjacência de um vértice O(n)
adjacentList:: Eq a => Graph a -> a -> Adj a
adjacentList [] _ = []
adjacentList grafo target
	| fst(head grafo) == target = snd(head grafo)
	| otherwise = adjacentList (tail grafo) (target)


search:: Eq a => Graph a -> a -> a -> [a]
search grafo comeco fim
	| (not(isMember (grafo) (comeco))) || (not(isMember (grafo) (fim))) = []
	| comeco == fim = [comeco]
	| ret == [] = []
	| otherwise = [comeco] ++ (head ret)
	where dfs = [search (removeVertex (grafo) (comeco)) (x) (fim) | x <- (adjacentList (grafo) (comeco)), isMember (grafo) (x)]; ret = [x | x <- dfs, x /= []]

--Questão 4:

base :: [[Int]]
base = [[3,3,3,2],[3,2,2,2],[2,3,3,3],[2,2,2,3]]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (f:s) = quicksort ([x | x <- (s), x <= f]) ++ [f] ++ quicksort ([x | x <- (s), x > f])

getMediana :: [Int] -> Int
getMediana [] = 0
getMediana (f:s)
	| mod (length (sorted)) (2) == 1 = (sorted) !! (div (length (sorted)) (2))
	| otherwise = div (((sorted) !! (div (length (sorted)) (2))) + ((sorted) !! ((div (length (sorted)) (2)) - 1))) (2)
	where sorted = quicksort (f:s)

getCols :: [[Int]] -> Int -> Int -> Int -> [[Int]]
getCols [] _ _ _ = []
getCols (f:s) comeco fim index
	| index > fim = []
	| index >= comeco && index <= fim = [f] ++ getCols (s) (comeco) (fim) (index+1) 
	| otherwise = getCols (s) (comeco) (fim) (index+1)

cols2List :: [[Int]] -> [Int]
cols2List [] = []
cols2List (f:s) = [x | x <- f] ++ cols2List s

buildMedianCol :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
buildMedianCol [] _ _ _ _ = []
buildMedianCol (f:s) i j filterN len
	| j >= len = []
	| j < div (filterN) (2) = [((f:s)!!i)!!j] ++ buildMedianCol (f:s) (i) (j+1) (filterN) (len)
	| j >= ((len) - div (filterN) (2)) = [((f:s)!!i)!!j] ++ buildMedianCol (f:s) (i) (j+1) (filterN) (len)
	| otherwise = [mediana] ++ buildMedianCol (f:s) (i) (j+1) (filterN) (len)
	where mediana = getMediana (cols2List (getCols (f:s) (i-(div (filterN) (2))) (i+(div (filterN) (2))) 0))

median :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
median [] _ _ _ _ = []
median (f:s) i j filterN len
	| i >= len = []
	| i < div (filterN) (2) = [(f:s)!!i] ++ median (f:s) (i+1) (j) filterN len
	| i >= ((len) - div (filterN) (2)) = [(f:s)!!i] ++ median (f:s) (i+1) (j) filterN len
	| otherwise = [buildMedianCol (f:s) (i) (0) (filterN) (length f)] ++ median (f:s) (i+1) (j) filterN len

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana [] _ = []
filtroMediana (f:s) filterN = median (f:s) 0 0 (filterN) (length (f:s))

-------------------------------
--ATIVIDADES DA AULA
-- Questão 1
afd :: String -> [Int] -> [(Int, Int, String)] -> Int -> [Int] -> Bool
afd [] _ _ _ _ = False
afd c s t ini fin = fst (percorre c s t ini fin) && member fin (snd(percorre c s t ini fin))

transit :: String -> Int -> [(Int, Int, String)] -> [Int]
transit ch s l = [b | (a, b, c) <- l, (ch == c && s == a) ]

percorre :: String -> [Int] -> [(Int, Int, String)] -> Int -> [Int] -> (Bool, Int)
percorre c s t ini fin
 |c == [] = (True, ini)
 |transit [(head c)] ini t == [] = (False, ini)
 |otherwise = percorre (tail c) s t ( (transit [(head c)] ini t) !! 0) fin 

member :: [Int] -> Int -> Bool
member x n = ([ l | l <- x, l == n]) /= []