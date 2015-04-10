--QUESTÃO 1
 
--Define uma tabela Hash do tipo (Int,Int)
type HashTable = [(Int,Int)]
 
--Base exemplo
baseHash :: HashTable
baseHash = [(3,4),(9,3)]
 
--Verifica se exise uma chave em O(n) - percorre todos os elementos
hasKey :: HashTable -> Int -> Bool
hashKey [] _ = False
hasKey hash key = ([x| x <- hash, fst(x) == key] /= [])
 
--Insere o par (key,val) em O(n) pois precisa verificar se a chave existe
put :: HashTable -> (Int,Int) -> HashTable
put hash pair
        | not (hasKey (hash) (fst(pair))) = hash ++ [pair]
        | fst(head (hash)) == fst(pair) = [pair] ++ (tail hash)
        | otherwise = [head hash] ++ put (tail (hash)) (pair)
 
--Recupera um par (key,val) em O(n) pois precisa verificar se a chave existe
get :: HashTable -> Int -> (Int,Int)
get [] key = (key,0)
get hash key
        | not (hasKey (hash) (key)) = (key,0)
        | otherwise = (head ([x|x <- hash, fst(x) == key]))
 
--Remove um par (key,val) em O(n) pois percorre todos os elementos
remove :: HashTable -> Int -> HashTable
remove [] _ = []
remove hash key = [x|x <- hash, fst(x) /= key]

--QUESTÃO 2

--Retorna verdadeiro se o numero recebido for membro da lista
membro :: [Int] -> Int -> Bool
membro x n = ([ l | l <- x, l == n]) /= []

--Quicksort para ordenar
quick[] = []
quick(h:t) = quick[x | x <- t, x <= h] ++ h:quick[y|y<-t, y>h]

--Recebe uma lista ordenada e remove os elementos repetidos da mesma
removeRep :: [Int] -> [Int]
removeRep l
 |l == [] = []
 |tail l == [] = l
 |head l == head(tail l) = removeRep (tail l)
 |otherwise = [head l] ++ removeRep (tail l)

--Recebe uma lista , ordena e retorna ela com os repetidos removidos
finalList :: [Int] -> [Int]
finalList l = removeRep (quick l)

--Recebe duas listas e retorna os membros em comum entre elas
intersec :: [Int] -> [Int] -> [Int]
intersec a b = [x| x <- a, membro b x]

--Verifica os casos
comparaConjuntos :: [Int] -> [Int]-> String
comparaConjuntos a b
 | a==b && b==[] = "A igual a B" -- se ambas forem vazias elas são iguais
 |intersec (finalList a) (finalList b) == [] = "Conjuntos disjuntos" -- se não houver elementos em comum sao disjuntos
 |finalList a == finalList b = "A igual a B" -- se a lista ordenada e com os iguais removidos forem iguais, os conjuntos sao iguais
 --pegam as listas ordenadas e sem repetidos, e verifica-se se a interseção é igual a alguma delas 
 |intersec (finalList a) (finalList b) == finalList a = "B contem A"
 |intersec (finalList a) (finalList b) == finalList b = "A contem B"
 |otherwise = "A interseciona B"

-- CODIGOS DA AULA
--TAKE
tk :: Ord t => [t] -> Int -> [t]
tk [] n = []
tk l n
 |n <= 0 = []
 |otherwise = [head l] ++ tk (tail l) (n-1)
 
--DROP
dr :: Ord t => [t] -> Int -> [t]
dr [] n = []
dr l n
 |n > 0 = dr (tail l) (n-1)
 |otherwise = l

--TAKE WHILE
tkWhile :: Ord t => (t -> Bool)-> [t] -> [t]
tkWhile f [] = []
tkWhile f l 
 |(f (head l)) = [head l] ++ tkWhile f (tail l) 
 |otherwise = []


--DROP WHILE

drWhile :: Ord t => (t -> Bool)-> [t] -> [t]
drWhile f [] = []
drWhile f l 
 |(f (head l)) =  drWhile f (tail l)
 |otherwise = l 


--ORDENAÇÃO
--mergesort generico
mergeSort :: Ord t => [t] -> [t]
mergeSort x
 |x == [] = []
 |tail x == [] = [head x]
 |otherwise = merge (mergeSort (fst (splitAt (div (length x) 2) x)))  (mergeSort (snd (splitAt (div (length x) 2) x)))

merge :: Ord t => [t] -> [t] -> [t]
merge a b
 |a == [] && b==[] =[]
 |a ==[] = b
 |b == [] = a
 |head a <= head b = head a:merge (tail a) b
 |otherwise = head b:merge a (tail b)

--AGRUPAR

agrupar :: Ord t => [[t]] -> [(t, Int)]
agrupar t = reverse (group (reverse (pairs (join t))))

group ::  Ord t =>  [(t, Int)] ->  [(t, Int)]
group [] = []
group (a:as)
 |as == [] = [a]
 |member as (fst a) = group as
 |otherwise = [a] ++ group as


member :: Ord t =>  [(t, Int)] -> t ->  Bool
member [] _ = False
member ((m, i):as) n
 |m == n = True
 |otherwise = member as n

join :: Ord t => [[t]] -> [t]
join [] = []
join s = head s ++ join (tail s)

quant :: Ord t => t -> [t] -> Int
quant t [] = 0
quant t s
 |head s == t = 1 + quant t (tail s)
 |otherwise = quant t (tail s)

pairs :: Ord t => [t] -> [(t, Int)]
pairs [] = []
pairs l = [(head l, (quant (head l) l))] ++ pairs (tail l)

