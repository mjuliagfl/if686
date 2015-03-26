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


