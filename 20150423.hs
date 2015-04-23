--QUESTOES DO TRABALHO (PRIMEIRA PARTE)
--NUMERAÇÃO DAS QUESTÕES BASEADA NO ARQUIVO .txt COM O MESMO NOME (20150423.txt)
--1)
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

pairsFib :: Int -> [Int]
pairsFib 0 = []
pairsFib n 
 |n == 0 = [0]
 |otherwise = pairsFib(n-1) ++ [fibo ((3*n) - 3)]

--2)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (h:t) = ordenar[x | x <- t, (sumDigits x) < (sumDigits h)] ++ h:ordenar[y|y<-t,  (sumDigits y) >=(sumDigits h)]

sumDigits :: Int -> Int
sumDigits n 
 |n < 10 = n
 |otherwise = (mod  n 10) + sumDigits(div n 10)

--3)
--3.1
getWord :: String -> String -- retorna a primeira palavra da string
getWord l = takeWhile (/= ' ') l

--3.2
dropWord :: String -> String -- retorna a string sem a primeira palavra
dropWord l = dropWhile (/= ' ') l


--3.3
dropSpace :: String -> String --remove os espacos do comeco da string
dropSpace [] = []
dropSpace l 
 |head l == ' ' = dropSpace(tail l)
 |otherwise = l


--3.4
type Word = String
splitWords :: String -> [Word] --pega cada palavra da string e coloca numa lista de palavras
splitWords [] = [[]]
splitWords s 
 |(dropWord s /= []) && ((tail (dropWord s) /= []) && (getWord s /= [])) = [getWord s] ++ splitWords( tail (dropWord s))
 |(dropWord s /= []) && (tail (dropWord s) /= []) = splitWords( tail (dropWord s))
 |otherwise = [getWord s]

--3.5
type Line = [Word]

--"Dado o número de caracteres em uma linha, devolve uma lista de palavras que não extrapola esse limite a partir de uma outra lista de palavras: "
getLine1 :: Int -> [Word] -> Line
getLine1 n [] = []
getLine1 n l
 |length(head l) <= n = [head l] ++ getLine1 (n- (length(head l))) (tail l)
 |otherwise = []


--3.6
--Análogo ao anterior, mas descarta as primeiras palavras da lista.
dropLine :: Int -> [Word] -> [Word]
dropLine n [] = []
dropLine n l
 |length(head l) <= n = dropLine (n- (length(head l))) (tail l)
 |otherwise = l

--3.7
--"Quebra uma lista de palavras em uma lista de linhas (uma lista de listas de palavras). 
-- Supõe que existe um tamanho de linha (em caracteres) pré-estabelecido (80, 100, fica a seu critério)."
splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines l 
 |dropLine 50 l /= l = [getLine1 50 l] ++ splitLines (dropLine 50 l) --supondo que o tamanho da linha é 50
 |otherwise =splitLines (tail l)


--fill
fill :: String -> [Line]
fill st = splitLines (splitWords st)

--3.8
joinLines :: [Line] -> String --pega lista de linhas e transforma numa unica string
joinLines [] = []
joinLines l = joinL (head l) ++ joinLines(tail l)

joinL :: Line -> String --pega linha e transforma em string
joinL [] = []
joinL l = (head l ++ " ") ++ joinL (tail l)

--4)
type Vector = [Double]
type Matrix = [Vector]

--transp :: Matrix -> Matrix -- retorna a transposta pra facilitar o calculo da multiplicacao



--multiplicaMatrizes :: Matrix -> Matrix -> Matrix -- as entradas devem ser matrizes quadradas



--5)
data Tree t = NilT | NodeT t (Tree t) (Tree t) deriving(Eq, Show)

bfs :: Eq t => Tree t -> t -> Bool
bfs NilT _ = False
bfs (NodeT x (left)(right)) n
 |x == n = True
 |otherwise = (bfs left n || bfs right n)

 --6)
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (NodeT x left right) = (NodeT (f x) (mapTree f left) (mapTree f right))

--7)
union :: Eq t => [t] -> [t] -> [t]
union a b = a ++ [x | x <- b, not((elem x a) && (elem x b))]

--8)
mapfilter :: (t -> Bool) -> [[t]] -> [[t]]
 -- faz isso : mapfilter f l = map (filter f) l
mapfilter f [] = []
mapfilter f l = [ [z | z <- x, f z] |x <- l]

--9.1)
--mapFold :: (t -> u -> u) -> [u] -> [([u] -> t)]

-----------------------------------------
--QUESTOES DA AULA
