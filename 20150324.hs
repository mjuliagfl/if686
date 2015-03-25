--Códigos da atividade:

--mergesort
mergeSort :: [Int] -> [Int]
mergeSort x
 |x == [] = []
 |tail x == [] = [head x]
 |otherwise = merge (mergeSort (fst (splitAt (div (length x) 2) x)))  (mergeSort (snd (splitAt (div (length x) 2) x)))
--a complexidade dessa função é O( n log(n)), pois chamamos a função "merge" (cuja complexidade é O(n) ) sobre o passo recursivo, 
--que é feito dividindo a lista na metade, portanto essa parte (as listas que são passadas para o merge) é calculada em O(log(n))

merge :: [Int] -> [Int] -> [Int]
merge a b
 |a == [] && b==[] =[]
 |a ==[] = b
 |b == [] = a
 |head a <= head b = head a:merge (tail a) b
 |otherwise = head b:merge a (tail b)


 --Função auxiliar swap
--Parâmetros: valor1 valor2 posição1 posiçõa2 indx lista
--Retorno: Lista
--Troca o elemento na posição1 por valor2 e troca a posição2 por valor1
--Custo O(n)
swap :: Int -> Int -> Int -> Int -> Int -> [Int] -> [Int]
swap _ _ _ _ _ [] = []
swap a b p1 p2 i (f:s)
	| p1 == i = b : swap a b p1 p2 (i+1) s
	| p2 == i = a : swap a b p1 p2 (i+1) s
	| otherwise = f: swap a b p1 p2 (i+1) s

--Função auxiliar getVal
--Parâmetros: lista posição indx
--Retorno: Inteiro
--Encontra o valor do lista que está na posição target
--Custo: O(n)
getVal :: [Int] -> Int -> Int -> Int
getVal (f:s) target indx
	| target == indx = f
	| otherwise = getVal(s)(target)(indx+1)

--Função auxiliar heapfy
--Parâmetros: lista noAtual tamanho
--Retorno: Lista
--Rearranja a lista de modo que, quando transforma em uma arvore binária,
--o elemento na raiz de cada subárvore é menor ou igual que os elementos filhos.
--Custo: chamaremos heapfy para cada nó -  O(n)
--Cada nó resolverá seus subheapfys n vezes (na verdade menos) - O(n)
--para cada nó chamamos Const*getVal - O(n)
--chamaremos swaps com custo O(n) - O(n)
--Custo total: O(N^4)
heapfy :: [Int] -> Int -> Int -> [Int]
heapfy [] _ _ = []
heapfy (f:s) node len
	--Se não tiver filhos, não precisa fazer nenhuma modificação
	| (2*node + 1) >= len = (f:s)
	--Se tiver ambos os filhos e o valor do noAtual for menor ou igual que o valor
	--de ambos os filhos após heapfy, então apenas aplicamos heapfy aos filhos 
	| (2*node + 2) < len && (getVal (f:s) node 0) <= 
		getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 1) 0
		&& (getVal (f:s) node 0) <= 
			getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 2) 0
			= (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len)
	--Se tiver ambos os filhos mas o filho da esquerda após heapfy for menor que o noAtual
	--Trocamos os valores dos nos e aplicamos heapfy na subArvore da esquerda e então na raiz
	| (2*node + 2) < len && (getVal (f:s) node 0) > 
		getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 1) 0
		= heapfy (heapfy (swap (getVal (f:s) node 0) 
			(getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 1) 0)
			(node) (2*node + 1) (0)
			(heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len)) (2*node + 1) len) (node) len
	--Se tiver ambos os filhos mas o filho da direita após heapfy for menor que o noAtual
	--Trocamos os valores dos nos e aplicamos heapfy na subArvore da direita e então na raiz
	| (2*node + 2) < len && (getVal (f:s) node 0) > 
		getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 2) 0
		= heapfy (heapfy (swap (getVal (f:s) node 0) 
			(getVal (heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len) (2*node + 2) 0)
			(node) (2*node + 2) (0)
			(heapfy (heapfy (f:s) (2*node + 1) len) (2*node + 2) len)) (2*node + 2) len) (node) len
	--Se tiver apens tiver o filho da esquerda mas o mesmo após heapfy for menor que o noAtual
	--Trocamos os valores dos nos e aplicamos heapfy na subArvore da esquerda e então na raiz
	| (getVal (f:s) node 0) > 
		getVal (heapfy (f:s) (2*node + 1) len) (2*node + 1) 0
		= heapfy (heapfy (swap (getVal (f:s) node 0) 
			(getVal (heapfy (f:s) (2*node + 1) len) (2*node + 1) 0)
			(node) (2*node + 1) (0)
			(heapfy (f:s) (2*node + 1) len)) (2*node + 1) len) (node) len
	--Qualquer outra coisa mantemos a lista como está
	| otherwise = (f:s)

--Função auxiliar removeLast
--Parâmetros: lista
--Retorno: Lista
--Retornamos a lista sem o ultimo elemento
--Custo: O(n)
removeLast:: [Int] -> [Int]
removeLast [] = []
removeLast (f:s) 
	| s == [] = []
	| otherwise = f:removeLast(s)

--Função heapSort
--Parâmetros: lista
--Retorno: Lista
--Retornamos a lista ordenada
--Custo: O(n)*O(n^4) -> O(n^5)
heapsort :: [Int] -> [Int]
heapsort [] = []
heapsort (f:s)
	| s == [] = [f]
	| otherwise = [head (heapfy (f:s) (0) (length (f:s)))] ++ 
		heapsort (removeLast ((heapfy (swap (head (heapfy (f:s) (0) (length (f:s))))
			(getVal (heapfy (f:s) (0) (length (f:s))) ((length (f:s))-1) (0))
			(0) ((length (f:s))-1) (0) (heapfy (f:s) (0) (length (f:s)))) (0) ((length (f:s))-1))))



--------------------------------------------------------------------------------------------------------------



--Códigos da aula:


--menorMaior
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (menor x (menor y z), maior x (maior y z))

maior :: Int -> Int -> Int
maior x y 
 |x> y = x
 |otherwise = y

menor :: Int -> Int -> Int
menor x y 
 |x < y = x
 |otherwise = y

--ordenaTripla
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z)
 |(fst (menorMaior x y z)) == x && (snd (menorMaior x y z)) == y = (x, z, y )
 |(fst (menorMaior x y z) )== y && (snd (menorMaior x y z))== x = (y, z, x )
 |(fst (menorMaior x y z)) == z && (snd(menorMaior x y z)) == y = (z, x, y )
 |(fst (menorMaior x y z) )== z && (snd (menorMaior x y z)) == x= (z, y, x )
 |(fst (menorMaior x y z) )== x && (snd (menorMaior x y z)) == z = (x, y, z)
 |otherwise = (y,x,z)

--ponto, reta, vertical
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

primeira :: Ponto -> Float
primeira (a, b) = a

segunda :: Ponto -> Float
segunda (a, b) = b

vertical :: Reta -> Bool
vertical ((a, b), (c,d)) = (a == c)

pontoY :: Float -> Reta -> Float
pontoY px ((x1, y1), (x2, y2))
 |x1==x2 = 0 -- se a reta for vertical y = infinito, portanto pode assummir qualquer valor
 |otherwise = ((y2-y1)*(px-x1))/(x2-x1) + y1

--biblioteca
type Pessoa = String

type Livro = String

type BancoDados = [(Pessoa,Livro)]
baseExemplo :: BancoDados

baseExemplo = 
 [("Sergio","O Senhor dos Aneis"),

 ("Andre","Duna"),
 
 ("Lucas","Duna"),
 
 ("Fernando","Jonathan Strange & Mr. Norrell"), 

 ("Fernando","A Game of Thrones")]

-- livros emprestados

livros :: BancoDados -> Pessoa -> [Livro]
livros b p = [snd x | x <- b, (fst x == p)]

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos b l = [fst x | x <- b, (snd x == l)]
 
emprestado :: BancoDados -> Livro -> Bool
emprestado b l
 |b == [] = False
 |snd(head b) == l = True
 |otherwise = emprestado (tail b) l
 
qtdEmprestimos ::BancoDados -> Pessoa -> Int
qtdEmprestimos b p = length (livros b p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar b p l = b ++ [(p, l)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver b p l 
 |b == [] = []
 |head b == (p,l) = devolver (tail b) p l 
 |otherwise = head b:devolver (tail b) p l 

--utilizando compreensão de listas:
membro :: [Int] -> Int -> Bool
membro x n = ([ l | l <- x, l == n]) /= []

--livros :: BancoDados -> Pessoa -> [Livro]
-- ja tihna feito dessa forma (coódigo mais acima)

--emprestimos :: BancoDados -> Livro ->[Pessoa]
-- ja tinha feito dessa forma (código mais acima)

emprestado2 :: BancoDados -> Livro -> Bool
emprestado2 b l = ([x | x <- b, snd x == l]) /= []

qtdEmprestimos2 :: BancoDados -> Pessoa -> Int
qtdEmprestimos2 b p = length ([x | x <- b, fst x == p])

devolver2 :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver2 b p l = [x | x <- b, x /= (p, l)]

--quicksort 

quick[] = []
quick(h:t) = quick[x | x <- t, x <= h] ++ h:quick[y|y<-t, y>h]





