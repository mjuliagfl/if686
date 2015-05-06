--Define uma tabela Hash do tipo key val
type HashTable a b = [(a,b)]

--Base de exemplo
base :: HashTable Char Int
base = [('a',2),('b',4)]

--Verifica se exise uma chave em O(n) - percorre todos os elementos
hasKey :: Eq t => Eq a => HashTable t a -> t -> Bool
hashKey [] _ = False
hasKey hash key = ([x| x <- hash, fst(x) == key] /= [])

--Insere o par (key,val) em O(n) pois precisa verificar se a chave existe
put :: Eq t => Eq a => HashTable t a -> (t,a) -> HashTable t a
put hash pair
	| not (hasKey (hash) (fst(pair))) = hash ++ [pair]
	| fst(head (hash)) == fst(pair) = [pair] ++ (tail hash)
	| otherwise = [head hash] ++ put (tail (hash)) (pair)

--Recupera um par (key,val) em O(n) pois precisa verificar se a chave existe
get :: Eq t => Eq a => HashTable t a -> t -> Maybe (t,a)
get [] key = Nothing
get hash key
	| not (hasKey (hash) (key)) = Nothing
	| otherwise = Just (head ([x|x <- hash, fst(x) == key]))

--Remove um par que tem chave em O(n)
remove :: Eq t => Eq a => HashTable t a -> t -> Maybe (HashTable t a)
remove [] _ = Nothing
remove hash key
	| not (hasKey (hash) (key)) = Nothing
	| otherwise = Just ([x|x <- hash, fst(x) /= key])