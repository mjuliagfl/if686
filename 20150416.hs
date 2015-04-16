--TRABALHO 8

listPartitioner :: Ord a => Num a => [a] -> ([a] -> [[a]])
listPartitioner l = func 
 where func t = [x | x <- (aux (quick l) t) , x /= []];
 			aux o lis
  				|o == [] = [lis]
  				|otherwise = [fst(splitList lis (head o))] ++ aux (tail o) (snd (splitList lis (head o)))


splitList :: Ord t => [t] -> t -> ([t], [t])
splitList l n= ([x | x <- l, x <= n], [x | x <- l, x > n])


quick[] = []
quick(h:t) = quick[x | x <- t, x <= h] ++ h:quick[y|y<-t, y>h]

---QUESTÕES DA AULA

func :: (t -> u -> v) -> (u -> t -> v)
func f  = \p1 p2 -> f (p2) (p1)

firsts :: ([(t, t)] -> [t])
firsts = \x -> [fst n| n <- x ]

newList :: ([[n]] -> Int -> [[n]])
newList = \x n-> [y | y <- x, (length y) > n]

removeDup :: Ord n => Eq n => ([[n]] -> [n])
removeDup = \x -> aux x
 where aux n = remove (quick (fun n));
 	fun n 
 	 |n == [] = []
 	 |otherwise = (head n) ++ fun (tail n)

remove :: Eq n => [n] -> [n]
remove l
 |l == [] = []
 |tail l == [] = l
 |head l == head(tail l) = remove (tail l)
 |otherwise = [head l] ++ remove (tail l)
 
--map.foldr

--aplição parcial
sumElem x l = map (+x) l  

--greaterElem :: Ord => [n] -> n
--greaterElem l = 