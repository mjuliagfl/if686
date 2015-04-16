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
