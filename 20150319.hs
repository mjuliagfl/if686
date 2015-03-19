double :: [Int] -> [Int]
double x |x == [] = x
	     |otherwise = [(2 * head x )] ++ double (tail x)

member :: [Int] -> Int -> Bool
member a b |a == [] = False
	       |head a == b = True
	       |otherwise = member(tail a) b


digits :: String -> String
digits x |x == [] =x
		 |(head x >= '0' && head x <= '9') = [head x] ++ digits (tail x)
	     |otherwise = digits (tail x)

 
sumpairs :: [Int] -> [Int] -> [Int]
sumpairs a b |a ==[] && b ==[] =[]
			 |a ==[] = b
			 |b ==[] = a
			 |otherwise = [(head a + head b)] ++ sumpairs (tail a) (tail b)
 
 -- quicksort
quick :: [Int] -> [Int]
quick x
 |x == [] = []
 |otherwise = (quick (less (head x) (tail x)) ) ++ (head x : quick (greater (head x) (tail x)))


less :: Int -> [Int] -> [Int]
less n l
 |l == [] =[]
 |head l < n = [head l] ++ less n (tail l)
 |otherwise  = less n (tail l)

greater :: Int -> [Int] -> [Int]
greater n l
 |l == [] =[]
 |head l >= n = [head l] ++ greater n (tail l)
 |otherwise = greater n (tail l)