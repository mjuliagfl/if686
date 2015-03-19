double :: [Int] -> [Int]
double x |x == [] = x
	     |otherwise = [(2 * head x )] ++ double (tail x)

member :: [Int] -> Int -> Bool
member a b |a == [] = False
	       |head a == b = True
	       |otherwise = member(tail a) b


digits :: String -> String
digits x |x == [] =x
	     |otherwise = number (head x) ++ digits (tail x)


number :: Char -> [Char]
number x 
 |x == '0' = ['0']
 |x == '1' = ['1']
 |x == '2' = ['2']
 |x == '3' = ['3']
 |x == '4' = ['4']
 |x == '5' = ['5']
 |x == '6' = ['6']
 |x == '7' = ['7']
 |x == '8' = ['8']
 |x == '9' = ['9']
 |otherwise = []

 
sumpairs :: [Int] -> [Int] -> [Int]
sumpairs a b |a ==[] && b ==[] =[]
			 |a ==[] = b
			 |b ==[] = a
			 |otherwise = [(head a + head b)] ++ sumpairs (tail a) (tail b)
 