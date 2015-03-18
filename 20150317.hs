vendas :: Int -> Int
vendas n = 3

equal :: Int -> Int -> Int
equal a b  | a == b  = 1
		   | otherwise = 0


vendasRepetidas :: Int -> Int -> Int
vendasRepetidas s n  | n == 0  = equal s (vendas n)
 | otherwise = (vendasRepetidas s (n-1)) + equal s (vendas n)