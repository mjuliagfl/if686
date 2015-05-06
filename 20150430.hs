data Failable t  = Error String| Correct t deriving (Show)


instance Monad Failable where
	(>>=) (Error x) _ = Error x
	(>>=) (Correct x) f = f x
	return x = Correct x

data Fila t = Val t Int (Fila t) | Empty  deriving (Show)--esse int guarda o tamanho maximo da fila

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila size first  
 |(size < 1) = Error "Unable to create: the size is too small"
 |otherwise = Correct (first, Val first size (Empty))


push :: t -> Fila t -> Failable (t, Fila t)
push val Empty = criarFila 5 val -- cria fila com tamanho 5 (escolhido aleatoriamente)
push val queue@(Val first size (q2))
 |queueSize queue >= size = Error "the queue is full"
 |otherwise = Correct (val, (pushEnd val queue))



pushEnd :: t -> Fila t -> Fila t
pushEnd val queue@(Val first size (Empty)) = (Val first size (Val val size Empty))
pushEnd val queue@(Val first size (q2)) = Val first size (pushEnd val q2)



queueSize :: Fila t -> Int
queueSize Empty = 0
queueSize queue@(Val first size (q2))
 |otherwise = 1 + queueSize q2

pop :: Fila t -> Failable (t, Fila t)
pop Empty = Error "the queue is empty"
pop queue@(Val first size (Empty)) = Correct (first, Empty)
pop queue@(Val first size (Val second size1 (tailtail))) = Correct (first, Val second size tailtail)

peek :: Fila t -> Failable (t, Fila t)
peek Empty = Error "the queue is empty"
peek queue@(Val first size (q2)) = Correct (first, queue)

