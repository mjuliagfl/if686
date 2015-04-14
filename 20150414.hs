--TRABALHO 7
--1)
compose :: (u -> v )-> [(t -> u)] -> [(t -> v)]
compose f [] = []
compose f (a:as) = comp a f  : compose f as


comp :: (t -> u) -> (u -> v) -> t -> v
comp f g e = g (f e)


--2)

g1 :: Grafo Int
g1 = NodeGraph (1) [(NodeGraph (2) [(NodeGraph (3) [(NillG,0)], 10)], 4),
					(NodeGraph (4) [(NillG,0)], 2)]

data Grafo t = NillG | NodeGraph t [(Grafo t, Int)] deriving (Eq, Show)

isMember :: Eq a => Grafo a -> [Grafo a] -> Bool
isMember _ [] = False
isMember (node) (f:s) = [x | x <- (f:s), x == node] /= []

doMapGraph :: Eq a => (a -> b) -> Grafo a -> [Grafo a] -> Grafo b
doMapGraph funct (NillG) _ = NillG
doMapGraph funct (NodeGraph a lista) [] = NodeGraph (funct a) (z)
	where z = [ ((doMapGraph (funct) (fst(x)) ([(NodeGraph a lista)]) ), snd(x)) | x <- lista]
doMapGraph funct (NodeGraph a lista) marcados = NodeGraph (funct a) (z2)
	where z2 = [ ((doMapGraph (funct) (fst(x)) ([(NodeGraph a lista)] ++ marcados) ), snd(x)) | x <- lista]

mapGraph :: Eq a => Eq b => (a -> b) -> Grafo a -> Grafo b
mapGraph funct NillG = NillG
mapGraph funct (NodeGraph a lista) = (doMapGraph (funct) ((NodeGraph a lista)) ([]))

getVals :: Eq a => [[Grafo a]] -> [Grafo a]
getVals lista
	| lista == [] = []
	| otherwise = [x | x <- head (lista)] ++ (getVals (tail lista))

getVertex :: Eq t => (Grafo t) -> [Grafo t] -> [Grafo t]
getVertex (NillG) marcados = []
getVertex (NodeGraph a lista) marcados
	| not(isMember (NodeGraph a lista) marcados) = [(NodeGraph a lista)] ++ adj ++ get
	| otherwise = adj ++ get
		where adj = [fst(x) | x <- lista, not(isMember (NodeGraph a lista) marcados) && fst(x) /= NillG];
			get = getVals ([(getVertex (x) (marcados ++ adj))| x <- adj] )

doFoldGraph :: Eq t => (Grafo t -> b -> b) -> b -> [Grafo t] -> b
doFoldGraph funct base [] = base
doFoldGraph funct base lista = funct (head lista) (doFoldGraph (funct) (base) (tail lista))

foldGraph :: Eq t => (Grafo t -> b -> b) -> b -> Grafo t -> b
foldGraph funct base grafo = (doFoldGraph funct base (getVertex grafo []))



--3)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq,Show)

doFilter :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
doFilter funct NilT = [NilT]
doFilter funct (Node t left right)
	| (funct t) = [(Node t (head (doFilter funct left)) (head (doFilter funct right)))]
	| otherwise = [NilT]

getInvalid :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
getInvalid funct NilT = []
getInvalid funct (Node t left right)
	| (funct t) = (getInvalid (funct) (left)) ++ (getInvalid (funct) (right))
	| otherwise = [(Node t left right)] ++ (getInvalid (funct) (left)) ++ (getInvalid (funct) (right))

unDoDoubleList :: Eq t => [[t]] -> [t]
unDoDoubleList [] = []
unDoDoubleList (f:s) = [x | x <- f] ++ unDoDoubleList s

filterTree :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
filterTree funct NilT = []
filterTree funct (Node t left right)
	| (funct t) = doIt ++ z2
	| otherwise = (filterTree funct left) ++ (filterTree funct right)
		where doIt = (doFilter funct (Node t left right));
			z = (getInvalid funct (Node t left right));
			z2 = unDoDoubleList ([ filterTree (funct) (x) | x <- z])


---------------------------------------------------------------------------------
--ATIVIDADES DA AULA

--filter
filter2 :: (t -> Bool) -> [t] -> [t]
filter2 f l = [x | x <- l, f x]

filterList :: [[Int]] -> Int -> [[Int]]
filterList l i = filter func l where func lis = (i > ( foldr (+) 0 lis) )


member :: Eq t => t -> [t] -> Bool
member n l = foldr (||) False (map (== n) l)

inter :: Eq t => [t] -> [t] -> [t]
inter l1 l2 = filter2 both l1
 where both n = (member n l1) && (member n l2)


diff :: Eq t => [t] -> [t] -> [t]
diff l1 l2 = (filter2 diffe l1 ) 
 where diffe n =  not ((member n l1) && (member n l2))

