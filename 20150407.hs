data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle a) = pi * a * a
area (Rectangle b c) = b * c

data DiaSemana = Segunda Int [String]| Terca Int [String]| Quarta Int [String] | Quinta Int [String]| Sexta Int [String]| Sabado | Domingo

fds :: DiaSemana -> Bool
fds (Sabado) = True
fds (Domingo) = True
fds _ = False


plc :: DiaSemana -> Bool
plc (Sabado) = False
plc (Domingo) = False
plc (Segunda _ s) = temPlc s
plc (Terca _ s) = temPlc s
plc (Quarta _ s) = temPlc s
plc (Quinta _ s) = temPlc s
plc (Sexta _ s) = temPlc s


temPlc :: [String] -> Bool
temPlc s
 |length s == 0 = False
 |length s == 1 = (head s == "PLC")
 |head s == "PLC" = True
 |otherwise = temPlc (tail s)


data Tree t = NilT | Node t (Tree t) (Tree t) deriving(Eq, Show)

data Expr = Lit Int |
 Add Expr Expr |
 Sub Expr Expr


showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Sub e1 e2) = showExpr e1 ++ " - " ++ showExpr e2

data List t = Nil | Cons t (List t)

--toList :: List t -> [t]