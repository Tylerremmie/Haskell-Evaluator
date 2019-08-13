-- Implement function eval in the following Haskell program that evaluates expressions to their integer values.
-- NAME: Tyler Remmie

data Op = BinOp | UniOp
data BinOp = Add | Sub | Mult | Pow deriving Show
data UniOp = Neg | Inc deriving Show
data Expr a = BinExp BinOp (Expr a) (Expr a) | UniExp UniOp (Expr a) | Val a deriving Show


-- defining semantics of binary operators
binsemantic Add  = (+)
binsemantic Sub  = (-)
binsemantic Pow  = (^)
binsemantic Mult = (*)

-- defining semantics of unary operatirs
unisemantic Neg  = (*(-1))
unisemantic Inc  = (+1)


-- eval should compute the value of an expression
eval :: Expr Integer -> Integer
eval (BinExp op exp1 exp2) = binsemantic op (eval exp1) (eval exp2)
eval (UniExp op exp) = unisemantic op (eval exp)
eval (Val n) = n

-- Test
a = UniExp Neg b
b = UniExp Neg (Val 10)
c = BinExp Add (BinExp Add (Val 2) (Val 3)) (BinExp Sub (Val 1) (Val 2)) 

-- eval a 
-- eval b
-- eval c