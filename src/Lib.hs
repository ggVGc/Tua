{-# LANGUAGE GADTs #-}

module Lib where



data ArithExpr where
  Lit :: Integer -> ArithExpr
  Add :: ArithExpr -> ArithExpr -> ArithExpr
  Mul :: ArithExpr -> ArithExpr -> ArithExpr
  Sub :: ArithExpr -> ArithExpr -> ArithExpr
  deriving (Show, Eq)

instance Num ArithExpr where
  x + y         = Add x y
  x * y         = Mul x y
  x - y         = Sub x y
  abs x         = error "Not implemented"
  signum x      = error "Not implemented"
  fromInteger = Lit


data Expr a where
  Num      :: ArithExpr -> Expr Integer
  Str      :: String -> Expr String
  LitFalse :: Expr Bool
  LitTrue  :: Expr Bool
  Equal    :: Expr Integer -> Expr Integer -> Expr Bool
  If       :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lam      :: String -> Expr a -> Expr a
  App      :: Expr a -> Expr a -> Expr a
  Let      :: String -> Expr a -> Expr a -> Expr a
  Var      :: String -> Expr a
  Prim     :: Prim -> Expr a


infixl 9 `App`

data Prim =
  Insert
  | EmptyRec
  | Member


gen :: Expr a -> String


gen (App (Prim Member) arg)  = "recMember("++gen arg++")"
gen (App (Prim Insert) arg)  = "recInsert("++gen arg++")"
gen (App (Prim EmptyRec) arg)  = "newRecord()"
gen (App expr arg)  = gen expr++"("++gen arg++")"

gen (Num ae) = genArith ae

gen (Str s)    = s

gen LitFalse    = "false"
gen LitTrue     = "true"

gen (If p e1 e2)  = "if("++gen p++") then "++gen e1++" else "++gen e2++" end "

gen (Equal a b)  = "("++gen a++") == ("++ gen b ++")"

gen (Lam name expr) =
  "function ("++name++")" ++gen expr++"end"

gen (Var name ) = name

gen (Let name expr cont) =
  name++" = "++ gen expr++"\n"++gen cont


genArith :: ArithExpr -> String
genArith (Lit x)   = show x
genArith (Add x y) = "("++(genArith x ++ "+" ++genArith y)++")"
genArith (Mul x y) = "("++(genArith x ++ "*" ++genArith y)++")"
genArith (Sub x y) = "("++(genArith x ++ "-" ++genArith y)++")"


