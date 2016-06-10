{-# LANGUAGE GADTs #-}

module Lib where

import Data.String


instance Num Expr where
  x + y         = Add x y
  x * y         = Mul x y
  x - y         = Sub x y
  abs x         = error "Not implemented"
  signum x      = error "Not implemented"
  fromInteger = Lit

data Expr where
  Lit      :: Integer -> Expr
  Add      :: Expr -> Expr -> Expr
  Mul      :: Expr -> Expr -> Expr
  Sub      :: Expr -> Expr -> Expr
  Str      :: String -> Expr
  LitFalse :: Expr
  LitTrue  :: Expr
  Equal    :: Expr -> Expr -> Expr
  If       :: Expr -> Expr -> Expr -> Expr
  Lam      :: String -> Expr -> Expr
  App      :: Expr -> Expr -> Expr
  Let      :: String -> Expr -> Expr -> Expr
  Var      :: String -> Expr
  Prim     :: Prim -> Expr

instance IsString Expr where
    fromString = Var



-- data Expr a where
--   Num      :: ArithExpr -> Expr Integer
--   Str      :: String -> Expr String
--   LitFalse :: Expr Bool
--   LitTrue  :: Expr Bool
--   Equal    :: Expr Integer -> Expr Integer -> Expr Bool
--   If       :: Expr Bool -> Expr a -> Expr a -> Expr a
--   Lam      :: String -> Expr a -> Expr a
--   App      :: Expr a -> Expr a -> Expr a
--   Let      :: String -> Expr a -> Expr a -> Expr a
--   Var      :: String -> Expr a
--   Prim     :: Prim -> Expr a



infixl 9 `App`

data Prim =
  Insert
  | EmptyRec
  | Member


-- gen :: Expr a -> String
gen :: Expr -> String


gen (App (Prim Member) arg)  = "recMember("++gen arg++")"
gen (App (Prim Insert) arg)  = "recInsert("++gen arg++")"
gen (App (Prim EmptyRec) arg)  = "newRecord()"
gen (App expr arg)  = gen expr++"("++gen arg++")"


gen (Str s)    = s

gen LitFalse    = "false"
gen LitTrue     = "true"

gen (If p e1 e2)  = "if("++gen p++") then "++gen e1++" else "++gen e2++" end "

gen (Equal a b)  = "("++gen a++") == ("++ gen b ++")"

gen (Lam name expr) =
  "function ("++name++") return " ++gen expr++"end"

gen (Var name ) = name

gen (Let name expr cont) =
  name++" = "++ gen expr++"\n"++gen cont

gen (Lit x)   = show x
gen (Add x y) = "("++(gen x ++ "+" ++gen y)++")"
gen (Mul x y) = "("++(gen x ++ "*" ++gen y)++")"
gen (Sub x y) = "("++(gen x ++ "-" ++gen y)++")"



