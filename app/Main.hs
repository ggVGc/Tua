module Main where

import Lib




-- 04:58 < ertes> and it'll be a lot more fun, too, because you can observe your results
-- 04:58 < ertes> i'd recommend to following steps:
-- 04:58 < ertes> 1. use simple named variables as explained earlier
-- 04:59 < ertes> 2. switch to de bruijn indices
-- 04:59 < ertes> observe the weaknesses of both
-- 04:59 < ertes> 3. learn about the "locally nameless" approach (search for "i'm not a number, i'm a free variable")
-- 4. add a manually checked type system using a simple custom type language:  data Type = Func Type Type | IntType;  Lam :: Name -> Type -> Expr -> Expr  -- give each bound variable a 
-- type signature
-- 05:01 < ertes> 5. learn about singleton types, type-level computation and propositional equality
-- 05:01 < ertes> 6. reuse haskell's type system





main :: IO ()
main = do
  let
    fun = Lam "x" (If (Equal (Num 3) (Var "x")) (Num 2) (Num 5))
    appl =
      Let "foo" fun (
          App ( Var "foo" ) (Num 2)
        )

    acc = Prim Member `App` Var "foo" `App` Var "bar"

  putStrLn (gen acc)
  -- putStrLn (gen appl)
