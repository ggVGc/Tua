{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib





main :: IO ()
main = do
  let
    program =
      Let "double" (Lam "x" ("x"*"x")) (
          Let "res" (
              "double" `App` 20
            )
            "print" `App` "res"

        )

  putStrLn (gen program)


  -- Generated:
  --  double = function (x) return (x*x)end
  --  res = double(20)

  -- Lua Output:
  --  400








--  i'd recommend to following steps:
--  1. use simple named variables as explained earlier
--  2. switch to de bruijn indices observe the weaknesses of both
--  3. learn about the "locally nameless" approach (search for "i'm not a number, i'm a free variable")
--  4. add a manually checked type system using a simple custom type language:  data Type = Func Type Type | IntType;  Lam :: Name -> Type -> Expr -> Expr  -- give each bound variable a 
--  type signature
--  5. learn about singleton types, type-level computation and propositional equality
--  6. reuse haskell's type system

