module Expr where

import Data.Char
import Data.Maybe

data Expr 
     = Num Double 
     | Var String
     | Add Expr Expr
     | Mul Expr Expr
     | Sin Expr
     | Cos Expr
   deriving (Show, Eq)

-- defines how our expressions should be written
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Var x) = x
showExpr (Add x y) = showExpr x ++ " + " ++ showExpr y
showExpr (Mul x y) = showFactor x ++ " * " ++ showFactor y
showExpr (Sin x) = showFactor (Sin x)
showExpr (Cos x) = showFactor (Cos x)

showFactor :: Expr -> String 
showFactor (Add x y)       = "(" ++ showExpr (Add x y) ++ ")"
showFactor (Sin x) = "sin " ++ "(" ++ showExpr x ++ ")"
showFactor (Cos x) = "cos " ++ "(" ++ showExpr x ++ ")"
showFactor k               = showExpr k

-- mul (sin (mul 2 2) 2)
eval :: Expr -> Double -> Double
eval (Num n) c   = n
eval (Var x) c   = c
eval (Add x y) c = eval x c + eval y c
eval (Mul x y) c = eval x c * eval y c
eval (Sin x) c   = sin (eval x c) 
eval (Cos x) c   = cos (eval x c)


type Parser expr = String -> Maybe (Expr,String)
-- Parses a string of an expression to the first double
num :: Parser expr
num s = case reads s of
    [(n,s')] -> Just (Num n, s')
    [] -> Nothing

-- 
expr, term :: Parser expr
expr = chain term   '+' Add
term = chain factor '*' Mul


chain :: Parser a -> Char -> (Expr -> Expr -> Expr) -> Parser a
chain p op f s =
  case p s of
    Just (a,s1) -> case s1 of
      c:s2 | c == op -> case chain p op f s2 of
        Just (b,s3) -> Just (f a b, s3)
        Nothing     -> Just (a,s1)
      _ -> Just (a,s1)

factor :: Parser Expr
factor ('(':s) =
   case expr s of
      Just (a, ')':s1) -> Just (a, s1)
factor ('s':'i':'n':s2) =
   case factor s2 of
      Just (b, s2) -> Just (Sin b, s2)
      _                -> Nothing
factor ('c':'o':'s':s) =
   case factor s of
      Just (b, s) -> Just (Cos b, s)
      _                -> Nothing
factor ('x':s2) = Just (Var "x", s2)
factor s = num s

-- takes a string and sends it to expr who then evaluates the data and produces an expr
readExpr :: String -> Maybe Expr
readExpr s =
  case expr (filterSpaces s) of
    Just (a,"") -> Just a
    _           -> Nothing


filterSpaces :: String -> String
filterSpaces s = filter (/= ' ') s

