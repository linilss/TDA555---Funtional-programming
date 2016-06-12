module ExprQC where

import Expr
import Test.QuickCheck

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = readExpr (showExpr e) == Just (assoc e)

arbitraryExpr :: Int -> Gen Expr
arbitraryExpr s =
  frequency [ (1, do n <- arbitrary
                     return (Num n))
            , (s, do a <- arbitraryExpr s'
                     b <- arbitraryExpr s'
                     return (Add a b))
            , (s, do a <- arbitraryExpr s'
                     b <- arbitraryExpr s'
                     return (Mul a b))
            , (s, do a <- arbitraryExpr s'
                     return (Sin a))
            , (s, do a <- arbitraryExpr s'
                     return (Cos a))
            ]
 where
  s' = s `div` 2

instance Arbitrary Expr where
    arbitrary = sized arbitraryExpr

assoc :: Expr -> Expr
assoc (Add (Add a b) c) = assoc (Add a (Add b c))
assoc (Add a b)         = Add (assoc a) (assoc b)
assoc (Mul (Mul a b) c) = assoc (Mul a (Mul b c))
assoc (Mul a b)         = Mul (assoc a) (assoc b)
assoc (Sin a)           = Sin (assoc a)
assoc (Cos a)           = Cos (assoc a)
assoc a                 = a
