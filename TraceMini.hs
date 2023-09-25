{-# LANGUAGE DeriveFunctor #-}
module Main where
import Prelude
import Data.Typeable
import Debug.Trace

data Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = trace ("unFix") x

data ExprF r = Const Int
             | Add r r
             | Mul r r
    deriving (Functor,Show)
calc :: ExprF Int -> Int
calc (Const i)   = trace ("calc: "++(show i)) i
calc (x `Add` y) = trace ("calc: "++(show x)++" Add "++(show y)) (x + y)
calc (x `Mul` y) = trace ("calc: "++(show x)++" Mul "++(show y)) (x * y)
evalcalc :: Fix ExprF -> Int
evalcalc = cata calc
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
testExpr = Fx $ 
             Fx ( 
              (Fx $ 
                 (Fx $ Const 2)
                 `Add`
                 (Fx $ Const 5))
              `Mul` 
              (Fx $ Const 6))
             `Add`
             Fx ( 
                (Fx $ 
                   (Fx $ Const 2)
                   `Add`
                   (Fx $ Const 4))
                `Mul` 
                (Fx $ Const 7))
un = Fx $ Const 1

main :: IO ()
main = do
  print $ evalcalc $ un
  print $ evalcalc $ testExpr
