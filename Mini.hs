{-# LANGUAGE DeriveFunctor #-}
module Main where
import Prelude
import Data.Typeable

data Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

data ExprF r = Const Int
             | Add r r
             | Mul r r
    deriving Functor
calc :: ExprF Int -> Int
calc (Const i)   = i
calc (x `Add` y) = x + y
calc (x `Mul` y) = x * y
evalcalc :: Fix ExprF -> Int
evalcalc = cata calc
testExpr = Fx $ 
              (Fx $ (Fx $ Const 2) `Add` (Fx $ Const 5)) `Mul` 
                           (Fx $ Const 6)

un = Fx $ Const 1

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

main :: IO ()
main = do
  print $ evalcalc $ un
