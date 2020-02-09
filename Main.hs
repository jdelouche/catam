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

data NatF a = ZeroF | SuccF a deriving Functor
fib :: NatF (Int, Int) -> (Int, Int)
fib ZeroF = (1, 1)
fib (SuccF (m, n)) = (n, m + n)
evalfib :: Fix NatF -> (Int,Int)
evalfib = cata fib
d = Fx $ SuccF $ Fx $ SuccF $ Fx $ SuccF $ Fx ZeroF
d' n = foldr (\_ z -> Fx $ SuccF z) (Fx ZeroF) [1..n]
len :: NatF Int -> Int
len ZeroF = 1
len (SuccF n) = n+1

data ListF e a = NilF | ConsF e a deriving Functor
lenAlg :: ListF e Int -> Int
lenAlg (ConsF e n) = n + 1
lenAlg NilF = 0

evalLen :: Fix (ListF e) -> Int
evalLen = cata lenAlg

l :: Foldable t => t e -> Fix (ListF e)
l s = foldr (\e a -> Fx $ ConsF e a) (Fx NilF) s

data StreamF e a = StreamF e a
  deriving (Functor,Show)

era :: [Int] -> StreamF Int [Int]
era (p : ns) = StreamF p (filter (notdiv p) ns)
    where notdiv p n = n `mod` p /= 0

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fx . fmap (ana coalg) . coalg

data F a b = Nil | Cons a b deriving (Eq, Show)
type T a = [a]

instance Functor (F a) where
  fmap _ Nil = Nil
  fmap f (Cons n a) = Cons n (f a)

mu f = x where x = f x
hylo :: (F a b2 -> b2) -> (b1 -> F a b1) -> b1 -> b2
hylo phi psi = mu (\x -> phi . fmap x . psi)
phi :: F Integer Integer -> Integer
phi Nil = 1
phi (Cons n m) = n*m

psi :: Integer -> F Integer Integer
psi 0 = Nil
psi n = Cons n (n-1)

factorial = hylo phi psi

cata' :: (F a b -> b) -> T a -> b
cata' phi = mu (\x -> phi . fmap x . outT)
outT :: T a -> F a (T a)
outT [] = Nil
outT (a:as) = Cons a as

lphi :: F a Int -> Int
lphi Nil = 0
lphi (Cons a n) = n + 1

l' = cata' lphi

ana' :: (b -> F a b) -> b -> T a
ana' psi = mu (\x -> inT . fmap x . psi)
inT :: F a (T a) -> T a
inT Nil = []
inT (Cons a as) = a:as

fpsi :: Int -> F Int Int
fpsi 0 = Nil
fpsi n = Cons n (n-1)

g = ana' fpsi

main :: IO ()
main = do
  print (factorial 10)
  print $ (cata len) $ d
  print $ (cata len) $ d' 98
  print $ evalcalc $ testExpr
  print $ evalfib $ d' 100
  print $ evalLen $ Fx $ ConsF "a" $ Fx $ ConsF "b" $ Fx $ NilF
  print $ evalLen $ l ("abc"::[Char])
  let x = ana era [2..]
  print $ typeOf (unFix x)
