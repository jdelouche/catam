{-# LANGUAGE DeriveFunctor #-}
-- https://bartoszmilewski.com/2017/02/28/f-algebras/
import Prelude
data Fix f = Fix (f (Fix f))
instance Show (Fix f) where
  show (Fix x)= "."
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg) . unFix;
ana  :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
data StreamF e a = StreamF e a deriving (Functor,Show)
al :: StreamF e [e] -> [e]
al (StreamF e a) = e : a
toListC :: Fix (StreamF e) -> [e]
toListC = cata al
notdiv p n = n `mod` p /= 0
erat :: [Int] -> StreamF Int [Int]
erat (p : ns) = StreamF p (filter (notdiv p) ns)
evalps :: [Int] -> Fix (StreamF Int)
evalps = ana erat
main = do print $ (toListC . evalps) [2..]
