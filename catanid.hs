{-# LANGUAGE DeriveFunctor #-}
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
data StreamF e a = NilF | StreamF e a deriving (Functor,Show)
al :: StreamF e [e] -> [e]
al (StreamF e a)  = e:a
al (NilF)     = []
toListC :: Fix (StreamF e) -> [e]
toListC = cata al
notdiv p n = n `mod` p /= 0
erat :: [Char] -> StreamF Char [Char]
erat [] = NilF
erat ('o': ns) = StreamF 'x'  ns
erat (p : ns) = StreamF  p ns
evalps :: [Char] -> Fix (StreamF Char)
evalps = ana erat
main = do print $ (toListC . evalps) "Helloooooooooo"
