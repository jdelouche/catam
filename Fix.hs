{-# LANGUAGE DeriveFunctor #-}
import Prelude
import Data.Typeable
data Fix f = Fix (f (Fix f))
unFix (Fix f) = f
ana coalg = Fix . (fmap (ana coalg)) . coalg
cata alg  = alg . (fmap (cata  alg)) . unFix
data StreamF e a = NilF | StreamF e a deriving (Functor,Show)
coalg :: [Int]->StreamF Int [Int]
coalg []          =  NilF
coalg (p : ns)    =  StreamF p ns
alg   :: StreamF Int [Int]->[Int]
alg NilF = []
alg (StreamF e a)  = e:a
main      = do 
               print $ typeOf $ (ana coalg)
               print $ typeOf $ (cata  alg)
               print $ ((cata alg) . (ana coalg)) [1..10]
