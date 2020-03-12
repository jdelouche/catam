{-# LANGUAGE DeriveFunctor #-}
import Prelude
import Data.Typeable
data Fix f = Fx (f (Fix f))
unFix (Fx f) = f
ana :: ([Int]->StreamF Int [Int]) -> ([Int] -> Fix (StreamF Int))
ana coalg = Fx . (fmap (ana coalg)) . coalg
cata :: ((StreamF Int [Int])->[Int]) -> (Fix (StreamF Int) -> [Int])
cata alg  = alg . (fmap (cata  alg)) . unFix
data StreamF e a = NilF | StreamF e a deriving (Functor,Show)
coalg :: [Int]->StreamF Int [Int]
coalg []          =  NilF
coalg (p : ns)    =  StreamF p ns
alg   :: StreamF Int [Int]->[Int]
alg NilF = []
alg (StreamF e a)  = e:a
main      = do let a = [1..10]
               putStr          "                                                                    a : "
               print                                                                                a
               putStr          "                                                                coalg : "
               print $ typeOf $                                                                 coalg
               putStr          "                                           (fmap (ana coalg)) . coalg : "
               print $ typeOf $                                            (fmap (ana coalg)) . coalg
               putStr          "                                      Fx . (fmap (ana coalg)) . coalg : "
               print $ typeOf $                                       Fx . (fmap (ana coalg)) . coalg
               putStr          "                              unFix . Fx . (fmap (ana coalg)) . coalg : "
               print $ typeOf $                               unFix . Fx . (fmap (ana coalg)) . coalg
               putStr          "          (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg : "
               print $ typeOf $           (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg
               putStr          "    alg . (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg : "
               print $ typeOf $     alg . (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg
               putStr          "                                             (cata alg) . (ana coalg) : "
               print $ typeOf $                                              (cata alg) . (ana coalg)
               putStr          "                                         ((cata alg) . (ana coalg)) a : "
               print $                                                   ((cata alg) . (ana coalg)) a
               putStr          "(alg . (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg) a : "
               print $          (alg . (fmap (cata alg)) . unFix . Fx . (fmap (ana coalg)) . coalg) a

