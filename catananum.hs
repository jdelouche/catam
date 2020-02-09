{-# LANGUAGE DeriveFunctor #-}
import Prelude
import Data.Typeable
data Fix f = Fix (f (Fix f))
instance Show (Fix f) where show (Fix x)= "."
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg) . unFix;
ana  :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
data StreamF e a = NilF | StreamF e a deriving (Functor,Show)
deconstruction ::            Fix (ConnectorF) -> Carrier
construction   :: Carrier -> Fix (ConnectorF)
deconstruction = cata send
construction   = ana receive
send (StreamF (e,Nothing) (x,p)) = ([],p)
send (StreamF (e,Just i)  (x,p)) = ([],i:p)
send (NilF)                      = ([],[])
send ::               InterfaceF -> Carrier
receive :: Carrier -> InterfaceF
receive ([],_)  = NilF
receive (('e':'1':ns),a) = StreamF ('x',Just 48) (ns,[])
receive (('e':'2':ns),a) = StreamF ('x',Just 49) (ns,[])
receive ((p : ns),a)     = StreamF ('_',Nothing) (ns,[])
type Carrier    = ([Char],[Int])
type Transfer   = (Char,Maybe Int)
type ConnectorF = StreamF Transfer
type InterfaceF = ConnectorF Carrier
main = do print $ (deconstruction . construction) ("e1 e2",[])
