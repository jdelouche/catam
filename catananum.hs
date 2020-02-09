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
ouput   ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
ouput   = cata send
input   = ana receive
send   (StreamF Nothing   (Right p))  = (Right p)
send   (StreamF (Just i)  (Right p))  = (Right (i:p))
send   (NilF)                         = (Right [])
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
receive (Left [])           = NilF
receive (Left ('e':'1':ns)) = StreamF (Just 48) (Left ns)
receive (Left ('e':'2':ns)) = StreamF (Just 49) (Left ns)
receive (Left (p : ns))     = StreamF (Nothing) (Left ns)
type Receiver   = [Int]
type Sender     = [Char]
type Carrier    = Either Sender Receiver
type Transfer   = Maybe Int
type ConnectorF = StreamF Transfer
type InterfaceF = ConnectorF Carrier
main = do print $ (ouput . input) (Left "h1 e2")
