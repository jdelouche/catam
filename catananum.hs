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
send (StreamF Nothing   (_,Just p))  = (Nothing,Just p)
send (StreamF (Just i)  (_,Just p))  = (Nothing,Just (i:p))
send (NilF)                          = (Nothing,Just [])
send ::               InterfaceF -> Carrier
receive :: Carrier -> InterfaceF
receive (Just [],_)           = NilF
receive (Just ('e':'1':ns),_) = StreamF (Just 48) (Just ns,Nothing)
receive (Just ('e':'2':ns),_) = StreamF (Just 49) (Just ns,Nothing)
receive (Just (p : ns),_)     = StreamF (Nothing) (Just ns,Nothing)
type Carrier    = (Maybe [Char],Maybe [Int])
type Transfer   = Maybe Int
type ConnectorF = StreamF Transfer
type InterfaceF = ConnectorF Carrier
main = do print $ (deconstruction . construction) (Just "e1 e2",Nothing)
