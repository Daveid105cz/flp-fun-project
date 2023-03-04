module Keys
    ( PrivateKey
    ) where

data PrivateKey = PrivateKey { 
    value :: Integer,
    theN :: Int
}

data PublicKey = PublicKey{
    x :: Integer,
    y :: Integer
}

data Key = Key{
    private :: PrivateKey,
    public :: PublicKey
}