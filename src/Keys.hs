module Keys
    ( PrivateKey
    ) where

data PrivateKey = PrivateKey { 
    value :: Integer,
    theN :: Int
} deriving Show

data PublicKey = PublicKey{
    x :: Integer,
    y :: Integer
} deriving Show

data Key = Key{
    private :: PrivateKey,
    public :: PublicKey
}

instance Show Key where
  show (Key private public) = "Key {\nd: "++show private ++ "\nQ: "++ show public ++ "\n}"
