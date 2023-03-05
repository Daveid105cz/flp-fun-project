module Keys
    ( PrivateKey(..), PublicKey(..), Key(..)
    ) where
import Text.Printf

newtype PrivateKey = PrivateKey Integer


data PublicKey = PublicKey{
    x :: Integer,
    y :: Integer
}

data Key = Key{
    private :: PrivateKey,
    public :: PublicKey
}

instance Show PrivateKey where
    show (PrivateKey value) = printf "0x%064X" value

instance Show PublicKey where
    show (PublicKey x y) = printf "0x04%064X%064X" x y

instance Show Key where
  show (Key private public) = "Key {\nd: "++show private ++ "\nQ: "++ show public ++ "\n}"
