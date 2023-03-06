module Keys
    ( PrivateKey(..), PublicKey(..), Keys(..)
    ) where
import Text.Printf

newtype PrivateKey = PrivateKey Integer


data PublicKey = PublicKey{
    x :: Integer,
    y :: Integer
}

data Keys = Keys{
    private :: PrivateKey,
    public :: PublicKey
}

instance Show PrivateKey where
    show (PrivateKey value) = printf "0x%064X" value

instance Show PublicKey where
    show (PublicKey x y) = printf "0x04%064X%064X" x y --TODO: remove the strednik

instance Show Keys where
  show (Keys private public) = "Key {\nd: "++show private ++ "\nQ: "++ show public ++ "\n}"
