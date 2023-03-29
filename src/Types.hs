-- Projekt: flp22-fun
-- Autor: David Podeszwa (xpodes05)
-- Rok: 2023
module Types
    ( 
        Point(..),
        Curve(..),
        PrivateKey(..),
        PublicKey(..), 
        Keys(..),
        SigningInfo(..),
        Signature(..),
        VerifyInfo(..),
        hexaShow
    ) where
import Text.Printf

data Point = Point { 
    x :: Integer, 
    y :: Integer 
} deriving Eq

data Curve = Curve { 
    p :: Integer, 
    a :: Integer, 
    b :: Integer, 
    g :: Point, 
    n :: Integer, 
    h :: Integer 
}

hexaShow :: Integer-> String
hexaShow = printf "0x%064X"

instance Show Point where
  show (Point x y) = "Point {\nx: "++hexaShow x ++ "\ny: "++hexaShow y ++ "\n}"

instance Show Curve where
  show (Curve p a b g n h) = 
    "Curve {\np: " ++ hexaShow p ++ 
    "\na: " ++ show a ++
    "\nb: " ++ show b ++
    "\ng: " ++ show g ++
    "\nn: " ++ hexaShow n ++
    "\nh: " ++ show h ++
    "\n}"



newtype PrivateKey = PrivateKey Integer

data PublicKey = PublicKey{
    px :: Integer,
    py :: Integer
}

data Keys = Keys{
    private :: PrivateKey,
    public :: PublicKey
}

instance Show PrivateKey where
    show (PrivateKey value) = printf "0x%064X" value

instance Show PublicKey where
    show (PublicKey x y) = printf "0x04%064X%064X" x y

instance Show Keys where
  show (Keys private public) = "Key {\nd: "++show private ++ "\nQ: "++ show public ++ "\n}"


data SigningInfo = SigningInfo{
    siCurve :: Curve,
    siKey :: Keys,
    siHash :: Integer
} deriving Show

data Signature = Signature{
    r :: Integer,
    s :: Integer
}

instance Show Signature where
    show (Signature r s) = "Signature {\nr: "++hexaShow r ++ "\ns: "++hexaShow s ++ "\n}"

data VerifyInfo = VerifyInfo{
    vCurve :: Curve,
    vSignature :: Signature,
    vPublicKey :: PublicKey,
    vHash :: Integer
} deriving Show
