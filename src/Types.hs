module Types
    ( Switch(..), Curve(..), Point(..)
    ) where
import Numeric (showHex)
import Text.Printf


data Switch = Info | KeyGen | Sign | Verify deriving (Enum, Show)

data Point = Point { 
    x :: Integer, 
    y :: Integer 
}

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
