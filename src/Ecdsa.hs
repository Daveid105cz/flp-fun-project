module Ecdsa 
    (
        generatePrivateKey,
        calculatePublicKey,
        SigningInfo(..)
    )where

import System.Random
import Keys (PublicKey(PublicKey), PrivateKey (PrivateKey), Keys)
import Curves

data SigningInfo = SigningInfo{
    curve :: Curve,
    key :: Keys,
    hash :: Integer
} deriving Show


generatePrivateKey :: StdGen -> Integer -> PrivateKey
generatePrivateKey genSeed maxN = let (theVal, _) = randomR (1,maxN-1) genSeed in PrivateKey theVal

calculatePublicKey :: Curve -> PrivateKey -> PublicKey
calculatePublicKey curve@(Curve p a b g n h) (PrivateKey pK) = PublicKey pX pY where
    (Point pX pY) = multscalar g pK a p 



