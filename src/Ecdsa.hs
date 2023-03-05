module Ecdsa 
    (
        generatePrivateKey,
        calculatePublicKey
    )where

import System.Random
import Keys (PublicKey(PublicKey), PrivateKey (PrivateKey))
import Curves

generatePrivateKey :: StdGen -> Integer -> PrivateKey
generatePrivateKey genSeed maxN = let (theVal, _) = randomR (1,maxN-1) genSeed in PrivateKey theVal

calculatePublicKey :: Curve -> PrivateKey -> PublicKey
calculatePublicKey curve privateKey = PublicKey 0xFFFF5555FFF 0xFFFFFFF



