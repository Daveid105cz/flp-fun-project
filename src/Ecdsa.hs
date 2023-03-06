module Ecdsa 
    (
        generatePrivateKey,
        calculatePublicKey,
        sign
    )where

import System.Random
import Types
import CurveMath


generatePrivateKey :: StdGen -> Integer -> PrivateKey
generatePrivateKey genSeed maxN = let (theVal, _) = randomR (1,maxN-1) genSeed in PrivateKey theVal

calculatePublicKey :: Curve -> PrivateKey -> PublicKey
calculatePublicKey curve@(Curve p a b g n h) (PrivateKey pK) = PublicKey pX pY where
    (Point pX pY) = multscalar g pK a p 

sign :: StdGen -> Curve -> Keys -> Integer -> (Signature, Integer)
sign generator curve@(Curve p a b g n h) keys@(Keys (PrivateKey pK) (PublicKey xP yP)) hash =
    if r==0 || s==0 
        then sign newGen curve keys hash
        else (Signature r s, k)
        where
        (k, newGen) = randomR (1,n-1) generator
        kPoint = multscalar g k a p
        r = mod (x kPoint) n
        s = mod ((hash + r * pK) * inverseMod k n) n

