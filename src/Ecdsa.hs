module Ecdsa 
    (
        generatePrivateKey,
        calculatePublicKey,
        sign,
        verify
    )where

import System.Random
import Types
import CurveMath


generatePrivateKey :: StdGen -> Integer -> PrivateKey
generatePrivateKey genSeed maxN = let (theVal, _) = randomR (1,maxN-1) genSeed in PrivateKey theVal

calculatePublicKey :: Curve -> PrivateKey -> PublicKey
calculatePublicKey curve@(Curve p a b g n h) (PrivateKey pK) = PublicKey pX pY where
    (Point pX pY) = multscalar g pK a p 

sign :: StdGen -> Curve -> Keys -> Integer -> Signature
sign generator curve@(Curve p a b g n h) keys@(Keys (PrivateKey pK) (PublicKey xP yP)) hash =
    if r==0 || s==0 
        then sign newGen curve keys hash
        else Signature r s
    where
        (k, newGen) = randomR (1,n-1) generator
        kPoint = multscalar g k a p
        r = mod (x kPoint) n
        s = mod ((hash + r * pK) * inverseMod k n) n


verify :: Curve -> PublicKey -> Signature -> Integer -> Bool
verify curve@(Curve p a b g n h) public@(PublicKey px py) signature@(Signature r s) hash
    | (r < 1 || r > (n-1)) || (s < 1 || s > (n-1)) = False
    | otherwise = mod r n == mod (x point) n where
        w = inverseMod s n
        u1 = mod (hash*w) n
        u2 = mod (r*w) n
        scalar1 = multscalar g u1 a p
        scalar2 = multscalar (Point px py) u2 a p
        point = add scalar1 scalar2 a p

        -- point = add 
