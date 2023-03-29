-- Projekt: flp22-fun
-- Autor: David Podeszwa (xpodes05)
-- Rok: 2023
module Ecdsa 
    (
        generatePrivateKey,
        calculatePublicKey,
        sign,
        verify
    )where

import System.Random ( StdGen, Random(randomR) )
import Types
    ( Curve(Curve),
      Keys(Keys),
      Point(Point, x),
      PrivateKey(..),
      PublicKey(PublicKey),
      Signature(Signature) )
import CurveMath ( add, inverseMod, multscalar )

-- | Generates a private key given a generator and a maximum value
generatePrivateKey :: StdGen -> Integer -> PrivateKey
generatePrivateKey genSeed maxN = let (theVal, _) = randomR (1,maxN-1) genSeed in PrivateKey theVal

-- | Calculates a public key given a curve and a already existing private key
calculatePublicKey :: Curve -> PrivateKey -> PublicKey
calculatePublicKey (Curve p a _ g _ _) (PrivateKey pK) = PublicKey pX pY where
    (Point pX pY) = multscalar g pK a p 

-- | Signs a message hash given a curve, the private key and a generator
-- The generator is used to generate a random number k used in the signature calculation
sign :: StdGen -> Curve -> Keys -> Integer -> Signature
sign generator curve@(Curve p a _ g n _) keys@(Keys (PrivateKey pK) _) hash =
    if r==0 || s==0 
        then sign newGen curve keys hash
        else Signature r s
    where
        (k, newGen) = randomR (1,n-1) generator
        kPoint = multscalar g k a p
        r = mod (x kPoint) n
        s = mod ((hash + r * pK) * inverseMod k n) n

-- | Verifies a signature given a curve, public key and a hash of the message
-- Returns True if the signature is valid, False otherwise
-- Signature is valid if the hash was signed with the private key corresponding to the public key
verify :: Curve -> PublicKey -> Signature -> Integer -> Bool
verify (Curve p a _ g n _) (PublicKey px py) (Signature r s) hash
    | (r < 1 || r > (n-1)) || (s < 1 || s > (n-1)) = False
    | otherwise = mod r n == mod (x point) n where
        w = inverseMod s n
        u1 = mod (hash*w) n
        u2 = mod (r*w) n
        scalar1 = multscalar g u1 a p
        scalar2 = multscalar (Point px py) u2 a p
        point = add scalar1 scalar2 a p
