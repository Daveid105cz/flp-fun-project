-- Projekt: flp22-fun
-- Autor: David Podeszwa (xpodes05)
-- Rok: 2023
module CurveMath
    ( multscalar, inverseMod, add
    ) where
import Types

inf :: Point
inf = Point 0 0

isInf :: Point -> Bool
isInf (Point 0 0) = True
isInf (Point _ _) = False


neg :: Point -> Integer -> Point
neg (Point x y) p = Point x ( mod (-y) p)

isNegation :: Point -> Point -> Integer -> Bool
isNegation p q curveP = p == neg q curveP

eucl :: Integer -> Integer -> Integer -> Integer ->Integer -> Integer -> (Integer, Integer, Integer)
eucl r t s oldR oldT oldS 
    | r == 0 = (oldR, oldT, oldS)
    | otherwise = eucl newR newT newS r t s where
        q = div oldR r
        newR = oldR - q*r
        newT = oldT - q*t
        newS = oldS - q*s
inverseMod :: Integer -> Integer -> Integer
inverseMod k p
    | k < 0 = p - inverseMod (-k) p
    | otherwise = mod x p where (gcd, y, x) = eucl p 1 0 k 0 1 


add' :: Point -> Point -> Integer -> Integer -> Point
add' (Point x1 y1) (Point x2 y2) a p = Point (mod x3 p) (mod y3 p) where
    x3 = m * m - x1 - x2
    y3 = m * (x1 - x3) - y1
    m = if x1 == x2
        then mod ((3 * x1 * x1 + a) * inverseMod (2 * y1) p) p
        else mod ((y1 - y2) * inverseMod (x1 - x2) p) p


add :: Point -> Point -> Integer -> Integer -> Point
add p@(Point x1 y1) q@(Point x2 y2) curveA curveP
    | isInf p = q
    | isInf q = p
    | (x1 == x2) && (y1 /= y2) = inf
    | otherwise = add' p q curveA curveP


multscalar' :: Point -> Integer -> Integer -> Integer -> Point
multscalar' point k curveA curveP
    | k == 0 = inf
    | k == 1 = point
    | mod k 2 == 1 = add point (multscalar' point (k-1) curveA curveP) curveA curveP
    | otherwise = multscalar' (add point point curveA curveP) (div k 2) curveA curveP

multscalar :: Point -> Integer -> Integer -> Integer -> Point
multscalar point k curveA curveP
    | mod k curveP == 0 = inf
    | isInf point = inf
    | k < 0 = multscalar (neg point curveP) (-k) curveA curveP
    | otherwise = multscalar' point k curveA curveP
