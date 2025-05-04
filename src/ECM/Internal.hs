{-# LANGUAGE MultiWayIf #-}
module ECM.Internal where

import System.Random (randomRIO)

data ECPoint = Inf | ECPoint
    { a :: Integer
    , b :: Integer
    , n :: Integer
    , x :: Integer
    , y :: Integer
    } deriving (Show)

bezoutCoeff :: Integer -> Integer -> (Integer, Integer, Integer)
bezoutCoeff a 0 = (a, 1, 0)
bezoutCoeff a b = (abs g, t, s - q * t)
  where
    (q, r) = a `divMod` b
    (g, s, t) = bezoutCoeff b r

modInv :: Integer -> Integer -> Maybe Integer
modInv a n
    | g /= 1 = Nothing
    | otherwise = if s < 0 
                  then Just (s + n)
                  else Just s
  where
    (g, s, _) = bezoutCoeff a n

addPoints :: ECPoint -> ECPoint -> Either ECPoint Integer
addPoints Inf q = Left q
addPoints p Inf = Left p
addPoints p@ECPoint { a, n, x = x1, y = y1 } ECPoint { x = x2, y = y2 }
    | x1 == x2 && y1 /= y2 = Left Inf
    | x1 == x2 && y1 == y2 && y1 == 0 = Left Inf
    | x1 == x2 && y1 == y2 = do 
        let res = modInv (2 * yp) n

        case res of
            Nothing -> Right (2 * yp)
            Just inv -> do 
                let s = ((3 * xp * xp + a) * inv) `mod` n
                    x3 = (s * s - 2 * xp) `mod` n
                    y3 = (s * (xp - x3) - yp) `mod` n

                Left p { x = x3, y = y3 }
    | otherwise = do
        let res = modInv (xr - xp) n

        case res of
            Nothing -> Right (xr - xp)
            Just inv -> do
                let s = ((yr - yp) * inv) `mod` n
                    x3 = (s * s - xp - xr) `mod` n
                    y3 = (s * (xp - x3) - yp) `mod` n

                Left p { x = x3, y = y3 }
  where
    (xp, yp) = if x2 < x1 then (x2, y2) else (x1, y1)
    (xr, yr) = if x2 < x1 then (x1, y1) else (x2, y2)

mulPoint :: Integer -> ECPoint -> Either ECPoint Integer
mulPoint 0 _ = Left Inf
mulPoint 1 p = Left p
mulPoint k p
    | odd k = do 
        let res = mulPoint (k - 1) p

        case res of
            Left p' -> addPoints p p'
            Right n -> Right n
    | otherwise = do
        let res = addPoints p p

        case res of
            Left p' -> mulPoint (k `div` 2) p'
            Right n -> Right n

genCurve :: Integer -> IO (Either ECPoint Integer)
genCurve n = do
    x <- randomRIO (1, n)
    y <- randomRIO (1, n)
    a <- randomRIO (1, n)

    let b = (y * y - x * x * x - a * x) `mod` n
    let g = gcd (4 * a * a * a + 27 * b * b) n 

    if | g == 1 -> return $ Left (ECPoint a b n x y)
       | g == n -> genCurve n
       | otherwise -> return $ Right g
      
lcm' :: [Integer] -> Integer
lcm' [n1, n2] = lcm n1 n2
lcm' (n : ns) = lcm n (lcm' ns)
lcm' _ = 0
