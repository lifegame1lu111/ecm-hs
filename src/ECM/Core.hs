module ECM.Core where

import ECM.Internal

getFactor :: Integer -> Integer -> IO Integer
getFactor n b = do
    p <- genCurve n

    case p of
        Left p' -> do
            let res = mulPoint (lcm' [1..b]) p'

            case res of
                Left q -> 
                    case q of
                        Inf -> return n
                        _ -> getFactor n b
                Right num -> return $ gcd num n
        Right num -> return $ gcd num n

factor :: Integer -> Integer -> IO [Integer]
factor n b
    | even n = (2 :) <$> factor (n `div` 2) b
    | n `mod` 3 == 0 = (3 :) <$> factor (n `div` 3) b
    | otherwise = do
        num <- getFactor n b

        if num == n
        then return [n]
        else (num :) <$> factor (n `div` num) b
