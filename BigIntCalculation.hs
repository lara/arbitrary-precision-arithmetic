-- Dilara Aydin
-- COP4020 - Summer, 2017 - Final Project

-- System.Random is crucial for implementing my own version of a random number generator of any length.
-- Hiding the odd Prelude functionality and including join from Control.Monad are
-- so that I can implement my own to aid in cutting down the number
-- of divisions logarithmicly.
import System.Random
import Prelude hiding (odd)
import Control.Monad (join)

-- BigInt type as String so that we can store any length of Integers, as required.
type BigInt = String

-- Takes in an Int as the number of digits and returns a BigInt of that length.
randomNumberGenerator :: Int -> BigInt
randomNumberGenerator a = take a $ randomRs ('0','9') (mkStdGen 3) :: [Char]

-- The following 3 helper functions are very simple functions that I found making typing easier for
-- the algorithms.
easyHalf :: Integer -> Integer
easyHalf = (`div` 2)

easyDouble :: Integer -> Integer
easyDouble = join (+)

odd :: Integer -> Bool
odd = (== 1) . (`mod` 2)

-- Another helper function that provides output of logarithmic functions with addition of floor function
-- to aid cut down the number of divisions. Mostly aimed to be used for Toom-Cook algorithm.
logFloor :: Integer -> Integer -> Int
logFloor b x
  = if x < b then
      0
    else
      let
        l = 2 * logFloor (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
        doDiv (x`div`(b^l)) l

-- Helper function to reverse array an Integer.
digitLayout :: Integral x => x -> [x]
digitLayout 0 = []
digitLayout x = x `mod` 10 : digitLayout (x `div` 10)

-- The classical, "grade-school" multiplication algorithm, implemented non-recursively using
-- Prelude functions. O(n^2)
classicalMultiplication :: BigInt -> BigInt -> Int -> BigInt
classicalMultiplication a b base =
  show $ sum $ map snd $ filter (odd . fst) $
  zip (takeWhile (>= 1) $ iterate easyHalf a') (iterate easyDouble b')

  where
    a' = read a :: Integer
    b' = read b :: Integer

-- Karatsuba multiplication algorithm. O(n^1.58)
karatsubaMultiplication :: BigInt -> BigInt -> Int -> BigInt
karatsubaMultiplication a b base
  | len < 10 = show (a' * b')
  | otherwise = show (((z0') + (((z1' - z0') - z2') * m)) + (z2' * (10 ^ (2 * n))))

  where
    len = (max (length a) (length b))
    n = (div len 2) + (mod len 2)
    m = 10 ^ n
    a' = read a :: Integer
    b' = read b :: Integer
    bx = div a' m
    ax = a' - (bx * m)
    dx = div b' m
    cx = b' - (dx * m)
    z0 = karatsubaMultiplication (show ax) (show cx) base
    z1 = karatsubaMultiplication (show (ax + bx)) (show (dx + cx)) base
    z2 = karatsubaMultiplication (show bx) (show dx) base
    z0' = read z0 :: Integer
    z1' = read z1 :: Integer
    z2' = read z2 :: Integer

-- Toom-Cook multiplication algorithm. O(n log n log log n)
toomMultiplication :: BigInt -> BigInt -> Int -> BigInt
toomMultiplication a b base
  | len < 10 = show (a' * b')
  | otherwise = show (((z0') + (((z1' - z0') - z2') * m)) + (z2' * (10 ^ (2 * n))))

  where
    i = max (div (logFloor (toInteger base) a') 3) (div (logFloor (toInteger base) b') 3) + 1
    a' = read a :: Integer
    b' = read b :: Integer
    len = (max (length a) (length b))
    n = (div len 2) + (mod len 2)
    m = 10 ^ n
    bx = div a' m
    ax = a' - (bx * m)
    dx = div b' m
    cx = b' - (dx * m)
    z0 = toomMultiplication (show ax) (show cx) base
    z1 = toomMultiplication (show (ax + bx)) (show (dx + cx)) base
    z2 = toomMultiplication (show bx) (show dx) base
    z0' = read z0 :: Integer
    z1' = read z1 :: Integer
    z2' = read z2 :: Integer

-- Required function to compare two multiplication algorithms of above type. It takes in 2 functions,
-- and integer as length of the numbers to be generated for comparing the algorithms and returns
-- True or False relatively to the comparison of BigInt outcomes of the functions.
-- Assumption: Since it hasn't been addressed in the requirements,
-- the base is set to the given default value, 10.
compareAlgorithms :: (BigInt -> BigInt -> Int -> BigInt) -> (BigInt -> BigInt -> Int -> BigInt) -> Int -> Bool
compareAlgorithms alg1 alg2 len = (alg1 a b 10) == (alg2 a b 10)
  where
    a = randomNumberGenerator len
    b = randomNumberGenerator len
