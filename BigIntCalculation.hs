-- Dilara Aydin
-- COP4020 - Summer, 2017 - Final Project

import System.Random
import Prelude hiding (odd)
import Control.Monad (join)

type BigInt = String

-- Takes in an Int as the number of digits and returns a BigInt of that length.
randomNumber :: Int -> BigInt
randomNumber a = take a $ randomRs ('0','9') (mkStdGen 3) :: [Char]

halve :: Integer -> Integer
halve = (`div` 2)

double :: Integer -> Integer
double = join (+)

odd :: Integer -> Bool
odd = (== 1) . (`mod` 2)

logFloor :: Integer->Integer->Int
logFloor b x
  = if x < b then
      0
    else
      let
        l = 2 * logFloor (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
        doDiv (x`div`(b^l)) l

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

classicalMultiplication :: BigInt -> BigInt -> Int -> BigInt
classicalMultiplication a b base =
  show $ sum $ map snd $ filter (odd . fst) $
  zip (takeWhile (>= 1) $ iterate halve a') (iterate double b')

  where
    a' = read a :: Integer
    b' = read b :: Integer

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

toomMultiplication :: BigInt -> BigInt -> Integer -> BigInt
toomMultiplication a b base
  | len < 10 = show (a' * b')
  | otherwise = show (a')

  where
    len = (max (length a) (length b))
    i = max (div (logFloor base a') 3) (div (logFloor base b') 3) + 1
    a' = read a :: Integer
    b' = read b :: Integer
    a2 = 0
    a1 = 0
    a0 = 0
    b2 = 0
    b1 = 0
    b0 = 0
