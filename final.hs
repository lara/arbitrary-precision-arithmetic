type BigInt = String

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


-- toomMultiplication :: BigInt -> BigInt -> Int -> BigInt

-- compare :: BigInt -> BigInt -> Int -> Bool

-- addToBigInt :: BigInt -> Int -> BigInt







-- S --> bbCa
-- C --> bbCA | cX
-- A --> XaX
-- X --> cC | e
