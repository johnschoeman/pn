module PN (pnSucc, pnShow, pnNumbers, toDecimal, baseOf) where

pnNumbers :: [[Integer]]
pnNumbers = [0] : [1] : [ pnSucc a | a <- tail pnNumbers ]

pnSucc :: [Integer] -> [Integer]
pnSucc xs = 
  pnIncrement b (padZeros b xs)
  where
     b = baseOf xs

baseOf :: [Integer] -> Int
baseOf xs =
  let
    (_, pns) = break (> 0) xs
    countPrimes :: Int
    countPrimes = foldr (\_ -> (+) 1) 0 pns
    maxPower :: Int
    maxPower = fromIntegral (maximum (0:pns))
  in
    max countPrimes maxPower

padZeros :: Int -> [Integer] -> [Integer]
padZeros b xs =
  lastN b (replicate l 0 ++ xs)
  where
    l = b - length xs

lastN :: Int -> [Integer] -> [Integer]
lastN n xs =
  drop (length xs - n) xs

pnIncrement :: Int -> [Integer] -> [Integer]
pnIncrement b xs =
    let
      next = addOneInBase b xs
    in
      if baseOf next >= b
        then next
        else pnIncrement b next

addOneInBase :: Int -> [Integer] -> [Integer]
addOneInBase b xs =
  let
    (rollOvers, digits) = break (< toInteger b) (reverse xs)
  in
    case digits of
      [] -> replicate b 0 ++ [toInteger b + 1]
      h:ds -> reverse (map (const 0) rollOvers ++ [h + 1] ++ ds)

---- 

toDecimal :: [Integer] -> Integer
toDecimal pn =
  foldl (\acc (n,p) -> acc * (p ^ n)) 1 withPrimes
  where
    withPrimes = zip (reverse pn) primes

primes :: [Integer]
primes = sieve [2..]
  where
    sieve [] = [2]
    sieve (p:xs) = p : sieve [ x | x <- xs, mod x p > 0 ]

pnShow :: [Integer] -> [Char]
pnShow pn =
  show pn ++ " : " ++ show (toDecimal pn)

-- {P} :: set of primes, 2 to P
-- {N} :: set of powers, 0 to N
-- |P| count of primes
-- |N| count of powers == max N
-- total size : |N|*|P|
--
-- Prime Notation : 
-- [7's place].[5's place].[3's place].[2's place]
-- 1.0.1.3 :: 7^1 * 5^0 * 3^1 * 2^3 == 168
--
-- Base :: max of (max power) and |P

-- pp sets
-- prime,power
-- 1,0 : _._.0 : 1
--
-- 1,1 : _._.1 : 2
--
-- 2,1 : _.1.0 : 3
-- 2,1 : _.1.1 : 6
-- 2/6
--
-- 2,2 : _.0.2 : 4
-- 2,2 : _.1.2 : 12
-- 2,2 : _.2.0 : 9
-- 2,2 : _.2.1 : 18
-- 2,2 : _.2.2 : 36
-- 27/36
--
-- primes - {2,3,5}, powers - {0,1,2}
-- 3,2 : 1.0.0 : 5
-- 3,2 : 1.0.1 : 10
-- 3,2 : 1.0.2 : 20
-- 3,2 : 1.1.0 : 15
-- 3,2 : 1.1.1 : 30
-- 3,2 : 1.1.2 : 60
-- 3,2 : 1.2.0 : 45
-- 3,2 : 1.2.1 : 90
-- 3,2 : 1.2.2 : 180
-- 3,2 : 2.0.0 : 25
-- 3,2 : 2.0.1 : 50
-- 3,2 : 2.0.2 : 100
-- 3,2 : 2.1.0 : 75
-- 3,2 : 2.1.1 : 150
-- 3,2 : 2.1.2 : 300
-- 3,2 : 2.2.0 : 225
-- 3,2 : 2.2.1 : 450
-- 3,2 : 2.2.2 : 900
-- |p|^n/(p^n)
--
-- 3,3 : 0.0.3 : 8
-- 3,3 : 0.1.3 : 24
-- 3,3 : 0.2.3 : 72
-- 3,3 : 0.3.0 : 27
-- 3,3 : 0.3.1 : 54
-- 3,3 : 0.3.2 : 108
-- 3,3 : 0.3.3 : 216
-- 3,3 : 1.0.3 : 40
-- 3,3 : 1.1.3 : 120
-- 3,3 : 1.2.3 : 360
-- 3,3 : 1.3.0 : 135
-- 3,3 : 1.3.1 : 270
-- 3,3 : 1.3.2 : 540
-- 3,3 : 1.3.3 : 1080
-- 3,3 : 2.0.3 : 200
-- 3,3 : 2.1.3 : 600
-- 3,3 : 2.2.3 : 1800
-- 3,3 : 2.3.0 : 675
-- 3,3 : 2.3.1 : 1350
-- 3,3 : 2.3.2 : 2700
-- 3,3 : 2.3.3 : 5400
-- 3,3 : 3.0.0 : 125
-- 3,3 : 3.0.1 : 250
-- 3,3 : 3.0.2 : 500
-- 3,3 : 3.0.3 : 1000
-- 3,3 : 3.1.0 : 375
-- 3,3 : 3.1.1 : 750
-- 3,3 : 3.1.2 : 1500
-- 3,3 : 3.1.3 : 3000
-- 3,3 : 3.2.0 : 1125
-- 3,3 : 3.2.1 : 2250
-- 3,3 : 3.2.2 : 4500
-- 3,3 : 3.2.3 : 9000
-- 3,3 : 3.3.0 : 3375
-- 3,3 : 3.3.1 : 6750
-- 3,3 : 3.3.2 : 13500
-- 3,3 : 3.3.3 : 27000
--
-- 4,3 : 1.0.0.0 : 7
-- 4,3 : 1.0.0.1 : 14
-- 4,3 : 1.0.0.2 : 28
-- 4,3 : 1.0.0.3 : 46
-- 4,3 : 1.0.1.0 : 21
-- 4,3 : 1.0.1.1 : xx
-- 4,3 : 1.0.1.2 : xx
-- 4,3 : 1.0.1.3 : xx
-- 4,3 : 1.0.2.0 : xx
-- 4,3 : 1.0.2.1 : xx
-- 4,3 : 1.0.2.2 : xx
-- 4,3 : 1.0.2.3 : xx
-- 4,3 : 1.0.3.0 : xx
-- 4,3 : 1.0.3.1 : xx
-- 4,3 : 1.0.3.2 : xx
-- 4,3 : 1.0.3.3 : xx
-- 4,3 : 1.1.0.0 : 7
-- 4,3 : 1.1.0.1 : 14
-- 4,3 : 1.1.0.2 : 28
-- 4,3 : 1.1.0.3 : 46
-- 4,3 : 1.1.1.0 : 21
-- 4,3 : 1.1.1.1 : xx
-- 4,3 : 1.1.1.2 : xx
-- 4,3 : 1.1.1.3 : xx
-- 4,3 : 1.1.2.0 : xx
-- 4,3 : 1.1.2.1 : xx
-- 4,3 : 1.1.2.2 : xx
-- 4,3 : 1.1.2.3 : xx
-- 4,3 : 1.1.3.0 : xx
-- 4,3 : 1.1.3.1 : xx
-- 4,3 : 1.1.3.2 : xx
-- 4,3 : 1.1.3.3 : xx
-- 4,3 : 1.2.0.0 : 7
-- 4,3 : 1.2.0.1 : 14
-- 4,3 : 1.2.0.2 : 28
-- 4,3 : 1.2.0.3 : 46
-- 4,3 : 1.2.1.0 : 21
-- 4,3 : 1.2.1.1 : xx
-- 4,3 : 1.2.1.2 : xx
-- 4,3 : 1.2.1.3 : xx
-- 4,3 : 1.2.2.0 : xx
-- 4,3 : 1.2.2.1 : xx
-- 4,3 : 1.2.2.2 : xx
-- 4,3 : 1.2.2.3 : xx
-- 4,3 : 1.2.3.0 : xx
-- 4,3 : 1.2.3.1 : xx
-- 4,3 : 1.2.3.2 : xx
-- 4,3 : 1.2.3.3 : xx
-- 4,3 : 1.3.0.0 : 7
-- 4,3 : 1.3.0.1 : 14
-- 4,3 : 1.3.0.2 : 28
-- 4,3 : 1.3.0.3 : 46
-- 4,3 : 1.3.1.0 : 21
-- 4,3 : 1.3.1.1 : xx
-- 4,3 : 1.3.1.2 : xx
-- 4,3 : 1.3.1.3 : xx
-- 4,3 : 1.3.2.0 : xx
-- 4,3 : 1.3.2.1 : xx
-- 4,3 : 1.3.2.2 : xx
-- 4,3 : 1.3.2.3 : xx
-- 4,3 : 1.3.3.0 : xx
-- 4,3 : 1.3.3.1 : xx
-- 4,3 : 1.3.3.2 : xx
-- 4,3 : 1.3.3.3 : xx
-- 4,3 : 2.0.0.0 : 49
-- 4,3 : ...
-- 4,3 : 3.3.3.3 : 9261000
--
-- 4,4 : 0.0.0.4 : 16
-- 4,4 : 0.0.1.4 : xx
-- 4,4 : 0.0.2.4 : xx
-- 4,4 : 0.0.3.4 : xx
-- 4,4 : 0.0.4.0 : xx
-- 4,4 : 0.0.4.1 : xx
-- 4,4 : 0.0.4.2 : xx
-- 4,4 : 0.0.4.3 : xx
-- 4,4 : 0.0.4.4 : xx
-- 4,4 : 0.1.0.4 : xx
-- 4,4 : 0.1.1.4 : xx
-- 4,4 : 0.1.2.4 : xx
-- 4,4 : 0.1.3.4 : xx
-- 4,4 : 0.1.4.0 : xx
-- 4,4 : 0.1.4.1 : xx
-- 4,4 : 0.1.4.2 : xx
-- 4,4 : 0.1.4.3 : xx
-- 4,4 : 0.1.4.4 : xx
-- 4,4 : 0.2.0.4 : xx
--
-- 4,4 : 4.4.4.4 : 1944810000
