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

