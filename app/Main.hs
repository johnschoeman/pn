import Control.Lens
import Data.Char
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List as List
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart = toRenderable layout
  where
    pns :: Int -> [(Int, Int)]
    pns x = zip [1..](map toDecimal (take x pnNumbers))

    points = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (pns 10000)
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot points]
           $ def

main = renderableToFile def "seq.png" chart


-- let n = take 1000 pnNumbers
-- mapM_ pnShow n

pnNumbers = [0] : [1] : [ pnSucc a | a <- tail pnNumbers]

pnShow :: [Int] -> IO ()
pnShow pn =
  let
    dec = toDecimal pn
  in
    print (show pn ++ " : " ++ show dec)


primes = [2,3,5,7]

toDecimal :: [Int] -> Int
toDecimal pn =
  let
    a = zip primes (reverse pn)
  in
    foldl (\acc (p,n) -> acc * (p ^ n)) 1 a

pnSucc :: [Int] -> [Int]
pnSucc xs = 
   let
     o = order xs
     maxPower = maximum' xs
   in
     if o > maxPower then
       addOneInBase (o - 1) xs
     else
       incrementPrime o xs

-- 1.1.0.1 -> 1.1.0.2
-- 1.3.3.3 -> 3.0.0.0
-- o:104 ...0.102.3.73.3 -> ...0.102.3.73.4
-- o:104 ...0.102.3.73.103 -> ...0.102.3.74.0
addOneInBase :: Int -> [Int] -> [Int]
addOneInBase o xs =
  let
    r = List.reverse xs
    (zeros, rest) = break (< o) r
  in
    case rest of
      [] -> List.replicate o 0 ++ [o+1]
      head:rs -> reverse (List.map (const 0) zeros ++ [head + 1] ++ rs)

-- 0.4.1.4 -> 0.4.2.4
-- 0.1.4.4 -> 0.2.0.4
-- :104 ...0.1.104.2  -> ...0.1.104.3
incrementPrime :: Int -> [Int] -> [Int]
incrementPrime o xs =
  if List.all (== o) xs then
    1 : List.replicate o 0
  else
    let
      next = addOneInBase o xs
    in
      if o `elem` next
        then next
        else incrementPrime o next


maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y -> if x >= y then x else y)

order :: (Num a, Num b) => [a] -> b
order = foldr (\x -> (+) 1) 0

-- {P} :: set of primes, 2 to P
-- {N} :: set of powers, 0 to N
-- |P| count of primes
-- |N| count of poweres == max N
-- total size : |N|*|P|
--
-- Prime Notation : 
-- [7's place].[5's place].[3's place].[2's place]
-- 1.0.1.3 :: 7^1 * 5^0 * 3^1 * 2^3 == 168
--
-- order :: min of (max power) and |P| 

-- p : { 2 } ; n : { 0 }
-- 0
-- 1
--
-- p : { 2 } ; n : { 0, 1 }
-- 0.1
-- 1.1
--
-- p : { 2, 3 } ; n : { 0, 1 }
-- 0.0
-- 1.0
-- 0.1
-- 1.1
--
-- p : { 2, 3 } ; n : { 0, 1, 2 }  
--
-- 0.0
-- 1.0
-- 2.0
-- 0.1
-- 1.1
-- 2.1
-- 0.2
-- 1.2
-- 2.2
--
-- 0.0
-- 0.1
-- 0.2
-- 1.0
-- 1.1
-- 1.2
-- 2.0
-- 2.1
-- 2.2
--
-- p : { 2, 3, 5 } ; n : { 0, 1, 2 }
-- 0.0.0 0.0.1 0.0.2
-- 1.0.0 1.0.1 1.0.2
-- 2.0.0 2.0.1 2.0.2
-- 0.1.0 0.1.1 0.1.2
-- 1.1.0 1.1.1 1.1.2
-- 2.1.0 2.1.1 2.1.2
-- 0.2.0 0.2.1 0.2.2
-- 1.2.0 1.2.1 1.2.2
-- 3.2.0 3.2.1 3.2.2
--
-- iterations --
-- i : p,n : size
-- 0 : 1,0 : 1
-- 1 : 1,1 : 2
-- 2 : 2,1 : 4
-- 3 : 2,2 : 9
-- 4 : 3,2 : 
-- 5 : 3,3
-- 6 : 4,3
-- 7 : 4,4
-- 8 : 5,4
-- 9 : 5,5
--
-- n odd,  (n+1)/2,(n+1)/2
-- n even, (n+2)/2,n/2
--
-- 2, 3
-- 0, 1, 2
-- 2^0*3^0  0.0
-- 2^1*3^0  0.1
-- 2^2*3^0  0.2
-- 2#0*3^0  1.0
--
-- 2, 3, 5
-- 0, 1, 2

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
