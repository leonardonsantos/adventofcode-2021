-- https://adventofcode.com/2021/day/3

import qualified Data.Map as Map

main = do
  contents <- getContents
  let parsedLines = fmap parse (lines contents)
  let size = length $ head parsedLines
  let (g, e) = solve1 parsedLines size
  print (g * e)

parse :: String -> [(Int, Char)]
parse = zip [0 ..]

bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

-- solve1 :: [[(Int, Char)]] -> Int ->  (Int, Int)
solve1 lines size =
  let hash = foldl f Map.empty lines
      gamma = foldl g "" [0 .. size -1]
      g :: [Char] -> Int -> [Char]
      g acc x =
        let c0 = hash Map.! (x, '0')
            c1 = hash Map.! (x, '1')
         in if c0 > c1 then acc ++ "0" else acc ++ "1"
      epsilon = fmap (\x -> if x == '0' then '1' else '0') gamma
      gammaDec = bin2dec gamma
      epsilonDec = bin2dec epsilon
   in (gammaDec, epsilonDec)
  where
    f :: Map.Map (Int, Char) Int -> [(Int, Char)] -> Map.Map (Int, Char) Int
    f h line = foldl f' h line
    f' :: Map.Map (Int, Char) Int -> (Int, Char) -> Map.Map (Int, Char) Int
    f' h x = Map.insertWithKey f'' x 1 h
    f'' :: (Int, Char) -> Int -> Int -> Int
    f'' k v1 v2 = v1 + v2
