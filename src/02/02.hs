-- https://adventofcode.com/2021/day/2

main = do
  contents <- getContents
  let parsedLines = fmap parse (lines contents)
  let (x, d) = solve1 parsedLines
  print (x, d)
  print (x * d)
  let (x2, d2, a) = solve2 parsedLines
  print (x2, d2, a)
  print (x2 * d2)

breakOnChars :: String -> String -> [String]
breakOnChars _ "" = []
breakOnChars chars (x : xs) =
  if elem x chars
    then "" : (breakOnChars chars xs)
    else
      ( let ys = breakOnChars chars xs
         in if null ys
              then [[x]]
              else (x : head ys) : tail ys
      )

parse :: String -> (String, Int)
parse instr =
  let cmd : number : _ = breakOnChars " " instr
      n = read number :: Int
   in (cmd, n)

solve1 :: [(String, Int)] -> (Int, Int)
solve1 = foldl f (0, 0)
  where
    f (x, d) (c, i)
      | c == "forward" = (x + i, d)
      | c == "down" = (x, d + i)
      | otherwise = (x, d - i)

solve2 :: [(String, Int)] -> (Int, Int, Int)
solve2 = foldl f (0, 0, 0)
  where
    f (x, d, a) (c, i)
      | c == "forward" = (x + i, d + (a * i), a)
      | c == "down" = (x, d, a + i)
      | otherwise = (x, d, a - i)