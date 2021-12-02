-- https://adventofcode.com/2021/day/2

main = do
  contents <- getContents
  let parsedLines = fmap parse (lines contents)
  let (x, y) = solve1 parsedLines
  print (x, y)
  print (x * y)

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