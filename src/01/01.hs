-- https://adventofcode.com/2021/day/1

main = do
  contents <- getContents
  let xs = numbers contents
  print $ solve1 xs

numbers :: String -> [Int]
numbers input =
  let allLines = lines input
      allNumbers = fmap read allLines
   in allNumbers

solve1 :: [Int] -> Int
solve1 xs =
  let (_, count) = foldl f (-1, -1) xs
   in count
  where
    f (previous, count) x = if x > previous then (x, count + 1) else (x, count)