-- https://adventofcode.com/2021/day/1

main = do
  contents <- getContents
  let xs = numbers contents
  print $ solve1 xs
  print $ solve2 xs

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

solve2 :: [Int] -> Int
solve2 xs =
  let queue = reverse $ take 3 xs
      (_, count) = foldl f (queue, 0) (drop 3 xs)
   in count
  where
    newQueue queue x = take 3 (x : queue)
    f (previous, count) x =
      let newQ = newQueue previous x
       in if (sum newQ) > (sum previous)
            then (newQ, count + 1)
            else (newQ, count)