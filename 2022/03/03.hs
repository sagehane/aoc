import Data.List

main :: IO ()
main = do
  file' <- readFile "sample_input"
  file <- readFile "input"
  print (part1 file')
  print (part1 file)
  print (part2 file')
  print (part2 file)

halveList :: [a] -> ([a], [a])
halveList xs = splitAt (length xs `div` 2) xs

intersect' :: Eq a => ([a], [a]) -> a
intersect' (xs, ys) = head (xs `intersect` ys)

charToInt :: Char -> Int
charToInt char =
  let c = fromEnum char
      a = fromEnum 'a'
      a' = fromEnum 'A'
   in c - a + fromEnum (c < a) * (a - a' + 26) + 1

part1 :: String -> Int
part1 = sum . map (charToInt . intersect' . halveList) . lines

splitByThree :: [a] -> [[a]]
splitByThree xs = case splitAt 3 xs of
  (ys, []) -> [ys]
  (ys, zs) -> ys : splitByThree zs

intersects :: Eq a => [[a]] -> [a]
intersects [] = []
intersects [x] = x
intersects (x : y : xs) = x `intersect` y `intersect` intersects xs

part2 :: String -> Int
part2 = sum . map (charToInt . head . intersects) . splitByThree . lines
