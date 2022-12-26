import Data.Bifunctor

main :: IO ()
main = do
  file' <- readFile "sample_input"
  file <- readFile "input"
  print (part1 file')
  print (part1 file)
  print (part2 file')
  print (part2 file)

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy x = second tail . span (/= x)

bimap' :: (a -> b) -> (a, a) -> (b, b)
bimap' f = bimap f f

parseInput :: String -> [((Int, Int), (Int, Int))]
parseInput = map (bimap' (bimap' read . splitBy '-') . splitBy ',') . lines

partSolver :: Enum b => (((Int, Int), (Int, Int)) -> b) -> String -> Int
partSolver f = sum . map (fromEnum . f) . parseInput

fullyContains :: Ord a => ((a, a), (a, a)) -> Bool
fullyContains (x, y) = helper (x, y) || helper (y, x)
  where
    helper ((x0, x1), (y0, y1)) = x0 <= y0 && x1 >= y1

part1 :: String -> Int
part1 = partSolver fullyContains

overlaps :: Ord a => ((a, a), (a, a)) -> Bool
overlaps ((x0, x1), (y0, y1)) = not (x1 < y0 || y1 < x0)

part2 :: String -> Int
part2 = partSolver overlaps
