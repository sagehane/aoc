import Data.Bifunctor

main = do file <- readFile "input"
          print (part1 file)
          print (part2 file)

areUnique :: Eq a => [a] -> Bool
areUnique [] = True
areUnique (x : xs) = x `notElem` xs && areUnique xs

-- Returns the index of the first non-unique item
-- Returns `length` if all are unique
uniqueTo :: Eq a => [a] -> Int
uniqueTo [] = 0
uniqueTo (x : xs) = if x `elem` xs then 1 else uniqueTo xs + 1

genericSolver :: Eq a => Int -> [a] -> Int
genericSolver n = helper . splitAt (n + 1)
  where
    helper :: Eq a => ([a], [a]) -> Int
    helper pair@(x : xs, y : ys) = if areUnique (x : xs)
                                   then n
                                   else 1 + helper (xs ++ [y], ys)

part1 :: String -> Int
part1 = genericSolver 4

part2 :: String -> Int
part2 = genericSolver 14
