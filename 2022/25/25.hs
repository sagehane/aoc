import Data.Char

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print $ part1 file'
          print $ part1 file

parseLine :: String -> Int
parseLine = f 0
  where
    f :: Int -> String -> Int
    f n [] = n
    f n (c:cs) = f (5 * n + snafuToInt c) cs

snafuToInt :: Char -> Int
snafuToInt '-' = -1
snafuToInt '=' = -2
snafuToInt c = digitToInt c

intToFive :: Int -> [Int]
intToFive = f []
  where
    f :: [Int] -> Int -> [Int]
    f xs n =
      let (q, r) = (n `quot` 5, n `rem` 5)
      in if (q == 0)
         then r : xs
         else f (r : xs) q

intToSnafu :: Int -> String
intToSnafu n =
  let fives = intToFive n
  in reverse . f . reverse $ fives
  where
    f :: [Int] -> String
    f [] = []
    f (x : y : xs) =
      case x of
        3 -> '=' : f (succ y : xs)
        4 -> '-' : f (succ y : xs)
        _ -> intToDigit x : f (y : xs)
    f [3] = "=1"
    f [4] = "-1"
    f [x] = [intToDigit x]

part1 :: String -> String
part1 = intToSnafu . sum . map parseLine . lines
