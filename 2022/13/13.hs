import Control.Monad
import Data.Bifunctor
import Data.List

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          --print (parseInput' file')
          print (part2 file')
          print (part2 file)

splitBySublist :: Eq a => [a] -> [a] -> ([a], [a])
splitBySublist _ [] = ([], [])
splitBySublist xs (y : ys) =
  case stripPrefix xs (y : ys) of
       Nothing -> first (y :) (splitBySublist xs ys)
       Just zs -> ([], zs)

splitBySublist' :: Eq a => [a] -> [a] -> [[a]]
splitBySublist' xs [] = []
splitBySublist' xs ys =
  let (y', ys') = splitBySublist xs ys
  in y' : splitBySublist' xs ys'


data ListAB = A Int | B [ListAB]
  deriving (Eq, Show)

instance Ord ListAB where
  (<=) (A x) (A y) = x <= y
  (<=) (A x) (B ys) = B [A x] <= (B ys)
  (<=) (B xs) (A y) = (B xs) <= B [A y]
  (<=) (B []) (B ys) = True
  (<=) (B (x : xs)) (B []) = False
  (<=) (B (x : xs)) (B (y : ys)) =
    case compare x y of
     LT -> True
     EQ -> B xs <= B ys
     GT -> False

splitByComma' :: String -> [String]
splitByComma' [] = []
splitByComma' str@('[':_) =
  case helper 0 str of
    (x, rest') -> x : splitByComma' rest'
  where
    helper :: Int -> String -> (String, String)
    helper 0 [] = ([], [])
    helper 0 (',':rest) = ([], rest)
    helper n (c:rest) =
      let f = case c of
                '[' -> succ
                ']' -> pred
                _ -> id
      in first (c :) (helper (f n) rest)
splitByComma' str =
  case break (== ',') str of
    (x, []) -> [x]
    (x, ',':rest) -> x : splitByComma' rest

parseLine :: String -> ListAB
parseLine [] = undefined
parseLine ('[':rest) =
  case init rest of
    [] -> B []
    rest' -> B ((map parseLine . splitByComma') rest')
parseLine x = A (read x)

parseInput :: String -> [(ListAB, ListAB)]
parseInput = map (helper . map parseLine . lines) . splitBySublist' "\n\n"
  where
    helper :: [ListAB] -> (ListAB, ListAB)
    helper (x : y : []) = (x, y)

part1 :: String -> Int
part1 = (helper 1) . parseInput
  where
    helper :: Int -> [(ListAB, ListAB)] -> Int
    helper n [] = 0
    helper n ((x, y) : xs) =
      if x <= y
      then n + helper (succ n) xs
      else helper (succ n) xs

parseInput' :: String -> [ListAB]
parseInput' = join . map (map parseLine . lines) . splitBySublist' "\n\n"

part2 :: String -> Int
part2 input =
  let x = parseLine "[[2]]"
      y = parseLine "[[6]]"
      xs = sort (x : y : parseInput' input)
      Just x' = elemIndex x xs >>= pure . succ
      Just y' = elemIndex y xs >>= pure . succ
  in x' * y'
