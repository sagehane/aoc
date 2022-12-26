import Data.List

main :: IO ()
main = do
  file' <- readFile "sample_input"
  file <- readFile "input"
  print (part1 file')
  print (part1 file)
  print (part2 file')
  print (part2 file)

splitBySub :: Eq a => [a] -> [a] -> [[a]]
splitBySub [] _ = undefined
splitBySub _ [] = []
splitBySub needle (x : xs) =
  case (stripPrefix needle (x : xs), splitBySub needle xs) of
    (Nothing, []) -> [[x]]
    (Nothing, y : ys) -> (x : y) : ys
    (Just xs', _) -> [] : splitBySub needle xs'

parseInput :: String -> [[Word]]
parseInput = map (map read . lines) . splitBySub "\n\n"

part1 :: String -> Word
part1 = maximum . map sum . parseInput

part2 :: String -> Word
part2 = sum . helper [0, 0, 0] . map sum . parseInput
  where
    helper :: [Word] -> [Word] -> [Word]
    helper = foldl (\acc x -> (init . reverse . sort) (x : acc))
