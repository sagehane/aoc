import Data.List
import Data.Bifunctor

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          print (part2 file')
          print (part2 file)

splitBySublist :: Eq a => [a] -> [a] -> ([a], [a])
splitBySublist _ [] = ([], [])
splitBySublist xs (y : ys) = case stripPrefix xs (y : ys) of
                                  Nothing -> first (y :) (splitBySublist xs ys)
                                  Just zs -> ([], zs)

-- Given a line, get the characters inside the crate
filterCrates :: [a] -> [a]
filterCrates = helper . tail
  where
    helper [] = []
    helper (x : xs) = x : helper (drop 3 xs)

parseCrates :: String -> [String]
parseCrates = map (snd . span (== ' ')) . transpose . map filterCrates . init . lines

parseInstructions :: String -> [(Int, Int, Int)]
parseInstructions = map helper . lines
  where
    helper :: String -> (Int, Int, Int)
    helper i0 = let Just (x, i1) = stripPrefix "move " i0 >>= pure . bimap read tail . break (== ' ')
                    Just (y, i2) = stripPrefix "from " i1 >>= pure . bimap read tail . break (== ' ')
                    Just z = stripPrefix "to " i2 >>= pure . read
                in (x, y, z)

parseInput :: String -> ([String], [(Int, Int, Int)])
parseInput = bimap parseCrates parseInstructions . splitBySublist "\n\n"

applyIndex :: Int -> (a -> a) -> [a] -> [a]
applyIndex index f xs = case splitAt index xs of
                                 (a, []) -> xs
                                 (a, (y : ys)) -> a ++ f y : ys

helper :: [String] -> (Int, Int, Int) -> [String]
helper crates' (x, y, z) = let y_crate' = take x (crates' !! (pred y))
                               drop' = applyIndex (pred y) (drop x)
                               take' = applyIndex (pred z) (y_crate' ++)
                           in (take' . drop') crates'

part1 :: String -> String
part1 input = let (crates, instructions) = parseInput input
              in map head (foldl helper crates instructions)
  where
    helper :: [String] -> (Int, Int, Int) -> [String]
    helper crates' (x, y, z) = let y_crate' = take x (crates' !! (pred y))
                                   drop' = applyIndex (pred y) (drop x)
                                   take' = applyIndex (pred z) (reverse  y_crate' ++)
                               in (take' . drop') crates'

part2 :: String -> String
part2 input = let (crates, instructions) = parseInput input
              in map head (foldl helper crates instructions)
  where
    helper :: [String] -> (Int, Int, Int) -> [String]
    helper crates' (x, y, z) = let (a, b) = splitAt x (crates' !! (pred y))
                                   drop' = applyIndex (pred y) (const b)
                                   take' = applyIndex (pred z) (a ++)
                               in (take' . drop') crates'
