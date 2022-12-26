import Data.Bifunctor
import Data.Char
import Data.List

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          print (part2 file')
          print (part2 file)

parseLine :: String -> [Int]
parseLine = map digitToInt

parseInput :: String -> [[Int]]
parseInput = map parseLine . lines

getVisible :: Ord a => [a] -> [Bool]
getVisible xs = zipWith (||) (oneSide xs) ((reverse . oneSide . reverse) xs)
  where
    oneSide :: Ord a => [a] -> [Bool]
    oneSide [] = []
    oneSide (x : xs) = True : helper x xs

    helper x [] = []
    helper x (y : xs) = (y > x) : helper (max x y) xs

getVisibles :: Ord a => [[a]] -> [[Bool]]
getVisibles xs = zipWith (zipWith (||)) (map getVisible xs) ((transpose . map getVisible . transpose) xs)

getVisibleCoord :: (Eq a, Ord a) => [a] -> [Int]
getVisibleCoord xs = oneSide xs `union` map (pred . (length xs -)) (oneSide (reverse xs))
  where
    oneSide [] = []
    oneSide (x : xs) = 0 : helper 1 x xs

    helper n x [] = []
    helper n x (y : xs) = if y > x then n : helper (succ n) y xs else helper (succ n) x xs

getVisibleCoords :: (Eq a, Ord a) => [[a]] -> [(Int, Int)]
getVisibleCoords xs =
  let f (a, b) = (b, a)
  in nub (helper 0 xs ++ (map f . helper 0 . transpose) xs)
  where
    helper :: Ord a => Int -> [[a]] -> [(Int, Int)]
    helper n [] = []
    helper n (x : xs) =
      let f x = (n, x)
      in map f (getVisibleCoord x) ++ helper (succ n) xs

part1 :: String -> Int
part1 = sum . map (sum . map fromEnum) . getVisibles . parseInput
--part1 = sum . map (length . filter id) . getVisibles . parseInput
--part1 = length . getVisibleCoords . parseInput

getViews :: [[a]] -> (Int, Int) -> (a, [a], [a], [a], [a])
getViews xs (x, y) =
  let (up', _ : down) = splitAt y (transpose xs !! x)
      (left', z : right) = splitAt x (xs !! y)
      up = reverse up'
      left = reverse left'
  in (z, up, left, right, down)

scenicScore :: Ord a => [[a]] -> (Int, Int) -> Int
scenicScore xs coord =
  let (z, up, left, right, down) = getViews xs coord
  in (product . map (helper z)) [up, left, right, down]
  where
    helper x [] = 0
    helper x (y : xs) = if x > y then 1 + helper x xs else 1

part2 :: String -> Int
part2 input =
  let trees = parseInput input
      width = length (trees !! 0) - 1
      height = length trees - 1
      coords = [(x, y) | x <- [0..width], y <- [0..height]]
  in (maximum . map (scenicScore trees)) coords
