data Rps = Rock | Paper | Scissors
  deriving (Eq, Enum)

succ3 :: Enum a => a -> a
succ3 = f 1
  where
    f num enum = toEnum ((fromEnum enum + num) `mod` 3)

pred3 :: Enum a => a -> a
pred3 = succ3 . succ3

instance Ord Rps where
  compare x y =
    if x == y
      then EQ
      else pred3 (compare (succ3 x) y)

rpsFromOrd :: Rps -> Ordering -> Rps
rpsFromOrd rps EQ = rps
rpsFromOrd rps ord = pred3 (rpsFromOrd rps (succ3 ord))

parseInput :: (Char -> a) -> String -> [(Rps, a)]
parseInput f = map helper . lines
  where
    helper [abc, ' ', xyz] = (abcToRps abc, f xyz)
    helper _ = undefined

abcToRps :: Char -> Rps
abcToRps char = toEnum (fromEnum char - fromEnum 'A')

calculate :: Ordering -> Rps -> Int
calculate x y = 3 * fromEnum x + fromEnum y + 1

part1 :: String -> Int
part1 = sum . map helper . parseInput xyzToRps
  where
    helper :: (Rps, Rps) -> Int
    helper (x, y) = calculate (compare y x) y

    xyzToRps :: Char -> Rps
    xyzToRps char = toEnum (fromEnum char - fromEnum 'X')

part2 :: String -> Int
part2 = sum . map helper . parseInput xyzToResult
  where
    helper :: (Rps, Ordering) -> Int
    helper (x, y) = calculate y (rpsFromOrd x y)

    xyzToResult :: Char -> Ordering
    xyzToResult = flip compare 'Y'

main :: IO ()
main = do
  file' <- readFile "sample_input"
  file <- readFile "input"
  print (part1 file')
  print (part1 file)
  print (part2 file')
  print (part2 file)
