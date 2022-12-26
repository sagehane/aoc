import Data.Bifunctor
import Data.List

type Coord = (Int, Int)

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print $ part1 10 file'
          print $ part1 2000000 file
          print $ part2 20 file'
          print $ part2 4000000 file

parseLine :: String -> (Coord, Coord)
parseLine line =
  let Just (x0, line') = first read . break (== ',') <$> stripPrefix "Sensor at x=" line
      Just (y0, line'') = first read . break (== ':') <$> stripPrefix ", y=" line'
      Just (x1, line''') = first read . break (== ',') <$> stripPrefix ": closest beacon is at x=" line''
      Just y1 = read <$> stripPrefix ", y=" line'''
  in ((x0, y0), (x1, y1))

diff :: Int -> Int -> Int
diff x y = abs $ x - y

modulus :: (Coord, Coord) -> Int
modulus x =
  let m g f (a, b) = g (f a) (f b)
      m' = m diff
  in m' fst x + m' snd x

checkedRow :: Int -> (Coord, Coord) -> Maybe Coord
checkedRow y coord =
  let m = modulus coord
      x' = fst . fst $ coord
      y' = snd . fst $ coord
      m' = m - diff y y'
  in if m' < 0
     then Nothing
     else Just (x' - m', x' + m')

reduceMaybe :: [Maybe a] -> [a]
reduceMaybe [] = []
reduceMaybe (Nothing : xs) = reduceMaybe xs
reduceMaybe (Just x : xs) = x : reduceMaybe xs

composeRange :: Coord -> [Coord] -> [Coord]
composeRange (x, y) [] = [(x, y)]
composeRange range@(x, y) (range'@(x', y') : ranges) =
  if x > y' + 1 || y < x' - 1
  then range' : composeRange range ranges
  else composeRange (min x x', max y y') ranges

part1 :: Int -> String -> Int
part1 y input =
  let coords = map parseLine . lines $ input
      ranges = foldr composeRange [] . reduceMaybe . map (checkedRow y) $ coords
      checked = sum . map (\(a, b) -> b - a + 1) $ ranges
      beacons = length . nub . filter (y ==) . map (snd . snd) $ coords
  in checked - beacons

part2 :: Int -> String -> Int
part2 n input =
  let coords = map parseLine . lines $ input
      checkedRow' x y = bimap (max 0) (min n) <$> checkedRow x y

      helper :: Int -> ([Coord], Int)
      helper -1 = undefined
      helper x =
        let ranges = foldr composeRange [] . reduceMaybe . map (checkedRow' x) $ coords
        in if (ranges == [(0, n)])
           then helper (pred x)
           else (ranges, x)

      (x, y) = first (succ . snd . head . sort) (helper n)
  in 4000000 * x + y
