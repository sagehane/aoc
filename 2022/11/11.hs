import Data.Bifunctor
import Data.List

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          print (part2 file')
          print (part2 file)

apply :: Int -> (a -> a) -> a -> a
apply 0 f = id
apply n f = apply (pred n) f . f

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

data Monkey = MkMonkey
  { items :: [Int]
  , operation :: Int -> Int
  , divTest :: Int
  , passNum :: (Int, Int)
  }

parseItems :: String -> [Int]
parseItems str =
  let Just items = stripPrefix "  Starting items: " str
  in (map read . splitBySublist' ", ") items

parseOperation :: String -> Int -> Int
parseOperation str =
  case stripPrefix "  Operation: new = old " str of
    Just "* old" -> \old -> (old * old)
    Just ('+':' ':int) -> (+ read int)
    Just ('*':' ':int) -> (* read int)

parseDivTest :: String -> Int
parseDivTest str =
  let Just n = stripPrefix "  Test: divisible by " str >>= (pure . read)
  in n

parsePassNum :: [String] -> (Int, Int)
parsePassNum (t : f : []) =
  let Just t' = stripPrefix "    If true: throw to monkey " t >>= (pure . read)
      Just f' = stripPrefix "    If false: throw to monkey " f >>= (pure . read)
  in (t', f')

parseMonkey :: String -> Monkey
parseMonkey str =
  let (_ : items : operation : divTest : passNum) = lines str
      items' = parseItems items
      operation' = parseOperation operation
      divTest' = parseDivTest divTest
      passNum' = parsePassNum passNum
  in MkMonkey items' operation' divTest' passNum'

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitBySublist' "\n\n"

appendItem :: Int -> Monkey -> Monkey
appendItem item monkey = monkey { items = items monkey ++ [item] }

-- TODO: I think I forgot to update the turn monkey in this function
--turn :: Monkey -> [Monkey] -> [Monkey]
--turn (MkMonkey [] o d p) ms = ms
--turn (MkMonkey (i:is) o d p) ms =
--  let i' = (o i `quot` 3)
--      f = if i' `rem` d == 0 then fst else snd
--  in turn (MkMonkey is o d p)
--          (helper i' (f p) ms)
--  where
--    helper :: Int -> Int -> [Monkey] -> [Monkey]
--    helper item index ms =
--      let (ms0, m : ms1) = splitAt index ms
--      in ms0 ++ appendItem item m : ms1
--
--round :: [Monkey] -> [Monkey]
--round = helper 0
--  where
--    helper :: Int -> [Monkey] -> [Monkey]
--    helper n ms =
--      if n == length ms
--      then ms
--      else helper (succ n) (turn (ms !! n) ms)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs =
  let (xsl, _ : xsr) = splitAt idx xs
  in xsl ++ x : xsr

--turn' :: Int -> [(Monkey, Int)] -> [(Monkey, Int)]
--turn' n ms =
--  case ms !! n of
--    (MkMonkey [] o d p, c) -> ms
--    (MkMonkey (i:is) o d p, c) ->
--      let i' = (o i) `div` 3
--          f = if i' `rem` d == 0 then fst else snd
--          ms' = replaceAt n (MkMonkey is o d p, succ c) ms
--      in turn' n (helper i' (f p) ms')
--  where
--    helper :: Int -> Int -> [(Monkey, Int)] -> [(Monkey, Int)]
--    helper item index ms =
--      let (ms0, m : ms1) = splitAt index ms
--      in ms0 ++ first (appendItem item) m : ms1
--
--    helper :: Int -> Int -> [(Monkey, Int)] -> [(Monkey, Int)]

turn' :: (Int -> Int) -> Int -> [(Monkey, Int)] -> [(Monkey, Int)]
turn' f n ms =
  let (msl, (MkMonkey is o d p, c) : msr) = splitAt n ms
      ms' = msl ++ (MkMonkey [] o d p, c + length is) : msr

      helper :: [Int] -> [(Monkey, Int)] -> [(Monkey, Int)]
      helper [] ms = ms
      helper (i : is) ms =
        let i' = f (o i)
            idx = (if i' `rem` d == 0 then fst else snd) p
            (msl, m : msr) = splitAt idx ms
            ms' = msl ++ first (appendItem i') m : msr
        in helper is ms'
  in helper is ms'

round' :: (Int -> Int) -> [(Monkey, Int)] -> [(Monkey, Int)]
round' f = helper 0
  where
    helper n ms =
      if n == length ms
      then ms
      else helper (succ n) (turn' f n ms)

part1 :: String -> Int
part1 =
  let counts = map snd . apply 20 (round' (`quot` 3)) . map (\m -> (m, 0)) . parseInput
  in product . take 2 . reverse . sort . counts

-- Consider getting the worry level of the product of all div tests?
part2 :: String -> Int
part2 input =
  let ms = parseInput input
      modulus = product (map divTest ms)
      counts = map snd . apply 10000 (round' (`rem` modulus)) . map (\m -> (m, 0))
  in (product . take 2 . reverse . sort . counts) ms
