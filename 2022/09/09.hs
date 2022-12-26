import Data.Bifunctor
import Data.List

apply :: Int -> (a -> a) -> a -> a
apply 0 f = id
apply n f = apply (pred n) f . f

main = do file' <- readFile "sample_input"
          file'' <- readFile "sample_input2"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          print (part2 file')
          print (part2 file'')
          print (part2 file)

type Coord = (Int, Int)
--type Model = (Coord, Coord)
--
--initCoord :: Coord
--initCoord = (0, 0)
--
--initModel :: Model
--initModel = (initCoord, initCoord)
--
--genericMove :: (b -> a) -> (b -> t) -> (t -> t -> Bool) -> (b, b) -> (a, b)
--genericMove f g cond (head, tail) =
--  let head' = f head
--      tail' = (if cond (g head) (g tail) then head else tail)
--  in (head', tail')
--
--up :: Model -> Model
--up = genericMove (second succ) snd (>)
--
--left :: Model -> Model
--left = genericMove (first pred) fst (<)
--
--right :: Model -> Model
--right = genericMove (first succ) fst (>)
--
--down :: Model -> Model
--down = genericMove (second pred) snd (<)
--
--charToMove :: Char -> (Model -> Model)
--charToMove 'U' = up
--charToMove 'L' = left
--charToMove 'R' = right
--charToMove 'D' = down
--charToMove _ = undefined
--
--parseLine :: String -> (Model -> Model, Int)
--parseLine (c : ' ' : int) = (charToMove c, read int)
--
--parseInput :: String -> [(Model -> Model, Int)]
--parseInput = map parseLine . lines
--
--part1 :: String -> Int
--part1 = length . nub . helper initModel . parseInput
--  where
--    helper :: Model -> [(Model -> Model, Int)] -> [Coord]
--    helper model [] = [snd model]
--    helper model ((f, 0) : xs) = helper model xs
--    helper model ((f, n) : xs) = snd model : helper (f model) ((f, pred n) : xs)

type Model = [Coord]

followHead :: Coord -> Coord -> Coord
followHead head@(x, y) tail@(x', y') =
  let f = helper x x'
      s = helper y y'
  in if f == 0 && s == 0
     then tail
     else bimap (f +) (s +) head
  where
    helper :: Int -> Int -> Int
    helper n n' =  (n' - n) `quot` 2

genericMove f = helper
  where
    helper [] = undefined
    helper [x] = [f x]
    helper (x : xs@(_ : _)) =
      let xs'@(_ : _) = helper xs
          y' = head xs'
          x' = followHead y' x
      in x' : xs'

up :: Model -> Model
up = genericMove (second succ)

left :: Model -> Model
left = genericMove (first pred)

right :: Model -> Model
right = genericMove (first succ)

down :: Model -> Model
down = genericMove (second pred)

charToMove :: Char -> (Model -> Model)
charToMove 'U' = up
charToMove 'L' = left
charToMove 'R' = right
charToMove 'D' = down
charToMove _ = undefined

parseLine :: String -> (Model -> Model, Int)
parseLine (c : ' ' : int) = (charToMove c, read int)

parseInput :: String -> [(Model -> Model, Int)]
parseInput = map parseLine . lines

traceTail :: Model -> [(Model -> Model, Int)] -> Model
traceTail [] _ = undefined
traceTail model@(_ : _) [] = [head model]
traceTail model@(_ : _) ((f, 0) : xs) = traceTail model xs
traceTail model@(_ : _) ((f, n) : xs) = head model : traceTail (f model) ((f, pred n) : xs)

part1 :: String -> Int
part1 = length . nub . traceTail (replicate 2 (0, 0)) . parseInput

part2 :: String -> Int
part2 = length . nub . traceTail (replicate 10 (0, 0)) . parseInput
