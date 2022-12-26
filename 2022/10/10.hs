main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          putStrLn (part2 file')
          putStrLn (part2 file)

data Instruction = Noop | Addx Int

parseLine :: String -> Instruction
parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':int) = Addx (read int)

parseInput :: String -> [Instruction]
parseInput = map parseLine . lines

registerValues ::  [Int] -> [Instruction] -> [Int]
registerValues rs [] = rs
registerValues (r : rs) (Noop : is) = r : registerValues (r : rs) is
registerValues (r : rs) (Addx v : is) = r : r : registerValues (r + v : rs) is

part1 :: String -> Int
part1 = sum . helper [20, 60..220] . registerValues [1] . parseInput
 where
   helper :: [Int] -> [Int] -> [Int]
   helper [] rs = []
   helper (x : xs) rs = x * rs !! (x - 1) : helper xs rs

displayPixel :: Int -> Int -> Char
displayPixel register width =
  let dim = fromEnum '.'
      lit = fromEnum '#'
      isLit = (abs (register - width) <= 1)
  in toEnum (dim + ((lit - dim) * fromEnum isLit))


drawPixels :: [Instruction] -> String
drawPixels = helper' . helper 1 0
  where
    f = (`rem` 40)

    helper :: Int -> Int -> [Instruction] -> String
    helper r w [] = []
    helper r w (Noop : is) =
      displayPixel r w : helper r (f (w + 1)) is
    helper r w (Addx v : is) =
      displayPixel r w : displayPixel r (f (w + 1)) : helper (r + v) (f (w + 2)) is

    helper' :: String -> String
    helper' [] = []
    helper' str =
      let (line, str') = splitAt 40 str
      in line ++ '\n' : helper' str'

part2 :: String -> String
part2 = drawPixels . parseInput
