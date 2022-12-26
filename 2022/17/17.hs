import Data.Bifunctor

sample_input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

data Rock = LineH | Plus | ReverseL | LineV | Box
  deriving Enum

nextRock :: Rock -> Rock
nextRock Box = LineH
nextRock rock = succ rock

moveRockLeft :: (Rock, Int) -> (Rock, Int)
moveRockLeft (rock, 0) = (rock, 0)
moveRockLeft (rock, x) = (rock, pred x)

moveRockRight :: (Rock, Int) -> (Rock, Int)
moveRockRight (LineH, 3) = (LineH, 3)
moveRockRight (Plus, 4) = (Plus, 4)
moveRockRight x =
  case x of
    (LineH, 2) -> x
    (Plus, 3) -> x
    (ReverseL, 3) -> x
    (LineV, 5) -> x
    (Box, 4) -> x
    _ -> second succ x

moveRock :: Char -> (Rock, Int) -> (Rock, Int)
moveRock '<' = moveRockLeft
moveRock '>' = moveRockRight

addRock :: (Rock, Int) -> [Int] -> [Int]
addRock (LineH, x) chamber =
  let (xs, ys) = first (replicate 4 . succ . maximum) . splitAt 4 $ chamber
  in xs ++ ys
addRock (Plus, x) chamber =
  case splitAt x chamber of
    (xs, a:b:c:ys) ->
      let m = max a c
          ys' = if m > b
               then m+1:m+2:m+1:ys
               else b+2:b+3:b+2:ys
      in xs ++ ys'
addRock (ReverseL, x) chamber =
  let (m, ys) = first (succ . maximum) . splitAt 3 $ chamber
  in [m,m,m+2] ++ ys
addRock (LineV, x) chamber =
  let (xs, y : ys) = splitAt x chamber
  in xs ++ y + 4 : ys
addRock (Box, x) chamber =
  let (xs, ys) = first (replicate 2 . (2 +) . maximum) . splitAt 2 $ chamber
  in xs ++ ys

part1 :: String -> Int
part1 input = maximum . helper 0 LineH $ (0, replicate 7 0, cycle input)
  where
    helper :: Int -> Rock -> (Int, [Int], String) -> [Int]
    helper 2022 rock (m, chamber, input) = chamber
    helper n rock (m, chamber, input) =
      let (rock', input') = first (foldl (flip moveRock) (rock, 2)) . splitAt 3 $ input
      in helper (succ n) (nextRock rock) . helper' 0 rock' $ (m, chamber, input')
      where
        helper' :: Int -> (Rock, Int) -> (Int, [Int], String) -> (Int, [Int], String)
        helper' n model = undefined
