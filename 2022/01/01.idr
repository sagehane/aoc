import Data.String
import System.File

sampleInput = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

total stripPrefix : Eq a => (needle : List a) -> (haystack : List a) -> Maybe (List a)
stripPrefix [] haystack = Just haystack
stripPrefix _ [] = Nothing
stripPrefix (x :: xs) (y :: ys) =
  if x == y then stripPrefix xs ys
            else Nothing

||| Splits a list by another list as a delimiter
total splitBySub : Eq a => (needle : List a) -> {auto 0 ok : NonEmpty needle} ->
                                                (haystack : List a) -> List (List a)
splitBySub [] _ impossible
splitBySub _ [] = []
splitBySub needle@(x :: xs) (y :: ys) =
  case (stripPrefix needle (y :: ys), splitBySub needle ys) of
       (Nothing, []) => [[y]]
       (Nothing, z :: zs) => (y :: z) :: zs
       (Just xs', _) => [] :: assert_total (splitBySub (x :: xs) xs')

total joinMaybe : List (Maybe a) -> Maybe (List a)
joinMaybe = foldl (\acc, elem => do acc' <- acc
                                    elem' <- elem
                                    Just (elem' :: acc'))
                  (Just [])

total parseInput : (input : String) -> Maybe (List (List Nat))
parseInput =
  --joinMaybe . map (joinMaybe . map parsePositive . lines . pack) . splitBySub (unpack "\n\n") . unpack
  let joinMaybes = joinMaybe . (map joinMaybe)
      parseLines = map parsePositive . lines
      splitByNewlines = map pack . splitBySub (unpack "\n\n") . unpack
  in joinMaybes . map parseLines . splitByNewlines
--parseInput input = helper (lines input)
--  where
--    helper : List String -> Maybe (List (List Nat))
--    helper [] = Just []
--    helper ("" :: xs) =
--      do xs' <- helper xs
--         Just ([] :: xs')
--    helper (x :: xs) =
--      do x' <- parsePositive x
--         (y :: ys) <- helper xs
--         | [] => Just [[x']]
--         Just ((x' :: y) :: ys)

total part1 : String -> Maybe Nat
part1 input = do input' <- parseInput input
                 Just (maximum (map sum input'))
  where
    maximum : List Nat -> Nat
    maximum = foldl (\acc, elem => max acc elem) 0

total part2 : String -> Maybe Nat
part2 input = do input' <- parseInput input
                 (pure . sum) (foldl (\acc, elem => (take 3 . reverse . sort) (elem :: acc))
                                     [0, 0, 0]
                                     (map sum input'))

main : IO ()
main = do Right file <- openFile "input" Read
          | Left err => putStrLn (show err)
          Right input <- fRead file
          | Left err => putStrLn (show err)
          let Just part1_output := part1 input
          | Nothing => putStrLn ("Part 1 failed, input was: " ++ (show input))
          putStrLn ("Part 1 is: " ++ show (part1_output))
          let Just part2_output := part2 input
          | Nothing => putStrLn ("Part 2 failed, input was: " ++ (show input))
          putStrLn ("Part 2 is: " ++ show (part2_output))
