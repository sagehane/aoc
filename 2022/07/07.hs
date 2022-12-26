{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import Data.Bifunctor
import Data.List

main = do file' <- readFile "sample_input"
          file <- readFile "input"
          print (part1 file')
          print (part1 file)
          print (part2 file')
          print (part2 file)

type File = (Int, String)

data Dir = MkDir
  { name :: String
  , dirs :: [Dir]
  , files :: [File]
  }

emptyDir :: String -> Dir
emptyDir name = MkDir name [] []

setFiles :: Dir -> [File] -> Dir
setFiles dir files = dir { files = files }

addDir :: Dir -> Dir -> Dir
addDir outer inner = outer { dirs = inner : dirs outer }

parseFile :: String -> File
parseFile = bimap read tail . break (== ' ')

parseLs :: [String] -> [File]
parseLs [] = []
parseLs (('d' : _) : strs) = parseLs strs
parseLs (file : strs) = parseFile file : parseLs strs

splitAtDelimiter :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
splitAtDelimiter isLimiter isDelimiter = helper 0
  where
    helper n [] = ([], [])
    helper n xs'@(x : xs) =
      if n == 0 && isDelimiter x
      then ([], xs')
      else let n' = (if isDelimiter x
                     then pred
                     else if isLimiter x
                          then succ
                          else id) n
           in (first (x :) . helper n') xs

parseCmds :: (Dir, [String]) -> Dir
parseCmds = fst . helper
  where
    helper :: (Dir, [String]) -> (Dir, [String])
    helper (dir, cmds) =
      case cmds of
        [] -> (dir, [])
        "$ cd .." : cmds' -> (dir, cmds')
        ('$':' ':'c':'d':' ':name) : cmds ->
          (helper . first (addDir dir) . helper) (emptyDir name, cmds)
        "$ ls" : cmds ->
          (helper . first (setFiles dir . parseLs) . break (isPrefixOf "$")) cmds

parseInput :: String -> Dir
parseInput input = parseCmds (emptyDir "/", (tail . lines) input)

dirSize :: Dir -> Int
dirSize dir = (sum . map fst . files) dir + (sum . map dirSize . dirs) dir

dirSizes :: Dir -> [Int]
dirSizes dir = dirSize dir : (join . map dirSizes . dirs) dir

part1 :: String -> Int
part1 = sum . filter (<= 100_000) . dirSizes . parseInput

part2 :: String -> Int
part2 input = let dir = parseInput input
              in (head . sort . filter (>= dirSize dir - (70_000_000 - 30_000_000)) . dirSizes) dir
