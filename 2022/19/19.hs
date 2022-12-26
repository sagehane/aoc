import Data.Bifunctor
import Data.List

data Resource = Ore Int | Clay Int | Obsidian (Int, Int) | Geode (Int, Int)

data Stat = MkStat
  { amount :: Int
  , production :: Int
  } deriving Show

--data Model = MkModel
--  { ore :: (Ore
--
--succProduction :: Resource -> Resource
--succProduction (MkResource a p) = MkResource a (succ p)
--
--produceResource :: Int -> Resource -> Resource
--produceResource turn r@(MkResource a p) = r { amount = a + p * turn }
--
--data Model = MkModel
--  { ore :: (Resource, Int)
--  , clay :: (Resource, Int)
--  , obsidian :: (Resource, (Int, Int))
--  , geode :: (Resource, (Int, Int))
--  } deriving Show
--
--spendTurn :: Int -> Model -> Model
--spendTurn n m@(MkModel o c ob g) =
--  let s = first (produceResource n)
--  in MkModel (s o) (s c) (s ob) (s g)
--
----consumeResource :: Int -> (Model -> (Resource, a)) -> Model -> Model
----consumeResource n f m
--
--parseBlueprint :: String -> Model
--parseBlueprint input =
--  let r = MkResource 0 0
--      Just (o, rest) =
--        first read . break (== ' ') <$> (((snd . break (== ':') <$> stripPrefix "Blueprint " input)
--        >>= stripPrefix ": Each ore robot costs "))
--      Just (c, rest') =
--        first read . break (== ' ') <$> stripPrefix " ore. Each clay robot costs " rest
--      Just (obo, Just (obc, rest'')) =
--        bimap read (\x -> first read . break (== ' ') <$> stripPrefix " ore and " x) . break (== ' ')
--        <$> stripPrefix " ore. Each obsidian robot costs " rest'
--      Just (gor, Just (gob, " obsidian.")) =
--        bimap read (\x -> first read . break (== ' ') <$> stripPrefix " ore and " x) . break (== ' ')
--        <$> stripPrefix " clay. Each geode robot costs " rest''
--  in MkModel (succProduction r, o) (r, c) (r, (obo, obc)) (r, (gor, gob))
--
--turnRequired :: (Resource, Int) -> Maybe Int
--turnRequired (MkResource a 0, n) = if a > n then Just 1 else Nothing
--turnRequired (MkResource a p, n) =
--  let n' = max (n - a) 0
--  in Just (n' `quot` p + fromEnum (n' `rem` p > 0) + 1)
--
--mkOreBot :: (Int, Model) -> Maybe (Int, Model)
--mkOreBot (n, m@(MkModel o _ _ _)) = do
--  t <- turnRequired o
--  let n' = n - t
--  if n' > 0
--  then let m' = spendTurn t m
--       in Just (n', m' { ore = first succProduction . ore $ m } )
--  else Nothing
--
--mkClayBot :: (Int, Model) -> Maybe (Int, Model)
--mkClayBot (n, m@(MkModel o c _ _)) = do
--  t <- turnRequired (fst o, snd c)
--  let n' = n - t
--  if n' > 0
--  then let m' = spendTurn t m
--       in Just (n', m' { clay = first succProduction . clay $ m } )
--  else Nothing
--
--mkObsidianBot :: (Int, Model) -> Maybe (Int, Model)
--mkObsidianBot (n, m@(MkModel o c ob _)) = do
--  to <- turnRequired n (fst o, fst . snd $ ob)
--  tc <-  turnRequired n (fst c, snd . snd $ ob)
--  let t = max to tc
--  let n' n - t
--  if n' > 0
--  then let m' = spendTurn t m
--       in Just (n', m' { clay = first succProduction . clay $ m } )
--  else Nothing
