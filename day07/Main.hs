import Data.List (elemIndices, sort)
import qualified Data.IntMap as IM

-- both Beam and Splitter are represented by their index in the current row
type Beam = Int
type Splitter = Int


main :: IO ()
main = do
  -- skips lines without splitters
  (starts, splitters) <- parseInput <$> readFile "input"

  print $ part1 starts splitters
  print $ part2 starts splitters


type Beams = IM.IntMap Int

part1 :: Beams -> [[Splitter]] -> Int
part1 starts splitters =
  let states = simulate starts splitters
      total_splitters = sum $ map length splitters
      unused_splitters = sum $ map length $ zipWith difference splitters (map IM.keys states)
   in total_splitters - unused_splitters

part2 :: Beams -> [[Splitter]] -> Int
part2 starts = sum . last . simulate starts

parseInput :: String -> (Beams, [[Splitter]])
parseInput input =
  let (start_line:splitters_lines) = lines input
      starts = foldr (\key -> IM.insertWith (+) key 1) mempty $ elemIndices 'S' start_line
      splitters = filter (not . null) $ map (elemIndices '^') splitters_lines
   in (starts, splitters)

simulate :: Beams -> [[Splitter]] -> [Beams]
simulate = scanl (flip splitBeams)

-- a - b for lists, except that the list should be sorted from low to high
difference :: Ord a => [a] -> [a] -> [a]
difference [] _ = []
difference xs [] = xs
difference (x:xs) (y:ys) =
  case compare x y of
    EQ -> difference xs ys
    LT -> x : difference xs (y:ys)
    GT -> difference (x:xs) ys


splitBeams :: [Splitter] -> Beams -> Beams
splitBeams [] beams = beams
splitBeams (splitter:splitters) beams =
  case beams IM.!? splitter of
    Nothing -> splitBeams splitters beams
    Just value -> IM.insertWith (+) (splitter - 1) value
                $ IM.insertWith (+) (splitter + 1) value
                $ splitBeams splitters (IM.delete splitter beams)
  where
    go [] beams = beams
    go splitters [] = []
    go (s:splitters) (b:beams) =
      case compare s b of
        LT -> go splitters (b:beams)
        GT -> b : go (s:splitters) beams
        EQ -> b - 1 : b + 1 : go (s:splitters) beams
