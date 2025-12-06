import Control.Monad (guard)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  print $ part1 input
  print $ part2 input

part1 :: [[Int]] -> Int
part1 = sum . invalidIDs True . concat

part2 :: [[Int]] -> Int
part2 = sum . invalidIDs False . concat

split :: Eq a => a -> [a] -> [[a]]
split _ [] = mempty
split elem input =
  let (items, input') = span (/=elem) input
   in items : split elem (drop 1 input')

parseInput :: String -> [[Int]]
parseInput input = split ',' input >>= \text -> do
  let [l, h] = map read $ split '-' text
  return [l..h]

invalidIDs :: Bool -> [Int] -> [Int]
invalidIDs single_split = filter (invalidID single_split)

invalidID :: Bool -> Int -> Bool
invalidID single_split value = (&&) (value > 10) $ any (\chunks -> and $ zipWith (==) chunks (drop 1 chunks)) $ allChunks text chop_sizes
  where
    text = show value
    chop_sizes =
      if single_split
      then [(length text + 1) `div` 2]
      else reverse [1..length text `div` 2]

allChunks :: [a] -> [Int] -> [[[a]]]
allChunks input = mapMaybe (`chop` input)

chop :: Int -> [a] -> Maybe [[a]]
chop 0 _ = Nothing
chop size [] = return []
chop size items = do
  let chunk = take size items
  guard $ length chunk == size
  rest <- chop size (drop size items)
  return (chunk:rest)
