import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (Down(Down), comparing)
import Control.Monad (forM_)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  print $ part1 input
  print $ part2 input


parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines


part1 :: [[Int]] -> Int
part1 = sum . map (maximumJolt 2)

part2 :: [[Int]] -> Int
part2 = sum . map (maximumJolt 12)

maximumJolt :: Int -> [Int] -> Int
maximumJolt 1 xs = maximum xs
maximumJolt iterations xs =
  let digit_index = pred iterations
      (i, x) = maximumBy (comparing snd) $ drop digit_index $ reverse $ zip [1..] xs
      y = maximumJolt digit_index (drop i xs)
   in x * (10^digit_index) + y
