import Data.List (transpose)

main :: IO ()
main = do
  content <- readFile "input"
  print $ part1 content
  print $ part2 content



type Op = Int -> Int -> Int
type Equation = (Op, [Int])
type Input = [Equation]


part1 :: String -> Int
part1 = sum . map (uncurry foldl1) . parseInput
  where
    parseInput :: String -> Input
    parseInput = map parseEquation . transpose . map words . reverse . lines

    parseEquation :: [String] -> Equation
    parseEquation ("+":rest) = ((+), map read rest)
    parseEquation ("*":rest) = ((*), map read rest)


part2 :: String -> Int
part2 = sum . map (uncurry foldl1) . parseInput
  where
    parseInput :: String -> Input
    parseInput content =
      let lines' = lines content
          ops = map parseOp $ filter (/=' ') $ last lines'
          values :: [[Int]] = map (map read) $ split "" $ map removeSpace $ transpose $ init lines'
       in zip ops values

    removeSpace :: String -> String
    removeSpace = filter (/=' ')

    parseOp :: Char -> Op
    parseOp '+' = (+)
    parseOp '*' = (*)


split :: Eq a => a -> [a] -> [[a]]
split _ [] = mempty
split elem input =
  let (items, input') = span (/=elem) input
   in items : split elem (drop 1 input')
