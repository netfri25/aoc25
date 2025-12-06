import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad (guard)
import Data.Function (fix)


main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  print $ part1 input


type Position = (Int, Int)
type Input = S.Set Position

parseInput :: String -> Input
parseInput input = S.fromList $ do
  (row, line) <- zip [1..] $ lines input
  (col, char) <- zip [1..] line
  guard $ char == '@'
  return (col, row)

part1 :: Input -> Int
part1 = S.size . toRemove . countNbors

part2 :: Input -> Int
part2 input = S.size $ S.difference input $ findAnswer input
  where
    step input = S.difference input $ toRemove $ countNbors input
    findAnswer input =
      let result = step input
       in if S.size result  == S.size input
          then result
          else findAnswer result

toRemove :: M.Map Position Int -> S.Set Position
toRemove = M.keysSet . M.filter (<4)

countNbors :: S.Set Position -> M.Map Position Int
countNbors input = foldr (\k -> M.insertWith (+) k 1) initial_map all_nbors
  where
    initial_map = M.fromSet (const 0) input
    all_nbors = concatMap (filter (`S.member` input) . nbors) input

nbors :: Position -> [Position]
nbors (x, y) =
  [ (x', y')
  | x' <- [x-1..x+1]
  , y' <- [y-1..y+1]
  , x /= x' || y /= y'
  ]
