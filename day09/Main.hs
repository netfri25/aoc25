import           Control.Monad     (guard, when)
import           Control.Monad.ST  (ST, runST)
import           Data.Bits         (xor)
import           Data.Foldable     (forM_)
import           Data.List         (sort, tails)
import qualified Data.Set          as S
import           Data.STRef.Strict (modifySTRef, newSTRef, readSTRef,
                                    writeSTRef)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  print $ part1 input
  -- print $ part2 input

type Point = (Int, Int)

parseInput :: String -> [Point]
parseInput = map (\line -> read $ '(' : line ++ ")") . lines

part1 :: [Point] -> Int
part1 = maximum . areas

-- part2 :: [Point] -> Int
-- part2 points = maximum
--   $ map (uncurry area)
--   $ filter (all (inBoundary boundary) . boundaryFrom . uncurry quad)
--   $ map ((\[a,b,c,d] -> (a, c)) . uncurry quad)
--   $ pairs points
--   where
--     boundary = S.fromList $ boundaryFrom points

-- inBoundary :: S.Set Point -> Point -> Bool
-- inBoundary boundary p@(end_x, y) = (||) (S.member p boundary) $ runST $ do
--   is_in_ref <- newSTRef False
--   forM_ [0..end_x] $ \x -> do
--     let prev = (x - 1, y)
--     let curr = (x, y)
--     let diff = not (S.member prev boundary) && S.member curr boundary
--     modifySTRef is_in_ref (xor diff)
--   readSTRef is_in_ref

-- boundaryFrom :: [Point] -> [Point]
-- boundaryFrom points = concat $ zipWith fill points (last points : points)

-- fill :: Point -> Point -> [Point]
-- fill (x1, y1) (x2, y2) = do
--   x <- [min x1 x2 .. max x1 x2]
--   y <- [min y1 y2 .. max y1 y2]
--   return (x, y)

pairs :: [a] -> [(a, a)]
pairs xs = do
  (x:xs') <- init $ init $ tails xs
  (x,) <$> xs'

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

areas :: [Point] -> [Int]
areas = map (uncurry area) . pairs

-- quad :: Point -> Point -> [Point]
-- quad (x1, y1) (x2, y2) = zip [xl, xl, xh, xh] [yl, yh, yh, yl]
--   where
--     xl = min x1 x2
--     xh = max x1 x2
--     yl = min y1 y2
--     yh = max y1 y2

-- quads :: [Point] -> [[Point]]
-- quads = map (uncurry quad) . pairs

