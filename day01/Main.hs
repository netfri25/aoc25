main :: IO ()
main = do
  input <- parseInput <$> readFile "input"
  print $ part1 input
  print $ part2 input

type Dial = Int
type Rotation = Int
type Modulus = Int

modulus :: Modulus
modulus = 100

dialStart :: Dial
dialStart = 50

parseInput :: String -> [Rotation]
parseInput = map parseRotation . lines

parseRotation :: String -> Rotation
parseRotation ('L':rest) = negate $ read rest
parseRotation ('R':rest) = read rest

applyRotation :: Dial -> Rotation -> Dial
applyRotation dial rot = (dial + rot) `mod` 100

-- dial is known to be 0 <= dial < Modulus
countZeros :: Modulus -> Dial -> Rotation -> Int
countZeros m start rot
  | rot >  0 = (start + rot) `div` m - start `div` m
  | rot <  0 = ceilDiv start m - ceilDiv (start + rot) m
  | otherwise = 0
  where
    ceilDiv a b = - ((-a) `div` b)

part1 :: [Rotation] -> Int
part1 = length . filter (==0) . scanl applyRotation dialStart

part2 :: [Rotation] -> Int
part2 rots = sum $ zipWith (countZeros modulus) dials rots
  where
    dials = scanl applyRotation dialStart rots
