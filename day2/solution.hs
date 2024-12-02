import GHC.Num

main :: IO ()
main = do
  input <- readFile "day2/input.txt"
  let output1 = solve1 input
   in print output1
  let output2 = solve2 input
   in print output2

solve1 :: String -> Integer
solve1 = toInteger . length . filter isSafe . processInput

solve2 :: String -> Integer
solve2 = toInteger . length . filter (any (True &&)) . map (map isSafe . getDampedReports) . processInput

getDampedReports :: Report -> [Report]
getDampedReports = aux [] []
  where
    aux :: [Report] -> Report -> Report -> [Report]
    aux acc pre post =
      case post of
        [] -> acc
        h:t -> aux ((pre ++ t) : acc) (pre ++ [h]) t

processInput :: String -> [Report]
processInput = map (map read . words) . lines

isSafe :: Report -> Bool
isSafe r =
  (allIncreasing r || allDecreasing r)
    && (maxAbsDiff r <= 3 && minAbsDiff r > 0)

allIncreasing :: Report -> Bool
allIncreasing r = all integerIsNegative $ zipWith (-) (drop 1 r) r

allDecreasing :: Report -> Bool
allDecreasing = allIncreasing . reverse

maxAbsDiff :: Report -> Integer
maxAbsDiff = maximum . pairwiseAbsDiff

minAbsDiff :: Report -> Integer
minAbsDiff = minimum . pairwiseAbsDiff

pairwiseAbsDiff :: Report -> [Integer]
pairwiseAbsDiff r = zipWith absDiff r (drop 1 r)

absDiff :: Integer -> Integer -> Integer
absDiff x y = abs (x - y)

type Report = [Integer]
