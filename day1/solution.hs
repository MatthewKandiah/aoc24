import Data.Char -- for isSpace
import Data.List -- for dropWhileEnd

main :: IO ()
main = do
  input <- readFile "day1/input.txt"
  putStrLn "Part 1:"
  let output1 = solve1 input
   in print output1
  putStrLn "Part 2:"
  let output2 = solve2 input
   in print output2

processInput :: String -> ([Integer], [Integer])
processInput input =
  toColumns $ map (toIntegers . toSplitLine) (filter isNotEmpty $ toLines input)

solve1 :: String -> Integer
solve1 input = sum $ absDiffColumns $ sortColumns $ processInput input

solve2 :: String -> Integer
solve2 input = sum $ getSimilarityScores $ processInput input

getSimilarityScores :: ([Integer], [Integer]) -> [Integer]
getSimilarityScores (l, r) = map (getSimilarityScore r) l

getSimilarityScore :: [Integer] -> Integer -> Integer
getSimilarityScore lst n = count lst n * n

count :: Eq a => [a] -> a -> Integer
count lst x = aux 0 lst
  where
    aux acc l =
      case l of
        [] -> acc
        h:t ->
          if h == x
            then aux (acc + 1) t
            else aux acc t

rev :: [a] -> [a]
rev = aux []
  where
    aux :: [a] -> [a] -> [a]
    aux acc l =
      case l of
        [] -> acc
        h:t -> aux (h : acc) t

isNotEmpty :: String -> Bool
isNotEmpty str = str /= []

toSplitLine :: String -> (String, String)
toSplitLine = aux []
  where
    aux :: String -> String -> (String, String)
    aux acc s =
      case s of
        [] -> (strip (rev acc), [])
        h:t ->
          if isSpace h
            then (strip (rev acc), strip t)
            else aux (h : acc) t

toIntegers :: (String, String) -> (Integer, Integer)
toIntegers (left, right) = (read left, read right)

toColumns :: [(a, a)] -> ([a], [a])
toColumns = aux [] []
  where
    aux leftAcc rightAcc lrList =
      case lrList of
        [] -> (rev leftAcc, rev rightAcc)
        (lh, rh):t -> aux (lh : leftAcc) (rh : rightAcc) t

sortColumns :: Ord a => ([a], [a]) -> ([a], [a])
sortColumns (left, right) = (sort left, sort right)

absDiffColumns :: Num a => ([a], [a]) -> [a]
absDiffColumns (l, r) = zipWith absoluteDifference l r
  where
    absoluteDifference x y = abs $ x - y

getNextLine :: String -> (String, String)
getNextLine = aux []
  where
    aux :: String -> String -> (String, String)
    aux acc s =
      case s of
        [] -> (reverse acc, [])
        h:t ->
          if h == '\n'
            then (reverse acc, t)
            else aux (h : acc) t

toLines :: String -> [String]
toLines = aux []
  where
    aux :: [String] -> String -> [String]
    aux acc s =
      let (next, rest) = getNextLine s
       in if null rest
            then reverse (map strip (next : acc))
            else aux (next : acc) rest

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

