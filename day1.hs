import Data.Char -- for isSpace
import Data.List -- for dropWhileEnd

main :: IO ()
main = do
  input <- readFile "input1.txt"
  let output = solve input
   in putStrLn $ show output

solve :: String -> Integer
solve input =
  sum
    $ absDiffColumns
    $ sortColumns
    $ toColumns
    $ map toIntegers
    $ map toSplitLine
    $ filter isNotEmpty
    $ toLines input

rev :: [a] -> [a]
rev lst = aux [] lst
  where
    aux :: [a] -> [a] -> [a]
    aux acc l =
      case l of
        [] -> acc
        h:t -> aux (h : acc) t

getNextLine :: String -> (String, String)
getNextLine str = aux [] str
  where
    aux :: String -> String -> (String, String)
    aux acc s =
      case s of
        [] -> (rev acc, [])
        h:t ->
          if h == '\n'
            then (rev acc, t)
            else aux (h : acc) t

toLines :: String -> [String]
toLines str = aux [] str
  where
    aux :: [String] -> String -> [String]
    aux acc s =
      let (next, rest) = getNextLine s
       in if rest == []
            then rev (map strip (next : acc))
            else aux (next : acc) rest

isNotEmpty :: String -> Bool
isNotEmpty str = str /= []

toSplitLine :: String -> (String, String)
toSplitLine str = aux [] str
  where
    aux :: String -> String -> (String, String)
    aux acc s =
      case s of
        [] -> (strip (rev acc), [])
        h:t ->
          if (isSpace h)
            then (strip (rev acc), strip t)
            else aux (h : acc) t

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

toIntegers :: (String, String) -> (Integer, Integer)
toIntegers (left, right) = (read left, read right)

toColumns :: [(a, a)] -> ([a], [a])
toColumns input = aux [] [] input
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
