main :: IO ()
main = do
  input <- readFile "day4/example.txt"
  let output1 = solve1 input
   in print output1

solve1 :: String -> Integer
solve1 x = 42

data Puzzle = Puzzle
  { rows :: [String]
  , cols :: [String]
  , diagonals :: [String]
  } deriving (Show, Eq)

processInput :: String -> Puzzle
processInput input =
  let l = lines input
   in Puzzle {rows = l, cols = toCols l, diagonals = l}

toCols :: [String] -> [String]
toCols l = map (\x -> map (!! x) l) [0 .. length (head l) - 1]

toDiags l = undefined

cycleElements :: Int -> [a] -> [a]
cycleElements n lst = (drop n lst) ++ (take n lst)
