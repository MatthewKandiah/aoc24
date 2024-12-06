import Data.List
import Control.Applicative

main :: IO ()
main = do
  input <- readFile "day4/example.txt"
  let output1 = solve1 input
   in print output1
  let output2 = solve2 input
   in print output2

solve1 :: String -> Integer
solve1 x = sum $ (map countXmas) $ (rows puzzle ++ cols puzzle ++ diagonals puzzle)
  where puzzle = processInput x

countXmas :: String -> Integer
countXmas = aux 0
  where
    aux count str = case parsedValue of
      Nothing -> case str of
        [] -> count
        _:t -> aux count t
      Just _ -> aux (count + 1) (tail str)
      where
        parsedValue = runParser parser str

xmasParser = stringP "XMAS"
samxParser = stringP "SAMX"
parser = xmasParser <|> samxParser

--------------------------------------------------COPY-PASTE from DAY3----------------------------------------------------------
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- we need to define these properties for our Parser or we can't use the operators we need later
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \y -> Just (y, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

--------------------------------------------------END COPY-PASTE from DAY3------------------------------------------------------

data Puzzle = Puzzle
  { rows :: [String]
  , cols :: [String]
  , diagonals :: [String]
  } deriving (Show, Eq)

processInput :: String -> Puzzle
processInput input =
  let l = lines input
   in Puzzle {rows = l, cols = toCols l, diagonals = toDiags l}

toCols :: [String] -> [String]
toCols l = map (\x -> map (!! x) l) [0 .. length (head l) - 1]

toDiags :: [String] -> [String]
toDiags ls = (toDiags1 ls) ++ (toDiags2 ls)

toDiags1 :: [String] -> [String]
toDiags1 =
  map concat
    . transpose
    . zipWith (\ns xs -> ns ++ map (: []) xs) (iterate ([] :) [])

toDiags2 :: [String] -> [String]
toDiags2 = toDiags1 . (map reverse)

solve2 :: String -> Integer
solve2 _ = 42

