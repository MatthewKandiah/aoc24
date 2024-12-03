import Control.Applicative
import Data.Char

main :: IO ()
main = do
  input <- readFile "day3/input.txt"
  let output1 = solve1 input
   in print output1
  let output2 = solve2 input
   in print output2

solve1 :: String -> Integer
solve1 = sum . map evaluate . parseInput1

parseInput :: Parser Value -> String -> [Value]
parseInput p = aux []
  where
    aux acc input =
      case parsedValue of
        Nothing ->
          case input of
            [] -> reverse acc
            _:t -> aux acc t
        Just (rest, token) -> aux (token : acc) rest
      where
        parsedValue = runParser p input

parseInput1 = parseInput productP

evaluate :: Value -> Integer
-- the fact this blows up for anything but value is probably a sign that Product should probably be a separate type
evaluate (Product (x, y)) = x * y

data Value
  = Mul
  | OpenParen
  | CloseParen
  | Comma
  | Num Integer
  | Product (Integer, Integer) -- represents valid mul(X,Y) expression with nothing else in it
  | Do
  | Dont
  deriving (Show, Eq)

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

mulP :: Parser Value
mulP = (\_ -> Mul) <$> stringP "mul"

openParenP :: Parser Value
openParenP = (\_ -> OpenParen) <$> charP '('

closeParenP :: Parser Value
closeParenP = (\_ -> CloseParen) <$> charP ')'

commaP :: Parser Value
commaP = (\_ -> Comma) <$> charP ','

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token) -- NOTE: can't return Nothing if token == "" because we'll use this to skip over meaningless characters, and we'll need the rest value

-- useful with spanP to actually try to parse multicharacter token
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

numberP :: Parser Value
numberP = Num . read <$> notNull (spanP isDigit)

productP :: Parser Value
productP = mulP *> openParenP *> pair <* closeParenP
  where
    pair =
      (\(Num left) _ (Num right) -> Product (left, right))
        <$> numberP
        <*> commaP
        <*> numberP

-- Part 2
doP = (\_ -> Do) <$> stringP "do()"

dontP = (\_ -> Dont) <$> stringP "don't()"

expressionP :: Parser Value
expressionP = productP <|> doP <|> dontP

parseInput2 :: String -> [Value]
parseInput2 = parseInput expressionP

evaluate2 :: [Value] -> Integer
evaluate2 = aux 0 True
  where
    aux :: Integer -> Bool -> [Value] -> Integer
    aux acc enabled lst =
      case lst of
        [] -> acc
        Do:t -> aux acc True t
        Dont:t -> aux acc False t
        Product (x, y):t ->
          if enabled
            then aux (acc + x * y) enabled t
            else aux acc enabled t

solve2 :: String -> Integer
solve2 = evaluate2 . parseInput2
