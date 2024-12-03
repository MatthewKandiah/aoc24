import Control.Applicative
import Data.Char

main :: IO ()
main = do
  input <- readFile "day3/input.txt"
  let output1 = solve1 input
   in print output1

solve1 :: String -> Integer
solve1 = sum . map evaluate . parseInput

parseInput :: String -> [Value]
parseInput = aux []
  where
    aux acc input =
      case parsedValue of
        Nothing ->
          case input of
            [] -> reverse acc
            _:t -> aux acc t
        Just (rest, token) -> aux (token : acc) rest
      where
        parsedValue = runParser expressionP input

evaluate :: Value -> Integer
-- the fact this blows up for anything but value is probably a sign that Expr should probably be a separate type
evaluate (Expr (x, y)) = x * y

data Value
  = Mul
  | OpenParen
  | CloseParen
  | Comma
  | Num Integer
  | Expr (Integer, Integer) -- represents valid mul(X,Y) expression with nothing else in it
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

expressionP = mulP *> openParenP *> pair <* closeParenP
  where
    pair =
      (\(Num left) _ (Num right) -> Expr (left, right))
        <$> numberP
        <*> commaP
        <*> numberP
