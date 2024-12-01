main :: IO ()
main = do
  input <- readFile "example1.txt"
  let output = toLines input
   in putStrLn $ show output

solve :: String -> Integer
solve input = 7

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
            then rev (next : acc)
            else aux (next : acc) rest

