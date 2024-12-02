module Utils where

import Data.Char -- for isSpace
import Data.List -- for dropWhileEnd

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
