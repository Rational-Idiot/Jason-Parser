module Main (main) where

data JSONval
  = JSONNull
  | JSONBooL Bool
  | JSONNumber Integer
  | JSONString String
  | JSONArray [JSONval]
  | JSONObject [(String, JSONval)]
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonValue :: Parser JSONval
jsonValue = undefined

main :: IO ()
main = undefined
