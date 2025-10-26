module Main (main) where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP (string)

data JSONval
  = JSONNull
  | JSONBool Bool
  | JSONNumber Integer
  | JSONString String
  | JSONArray [JSONval]
  | JSONObject [(String, JSONval)]
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

jsonNull :: Parser JSONval
jsonNull = (\_ -> JSONNull) <$> stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure p = Parser $ \input -> Just (input, p)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notnull :: Parser [a] -> Parser [a]
notnull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JSONval
jsonNumber = f <$> notnull (spanP isDigit)
  where
    f digits = JSONNumber $ read digits

jsonString :: Parser JSONval
jsonString = JSONString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []

jsonArray :: Parser JSONval
jsonArray = JSONArray <$> (charP '[' *> ws *> element <* ws <* charP ']')
  where
    element = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JSONval
jsonObject = JSONObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' *> ws) <*> jsonValue

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonBool :: Parser JSONval
jsonBool = f <$> ((stringP "true") <|> (stringP "false"))
  where
    f "true" = JSONBool True
    f "false" = JSONBool False
    f _ = undefined

jsonValue :: Parser JSONval
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile file parser = do
  input <- readFile file
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
