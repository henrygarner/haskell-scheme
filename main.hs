module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

escapedChars :: Parser Char
escapedChars = do char '\\' -- a backslash
                  x <- oneOf "\\\"" -- either backslash or doublequote
                  return x -- return the escaped character

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
