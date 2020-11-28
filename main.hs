module Main where
import Control.Monad  
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val 

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = do
                x <- many1 digit
                return $ Number $ read x
-- | parseNumber = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do
             x <- many1 digit
             char '.'
             y <- many1 digit
             let a = (x ++ "." ++ y)
             return $ Float $ read a

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseFloat
         <|> parseNumber

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

