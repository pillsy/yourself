module Main where

import Control.Monad
import Data.List
import Data.Maybe (fromJust)
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Char Char
             | String String
             | Bool Bool
             deriving Show

charToChar :: [(Char, Char)] -> Parser Char
charToChar alist = do
    c <- oneOf $ map fst alist
    return $ fromJust $ lookup c alist

escape :: Parser Char
escape = char '\\' >> charToChar [ ('\\', '\\')
                                 , ('\"', '\"')
                                 , ('t',  '\t')
                                 , ('b',  '\b')
                                 , ('r',  '\r')
                                 , ('n',  '\n')]

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ noneOf "\\\"" <|> escape
                 char '"'
                 return $ String x 

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ Atom atom
               
intParser :: Parser Char -> (String -> Integer) -> Parser LispVal
intParser dig rd = liftM (Number . rd) (many1 dig)

parseDecimal :: Parser LispVal
parseDecimal = intParser digit read 

readBin :: String -> Integer
readBin = foldl' (\r b -> 2 * r + bit b) 0
  where bit b = case b of '0' -> 0
                          '1' -> 1

base :: Parser LispVal
base = (oneOf "de" >> parseDecimal) 
   <|> parser 'x' hexDigit (unwrap readHex)
   <|> parser 'o' octDigit (unwrap readOct)
   <|> parser 'b' (oneOf "01") readBin
  where parser p d r = char p >> intParser d r 
        unwrap f = fst . head . f

parseHash :: Parser LispVal 
parseHash = char '#' >> 
    (base <|> bool 't' True <|> bool 'f' False)
  where bool c v = char c >> return (Bool v)  

parseExpr :: Parser LispVal 
parseExpr = parseAtom
        <|> parseString
        <|> parseDecimal
        <|> parseHash

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
