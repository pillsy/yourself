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
             | Integer Integer
             | Real Double
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

atomTail :: Parser String
atomTail = many (letter <|> digit <|> symbol)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- atomTail
               let atom = first : rest
               return $ Atom atom
               
intParser :: Parser Char -> (String -> Integer) -> Parser LispVal
intParser dig rd = liftM (Integer . rd) (many1 dig)

sign :: Parser Char
sign = oneOf "+-"

parseSign :: Parser LispVal
parseSign = do s    <- sign 
               digs <- many digit
               case (s, digs) of 
                 (_,  "") -> liftM (Atom . (s:)) atomTail 
                 ('+', _) -> decimal digs
                 ('-', _) -> decimal $ s : digs

unwrap :: (a -> [(b, c)]) -> a -> b
unwrap f = fst . head . f

decimal :: String -> Parser LispVal
decimal int = do frac <- option "" $ do
                   pt   <- char '.'
                   digs <- many digit
                   return $ pt : digs
                 expt <- option "" $ do
                   ex   <- oneOf "efgEFG"
                   s    <- option "" $ liftM (:[]) sign
                   digs <- many1 digit
                   return $ ex : (s ++ digs)
                 let float = frac ++ expt  
                 return $ if null float 
                    then Integer $ read int
                    else Real    $ fst . head . readFloat $ int ++ float

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= decimal

readBin :: String -> Integer
readBin = foldl' (\r b -> 2 * r + bit b) 0
  where bit b = case b of '0' -> 0
                          '1' -> 1

base :: Parser LispVal
base = parser 'd' digit    read
   <|> parser 'x' hexDigit (unwrap readHex)
   <|> parser 'o' octDigit (unwrap readOct)
   <|> parser 'b' (oneOf "01") readBin
  where parser p d r = char p >> intParser d r 

parseHash :: Parser LispVal 
parseHash = char '#' >> 
    (base <|> bool 't' True <|> bool 'f' False)
  where bool c v = char c >> return (Bool v)  

parseExpr :: Parser LispVal 
parseExpr = parseSign
        <|> parseAtom
        <|> parseString
        <|> parseNumber
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
