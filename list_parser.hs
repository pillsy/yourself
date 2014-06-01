module Main where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe (fromJust)
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
    (base <|> vector <|> bool 't' True <|> bool 'f' False)
  where bool c v = char c >> return (Bool v)  

expressions :: Parser [LispVal]
expressions = sepBy parseExpr spaces

vector :: Parser LispVal
vector = do char '('
            elts <- expressions
            char ')'
            return . Vector $ listArray (0, length elts - 1) elts  

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ expressions

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

listWith :: String -> Parser LispVal
listWith car = do x <- parseExpr 
                  return $ List [Atom car, x]

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> listWith "quote"

parseBackquoted :: Parser LispVal
parseBackquoted = char '`' >> listWith "quasiquote"

parseListLike :: Parser LispVal
parseListLike = do char '('
                   l <- (try parseList) <|> parseDottedList
                   char ')'
                   return l 

parseUnquoted :: Parser LispVal
parseUnquoted = char ',' >> (do char '@'
                                list <- parseListLike
                                return $ List [Atom "unquote-splicing", list] 
                             <|> listWith "unquote")


parseExpr :: Parser LispVal 
parseExpr = parseSign
        <|> parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseHash
        <|> parseQuoted 
        <|> parseBackquoted
        <|> parseUnquoted
        <|> parseListLike     

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
