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

charNames :: [(Char, String)]
charNames = [ (' ',  "space")
            , ('\n', "newline")
            , ('\t', "tab") ]

showVal :: LispVal -> String
showVal (String str)       = show str
showVal (Atom name)        = name
showVal (Integer int)      = show int
showVal (Real real)        = show real
showVal (Char c)           = "#\\" ++ 
    maybe [c] id (lookup c charNames)
showVal (List list)        = "("  ++ unwordsList list ++ ")"
showVal (DottedList hd tl) = "("  ++ unwordsList hd 
                                  ++ " . " 
                                  ++ showVal tl ++ ")"
showVal (Vector vec)       = "#(" ++ unwordsList (elems vec) ++ ")"
    
instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

rassoc :: Eq b => b -> [(a, b)] -> Maybe a 
rassoc e = lookup e . map (\(x, y) -> (y, x))

character :: Parser LispVal
character = do char '\\'
               cs <- many1 letter <|> liftM (:[]) anyChar
               case rassoc cs charNames of
                 Just c  -> return (Char c)
                 Nothing -> if length cs == 1 
                               then return (Char $ head cs)
                               else fail "bad character literal"
  

parseHash :: Parser LispVal 
parseHash = char '#' >> 
    (base <|> vector <|> bool 't' True <|> bool 'f' False <|> character)
  where bool c v = char c >> return (Bool v)

expressions :: Parser [LispVal]
expressions = sepEndBy parseExpr spaces

inParens :: Parser a -> (a -> LispVal) -> Parser LispVal
inParens prs k = do char '(' 
                    x <- prs
                    char ')'
                    return $ k x      

vector :: Parser LispVal
vector = inParens expressions $ \elts ->
  Vector $ listArray (0, length elts - 1) elts   

spaces :: Parser ()
spaces = skipMany1 space

final :: Parser (Maybe LispVal)
final = option Nothing $ char '.' >> spaces >> liftM Just parseExpr


listWith :: String -> Parser LispVal
listWith car = do x <- parseExpr 
                  return $ List [Atom car, x]

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> listWith "quote"

parseBackquoted :: Parser LispVal
parseBackquoted = char '`' >> listWith "quasiquote"

parseListLike :: Parser LispVal
parseListLike = do char '('
                   front <- expressions
                   back  <- final
                   char ')'
                   return $ case back of 
                     Nothing           -> List front
                     Just (List back') -> List (front ++ back')
                     Just dotted       -> DottedList front dotted

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
  Right val -> "Found "     ++ show val

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
