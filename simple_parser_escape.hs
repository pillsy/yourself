
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
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
                 x <- many ((noneOf "\\\"") <|> escape)
                 char '"'
                 return $ String x 

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseDigits :: Parser LispVal
parseDigits = many1 digit >>= (return . Number . read)

parseNumber :: Parser LispVal
parseNumber = parseDigits 

parseExpr :: Parser LispVal 
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
