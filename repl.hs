{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Control.Monad
import Control.Monad.Error
import Data.Array
import Data.IORef
import Data.List
import Data.Maybe (fromMaybe, fromJust)
import System.IO hiding (try)
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

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError -> IOThrowsError a 
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action >>= return . extractValue)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= 
    return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ 
                                UnboundVar "Getting an unbound variable"
                                var)
                             (liftIO . readIORef)
                             (lookup var env)   


charNames :: [(Char, String)]
charNames = [ (' ',  "space")
            , ('\n', "newline")
            , ('\t', "tab") ]

showVal :: LispVal -> String
showVal (Bool bool)        = if bool then "#t" else "#f"
showVal (String str)       = show str
showVal (Atom name)        = name
showVal (Integer int)      = show int
showVal (Real real)        = show real
showVal (Char c)           = "#\\" ++ fromMaybe [c] (lookup c charNames)
showVal (List list)        = "("   ++ unwordsList list ++ ")"
showVal (DottedList hd tl) = "("   ++ unwordsList hd 
                                   ++ " . " 
                                   ++ showVal tl ++ ")"
showVal (Vector vec)       = "#("  ++ unwordsList (elems vec) ++ ")"
 
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
   
instance Show LispVal where show = showVal

data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

showError :: LispError -> String 
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " 
                                        ++ show expected 
                                        ++ " args; found values "
                                        ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " 
                                        ++ expected ++ ", found " 
                                        ++ show found  
showError (Parser err)              = "Parse error at " ++ show err
showError (Default msg)                 = msg

instance Show LispError where show = showError

instance Error LispError where
    noMsg  = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a 
extractValue (Right val) = val
extractValue (Left _)    = undefined

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
                 (_,   _) -> undefined

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
                    else Real    $ unwrap (readSigned readFloat) 
                                 $ int ++ float

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= decimal

readBin :: ReadS Integer
readBin = readInt 2 (`elem` "01") (read . (:[]))

base :: Parser LispVal
base = parser 'd' digit    read
   <|> parser 'x' hexDigit     (unwrap readHex)
   <|> parser 'o' octDigit     (unwrap readOct)
   <|> parser 'b' (oneOf "01") (unwrap readBin)
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
listWith str = do x <- parseExpr 
                  return $ List [Atom str, x]

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val

type LispFunction = [LispVal] -> ThrowsError LispVal

primitives :: [(String, LispFunction)]
primitives = [ ("cons",           cons)
             , ("car",            cdr)
             , ("cdr",            cdr)
             , ("eq?",            eqv)
             , ("eqv?",           eqv)
             , ("equal?",         equal)
             , ("memv",           memv)
             , ("+",              numericBinop (+))
             , ("-",              numericBinop (-))
             , ("*",              numericBinop (*))
             , ("/",              numericBinop div)
             , ("mod",            numericBinop mod)
             , ("quotient",       numericBinop quot)
             , ("remainder",      numericBinop rem) 
             , ("=",              numBoolBinop (==))
             , (">",              numBoolBinop (>))
             , ("<",              numBoolBinop (<))
             , ("/=",             numBoolBinop (/=))
             , (">=",             numBoolBinop (>=))
             , ("<=",             numBoolBinop (<=))
             , ("&&",             boolBoolBinop (&&))
             , ("||",             boolBoolBinop (||))
             , ("string=?",       strBoolBinop (==))
             , ("string>?",       strBoolBinop (>))
             , ("string<?",       strBoolBinop (<))
             , ("string<=?",      strBoolBinop (<=))
             , ("string>=?",      strBoolBinop (>=))
             , ("symbol?",        typePredicate isSymbol)
             , ("string?",        typePredicate isString)
             , ("number?",        typePredicate isNumber)
             , ("integer?",       typePredicate isInteger)
             , ("real?",          typePredicate isReal)
             , ("char?",          typePredicate isChar)
             , ("list?",          typePredicate isList)
             , ("pair?",          typePredicate isPair)
             , ("null?",          typePredicate isNull)
             , ("vector?",        typePredicate isVector)
             , ("symbol->string", symbolOp String)
             , ("make-string",    makeString)
             , ("string",         concatStrings)
             , ("explode",        symbolOp stringToChars)
             , ("string->symbol", stringToSymbol) ]

numArgsError :: Integer -> [LispVal] ->ThrowsError a
numArgsError n = throwError . NumArgs n

same :: Eq a => a -> a -> ThrowsError LispVal
x `same` y = return $ Bool (x == y)

sameList :: LispFunction -> [LispVal] -> [LispVal] -> ThrowsError LispVal
sameList eq xs ys = return . Bool $ 
    length xs == length ys && all eqPair (zip xs ys)
  where eqPair (x', y') = case eq [x', y'] of 
                            Right (Bool b) -> b
                            Left _         -> False

eqv :: LispFunction
eqv [Bool x,    Bool y]    = x `same` y
eqv [Integer x, Integer y] = x `same` y
eqv [String x,  String y]  = x `same` y
eqv [Atom x,    Atom y]    = x `same` y
eqv [List xs, List ys]     = sameList eqv xs ys  
eqv [DottedList xs x, DottedList ys y] = eqv [List $ x:xs, List $ y:ys]
eqv [_ , _] = return $ Bool False
eqv bad = numArgsError 2 bad

type Predicate = LispVal -> Bool

typePredicate :: Predicate -> LispFunction
typePredicate p = fn where 
  fn [x] = return . Bool $ p x
  fn xs  = numArgsError 1 xs

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpack) =
    do x' <- unpack x
       y' <- unpack y
       return $ x' == y'
    `catchError` const (return False)   

equal :: LispFunction
equal [DottedList xs x, DottedList ys y] = equal [List (x:xs), List (y:ys)]
equal [List xs, List ys] = sameList equal xs ys
equal [x, y] = do
    equals' <- liftM or $ mapM (unpackEquals x y)
      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqv'    <- eqv [x, y]
    return $ Bool (equals' || let (Bool e) = eqv' in e)

isSymbol :: Predicate
isSymbol x = case x of (Atom _) -> True; _ -> False

isString :: Predicate
isString x = case x of (String _) -> True; _ -> False

isReal :: Predicate
isReal x = case x of (Real _) -> True; _ -> False

isInteger :: Predicate
isInteger x = case x of (Integer _) -> True; _ -> False

isChar :: Predicate
isChar x = case x of (Char _) -> True; _ -> False

isList :: Predicate
isList x = case x of (List _) -> True; _ -> False

isPair :: Predicate 
isPair x = case x of List (_:_)     -> True
                     DottedList _ _ -> True
                     _              -> False

isVector :: Predicate
isVector x = case x of (Vector _) -> True; _ -> False

isNumber :: Predicate
isNumber x = isReal x || isInteger x

isNull :: Predicate
isNull x = case x of List [] -> True; _ -> False

typeMismatchError :: String -> LispVal -> ThrowsError a 
typeMismatchError ty bad = throwError $ TypeMismatch ty bad

wrongOne :: String -> [LispVal] -> ThrowsError a
wrongOne typeName [x] = typeMismatchError typeName x
wrongOne _        xs  = numArgsError 1 xs

car :: LispFunction 
car [List (x: _)]          = return x
car [DottedList (x : _) _] = return x
car bad                    = wrongOne "pair" bad

cdr :: LispFunction 
cdr [List (_: xs)]          = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [] x]       = return x
cdr bad                     = wrongOne "pair"  bad

cons :: LispFunction
cons [x, y] = return $ 
  case y of
    List       xs    -> List (x:xs)
    DottedList xs y' -> DottedList (x:xs) y'
    _                -> DottedList [x]    y
cons bad    = numArgsError 2 bad

symbolName :: [LispVal] -> ThrowsError String
symbolName [Atom name] = return name
symbolName xs          = wrongOne "symbol" xs

symbolOp :: (String -> LispVal) -> LispFunction
symbolOp f xs = liftM f $ symbolName xs 

stringToChars :: String -> LispVal
stringToChars = List . map Char

stringToSymbol :: LispFunction
stringToSymbol [String s] = return $ Atom s
stringToSymbol xs         = wrongOne "string" xs

makeString :: LispFunction
makeString [Integer n, Char c] 
  | 0 <= n    = return . String $ genericTake n $ repeat c
  | otherwise = typeMismatchError "positive number" $ Integer n
makeString bad@[_, _] = typeMismatchError "integer and char" $ List bad
makeString bad        = numArgsError 2 bad

concatStrings :: LispFunction
concatStrings ss = liftM (String . concat) $ mapM unpackStr ss

charsToString :: LispFunction
charsToString cs = liftM String $ mapM unpackChar cs

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar bad      = typeMismatchError "char" bad

unpackNum :: LispVal -> ThrowsError Integer 
unpackNum (Integer n) = return n
unpackNum (String n)  = let parsed = reads n in
                        if null parsed
                           then throwError $ TypeMismatch "number" $ String n
                           else return . fst . head $ parsed    
unpackNum (List [n])  = unpackNum n
unpackNum bad         = typeMismatchError "number" bad

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)  = return s
unpackStr (Integer s) = return $ show s
unpackStr (Real s)    = return $ show s
unpackStr b@(Bool _)  = return $ show b
unpackStr (Atom s)    = return s 
unpackStr (Char c)    = return [c]
unpackStr bad         = typeMismatchError "string" bad

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool bad      = typeMismatchError "boolean" bad

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> LispFunction
boolBinop unpack op args =
   if length args /= 2 
      then numArgsError 2 args
      else do [left, right] <- mapM unpack args
              return . Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum 
strBoolBinop  = boolBinop unpackStr 
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> LispFunction
numericBinop _  args | length args < 2 = numArgsError 2 args
numericBinop op args = liftM (Integer . foldl1 op) $ mapM unpackNum args 

apply :: String -> LispFunction
apply fn args = maybe (throwError $ NotFunction
                        "Unrecognized primitive function" fn) 
                      ($ args) 
                      (lookup fn primitives)

chain :: LispFunction -> LispFunction -> LispFunction
chain f g = \xs -> do y <- g xs 
                      f [y] 

unspecified :: LispVal
unspecified = Atom "<unspecified>"

ifM :: ThrowsError LispVal -> ThrowsError LispVal
                           -> ThrowsError LispVal
                           -> ThrowsError LispVal
ifM pred cons alt = do pred' <- pred 
                       case pred' of 
                         Bool False -> alt
                         _          -> cons

memv :: LispFunction
memv [x, List ys] = go ys
  where go []         = return $ List []
        go ys@(y:ys') = ifM (eqv [x, y]) 
                            (return $ List ys) 
                            (go ys')
memv [x, bad]     = typeMismatchError "list" bad
memv bad          = numArgsError 2 bad

true :: ThrowsError LispVal
true = return $ Bool True

false :: ThrowsError LispVal
false = return $ Bool False

badSpecialFormError :: String -> LispVal -> ThrowsError a
badSpecialFormError msg bad = throwError $ 
    BadSpecialForm ("Invalid " ++ msg) bad

cond :: [LispVal] -> ThrowsError LispVal
cond [] = return unspecified
cond [List [Atom "else", alt]]           = eval alt
cond (List [pred, conseq] : alts)        = 
     ifM (eval pred) (eval conseq) (cond alts)
cond (List [pred, Atom "=>", fn] : alts) = do
     pred' <- eval pred
     ifM (return pred') (eval $ List [fn, pred']) (cond alts) 
cond bad = badSpecialFormError "cond clause" (List bad)

case' :: [LispVal] -> ThrowsError LispVal
case' [_] = return unspecified
case' [_, List [Atom "else", alt]] = eval alt
case' (x : List[ys@(List _), conseq] : alts) = 
    ifM (typePredicate isPair `chain` memv $ [x, ys]) 
        (eval conseq) 
        (case' (x : alts))        
case' bad = badSpecialFormError "case clause" (List bad)

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool _)                = return val
eval val@(String _)              = return val
eval val@(Integer _)             = return val
eval val@(Real _)                = return val
eval val@(Vector _)              = return val
eval val@(Char _)                = return val
eval (List [Atom "quote", val])  = return val
eval (List (Atom "cond" : opts)) = cond opts
eval (List [Atom "if", pred, conseq, alt]) =
    ifM (eval pred) (eval conseq) (eval alt)
eval (List [Atom "if", pred, conseq]) = 
    ifM (eval pred) (eval conseq) (return unspecified)
eval (List (Atom "case" : clauses)) = case' clauses  
eval (List (Atom func : args))  = mapM eval args >>= apply func 
eval bad                        = throwError $ 
    BadSpecialForm "Unrecognized special form" bad

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue 
                         $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp > ") evalAndPrint

main :: IO ()
main = do args   <- getArgs
          case length args of 
            0 -> runRepl
            1 -> evalAndPrint $ head args
            _ -> hPutStrLn stderr "out of coffee!"
