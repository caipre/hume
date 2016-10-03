module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input =
   case parse (spaces >> parseExpr) "lisp" input of
     Left err -> String $ "No match: " ++ show err
     Right val -> val

spaces :: Parser ()
spaces = skipMany space

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

parseString :: Parser LispVal
parseString = do
   char '"'
   str <- many (noneOf "\"")
   char '"'
   return $ String str

parseAtom :: Parser LispVal
parseAtom = do
   first <- letter <|> symbol
   rest <- many (letter <|> digit <|> symbol)
   let atom = first:rest
   return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                l <- try parseList <|> parseDottedList
                char ')'
                return l

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("rem", numericBinop rem)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
   let parsed = reads n :: [(Integer, String)] in
       if null parsed
          then 0
          else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom fn : args)) = apply fn $ map eval args

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
