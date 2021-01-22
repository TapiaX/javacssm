module Scanner  where

import Data.Char (isSpace,isAlpha,isDigit,isAlphaNum)

data Symbol = Symbol Category String

instance Show Symbol where
  show (Symbol c s) = show c ++ " " ++ show s

data Category = 
   ReservedWord           --a few principal words
 | Separator 
 | Operator               --operators of 1 or more characters
 | IntegerLiteral         --only decimal integers (whitout underscores)
 | BooleanLiteral        
 | Identifier
 deriving (Show,Eq)

instance Eq Symbol where
  (Symbol ReservedWord   pl) == (Symbol ReservedWord   pr) = pl == pr
  (Symbol Separator      sl) == (Symbol Separator      sr) = sl == sr
  (Symbol Identifier     il) == (Symbol Identifier     ir) = True
  (Symbol Operator       ol) == (Symbol Operator       or) = ol == "" || ol == or
  (Symbol IntegerLiteral el) == (Symbol IntegerLiteral er) = True
  (Symbol BooleanLiteral el) == (Symbol BooleanLiteral er) = True
  _                          == _                          = False

scan :: String -> [ Symbol ]
scan []  =  []
scan ccs@(c:cs)
  | isSpace c             = scan (dropWhile isSpace cs)
  | isAlpha c || c == '_' = let (word,rest) = span isJavaLetter ccs
                            in  (if isReservedWord word
                                  then Symbol ReservedWord word
                                  else if isBooleanLit word
                                        then Symbol BooleanLiteral word
                                        else Symbol Identifier     word ):scan rest
  | isDigit  c            = let (word,rest) = span isDigit ccs in (Symbol IntegerLiteral word):scan rest
  | isSeparator c         = (Symbol Separator (c:[])):scan cs
  | isOperator c          = let (op,rest) = span isOperator ccs in (verifyOperator op)++scan rest
  | otherwise             = error ("Invalid symbol: "++[c])

isJavaLetter l = isAlphaNum l || l == '_'
isSeparator s = s `elem` separators
isReservedWord w = w `elem` reservedWords
isOperator o = o `elem` operators
isBooleanLit b = b `elem` booleanLits

verifyOperator [] = []
verifyOperator ops@(x:xs) = if ops `elem` operators2 then [Symbol Operator ops]
                                else (Symbol Operator [x]):verifyOperator xs 

separators = "(){}[];,."
reservedWords = [ "boolean", "class", "do", "else", "for", "if", "int",
                       "private", "public", "return", "void", "while" ,"_"]
operators = "=+>-<*!/~&?|:^%"
operators2 = ["=",">","<","!","~","?",":","->",
              "==",">=","<=","!=","&&","||","++","--",
              "+","-","*","/","&","|","^","%","<<",">>",">>>",
              "+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=",">>>="]
booleanLits = ["true","false"]

