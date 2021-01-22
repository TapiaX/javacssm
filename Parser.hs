module Parser where

import Prelude hiding ( (<*>), (<$>), (*>), (<*), (<$) )

import Scanner (Symbol (..),Category (..))
import ParsingCombinators
import ContextAnalisis
--import AST

--parse :: [Symbol] -> Program
parse p = if null lrs then error "Sintax error" else fst (head lrs)
  where lrs  = parseProgram p

-- parseProgram :: Parser Symbol Program
parseProgram = parseClass

parseClass = sem_Program_Class <$ parseReservedWord "class" <*> parseIdentifier <*> parseBodyClass

parseBodyClass  = parseSeparator "{" *>  parseMembers <* parseSeparator "}"

--parseMembers  :: Parser Symbol [Miembro]
parseMembers  =   pFoldr sem_MembersList_Cons sem_MembersList_Nil parseMembre

--parseMembre :: Parser Symbol Miembro
parseMembre = parseAtribute <|> parseMethod

--parseAtribute :: Parser Symbol Miembro
parseAtribute = sem_Member_Atribute <$> parseType <*> parseIdentifier <* parseSeparator ";"



--parseType :: Parser Symbol Tipo
parseType =   sem_Type_IntegerType   <$ parseReservedWord "int"
          <|> sem_Type_VoidType    <$ parseReservedWord "void"
          <|> sem_Type_BooleanType <$ parseReservedWord "boolean"

--parseMethod :: Parser Symbol Miembro
parseMethod =  sem_Member_Method <$> parseType <*> parseIdentifier <*> parseParameters <*> parseBlock

parenthesised p = parseSeparator "(" *> p <* parseSeparator ")"

--parseParameters :: Parser Symbol [Parametro]
parseParameters = parenthesised parseParametersList
               

parseParametersList =  pListOfFoldr sem_ParametersList_Cons
                                     sem_ParametersList_Nil
                                     parseParameter 
                                     (parseSeparator ",")

        
parseParameter = sem_Parameter_Parameter <$> parseType <*> parseIdentifier

--parseCuerpoMetodo :: Parser Symbol [Instruccion]
parseBlock = parseSeparator "{" *> parseInstructions <* parseSeparator "}"

--parseInstructions :: Parser Symbol [Instruccion]
parseInstructions = pFoldr sem_InstructionsList_Cons 
                            sem_InstructionsList_Nil  
                            parseInstruction

--parseInstruction :: Parser Symbol Instruccion
parseInstruction =  parseAsignement
                <|> parseConditional
                <|> parseConditionalCycle
                <|> parseDeclaration
                <|> parseMethodCall
                <|> parseReturn

--parseAsignement :: Parser Symbol Instruccion
parseAsignement  = sem_Instruction_Asignement <$> parseIdentifier <* parseOperator "=" 
                                              <*> parseExpression <* parseSeparator ";"

parseConditional = sem_Instruction_Conditional <$> parseReservedWord "if" *> parseCondition 
                                               <*> parseBlock <*> option parseConditionalElse
                                                                          sem_InstructionsList_Nil

parseCondition = parenthesised parseExpression

parseConditionalElse = parseReservedWord "else" *> parseBlock

parseConditionalCycle = sem_Instruction_ConditionalCycle <$> parseReservedWord "while" 
                                                          *> parseCondition <*> parseBlock

parseDeclaration = sem_Instruction_Declaration <$> parseType <*> parseIdentifier <* parseSeparator ";"

parseMethodCall = parseCall sem_Instruction_MethodCall <* parseSeparator ";"     

parseCall :: (String -> T_LocalParams -> a) -> Parser Symbol a
parseCall  f = f <$> parseIdentifier <*> parenthesised parseLocalParams  

parseLocalParams = pListOfFoldr sem_LocalParams_Cons 
                              sem_LocalParams_Nil 
                              parseExpression 
                              (parseSeparator ",")

parseReturn = sem_Instruction_Return <$> parseReservedWord "return" *> parseExpression <* parseSeparator ";"

-- para expresiones con precedencia de Operatores
parseExpression = expr
--expr :: Parser Symbol Expression
expr = foldr gen fact [ors,ands,equals,comps,adds,mults]

type StringOp = (String,Op)

ors  = [("||",Or)]
ands = [("&&",And)]
equals = [("==",Equal),("!=",NotEqual)]
comps = [("<",Less),(">",Greater),("<=",LessEqual),(">=",GreaterEqual)]
mults = [ ("*",Product), ("/",Division),("%",Module)]
adds = [ ("+",Add), ("-",Sub)]


--fact :: Parser Symbol Expression
fact =  sem_Expression_LInteger <$> pparseIntegerLiteral
    <|> sem_Expression_LBoolean <$> parseBooleanLiteral
    <|> sem_Expression_Variable <$> parseIdentifier  
    <|> parseCall sem_Expression_ResultCall

--gen :: [StringOp] -> Parser Symbol a -> Parser Symbol a
gen ops p = chainr p (choice (map f ops))
  where f (s,c) = sem_Expression_Operation (sem_Op c) <$ parseOperator s

-- Parsers basicos

parseReservedWord :: String -> Parser Symbol String
parseReservedWord reservedWord = extract <$> parseSymbol ReservedWord reservedWord
  
parseIdentifier :: Parser Symbol String
parseIdentifier = extract <$> parseSymbol Identifier ""

parseSeparator :: String -> Parser Symbol String
parseSeparator separator = extract <$> parseSymbol Separator separator

parseOperator :: String -> Parser Symbol String
parseOperator operator = extract <$> parseSymbol Operator operator

pparseIntegerLiteral :: Parser Symbol String
pparseIntegerLiteral = extract <$> parseSymbol IntegerLiteral ""

parseBooleanLiteral :: Parser Symbol String
parseBooleanLiteral = extract <$> parseSymbol BooleanLiteral ""

parseSymbol :: Category -> String -> Parser Symbol Symbol
parseSymbol c s = symbol (Symbol c s)

extract  :: Symbol -> String
extract (Symbol _ s) = s


