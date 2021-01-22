module AST where

sem_Program_Class = Class
sem_MembersList_Cons = (:)
sem_MembersList_Nil = []
sem_Member_Atribute = Atribute
sem_Member_Method = Method
sem_Type_IntegerType = IntegerType
sem_Type_BooleanType = BooleanType
sem_Type_VoidType = VoidType
sem_ParametersList_Cons = (:)
sem_ParametersList_Nil = []
sem_Parameter_Parameter = Parameter
sem_InstructionsList_Cons = (:)
sem_InstructionsList_Nil = []
sem_Instruction_Asignement = Asignement
sem_Instruction_Conditional = Conditional
sem_Instruction_ConditionalCycle = ConditionalCycle
sem_Instruction_Declaration = Declaration
sem_Instruction_MethodCall = MethodCall
sem_Instruction_Return = Return
sem_LocalParams_Cons = (:)
sem_LocalParams_Nil = []
sem_Expression_LInteger =LInteger 
sem_Expression_LBoolean = LBoolean
sem_Expression_Variable = Variable
sem_Expression_Operation =  Operation 
sem_Expression_ResultCall = ResultCall
sem_Op = id

-- Types para el arbol de derivacion

data Program = Class String MembersList
              deriving Show

type MembersList = [Member]

data Member = Atribute Type String
             | Method Type String ParametersList InstructionsList
             deriving Show

data Type = IntegerType
          | BooleanType
          | VoidType
          deriving Show

type ParametersList = [Parameter]

data Parameter = Parameter Type String
               deriving Show

type InstructionsList = [Instruction]

data Instruction = Asignement String Expression
                 | Conditional Expression InstructionsList InstructionsList
                 | ConditionalCycle Expression InstructionsList
                 | Declaration Type String
                 | MethodCall String LocalParams
                 | Return Expression
                 deriving  Show

type LocalParams = [Expression]

data Expression  = LInteger String
                | LBoolean String
                | Variable String
                | Operation Op Expression Expression
                | ResultCall String LocalParams
                deriving Show

data Op = Or
        | And
        | Equal
        | NotEqual
        | Less
        | Greater
        | LessEqual
        | GreaterEqual
        | Add
        | Sub
        | Product
        | Division
        | Module
        deriving ( Show)



-- for compatibility in place of AnalisisContexto
type T_Program = Program
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {ast_Syn_Program :: Program,
                                  errorProgram_Syn_Program :: String,
                                  messages_Syn_Program :: ([String]),
                                  output_Syn_Program :: String}
wrap_Program :: T_Program -> Inh_Program -> Syn_Program
wrap_Program sem (Inh_Program) = let _lhsOast = sem
                                   in  (Syn_Program _lhsOast "No code for show" ["AST.hs only make the tree"] "nop")

type T_LocalParams = LocalParams