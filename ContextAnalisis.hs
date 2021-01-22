

-- UUAGC 0.9.52.2 (ContextAnalisis.ag)
module ContextAnalisis where

import Data.List (elemIndex,(\\)) 
import Data.Maybe (isJust,fromJust)

{-# LINE 528 "ContextAnalisis.ag" #-}
 

--revision de uso de variables declarateds before o despues
messageUse :: String -> [String] -> [[String]] -> [String] 
messageUse id all local = if [] /= (dropWhile (not.(id `elem`)) local) || id `elem` all 
                             then [] {-["encontado en "++ show (map (id `elem`) local)]-}
                             else ["'"++id++"' used without declaration"]

messageDecl :: String -> [[String]] -> [String]
messageDecl id before = if null (dropWhile (not.(id `elem`)) before)
                             then []
                             else [id++" twice declarated"]

variableVoid id type1 = if type1 == VoidType 
                        then ["only methods can be of void type"]
                        else []

--para mark errores en el codigo
mark [] = ""
mark xs = tab (" <-# " ++ foldr (\x a-> x++(if null a then "" else "\n ^^ # "++a)) "" xs)

--funcion para busqueda de type1 en los types declarados
findType :: String -> [String] -> [Type] -> [[String]] ->[[Type]] -> Maybe Type
findType v declarateds types locals typesL | isJust i = Just ((typesL!!(fromJust i))!!(fromJust j))
                                             | isJust index = Just (types!!(fromJust index))
                                             | otherwise     = Nothing
    where (i,j) = findLocal v locals
          index = findIdent v declarateds

findIdent :: String -> [String] -> Maybe Int
findIdent v declarateds = elemIndex v declarateds 

--funcion para message error de types en asignacion
compTypes :: String -> Maybe Type -> Maybe Type ->[String]
compTypes v Nothing _  = ["undefined type  for '"++v++"'"]
compTypes v t1      t2 = if t1/=t2 then ["It was expected "++(expectedType t1)++" for "++v]
                                     else []
expectedType tE = case tE of
                       Just IntegerType -> "an integer" 
                       Just BooleanType -> "a boolean"
                       Just VoidType -> "void"
                       Nothing -> "nothing"

--funciones para evaluacion de types en expressiones

messageType1 op tE tI tD mI mD = (if tI==(Just tE)&&tD==tI then [] 
                                   else ["It was expected "++(expectedType (Just tE))
                                         ++ (lado tI tD tE) ++"'"++op++"'"]
                                 )++mI++mD

messageType2 op tI tD mI mD = (if tI==tD then [] 
                                else ["types are not comparables in '"++op++"'"]
                              )++mI++mD

messageType3 op tI tD mI mD = (if tI==(Just IntegerType)&&tI==tD then [] 
                                else ["types are not comparables in  '"++op++"'"]
                              )++mI++mD

lado typeI typeD typeE = let mE = Just typeE
                         in " a "++ (if typeI/=mE && typeD==mE then  "to the left side" 
                                      else if typeI==mE && typeD/=mE then "to the rigth side"
                                            else "both sides"
                                    ) ++" of "

--funciones para identacion 
tab :: String -> String
tab s = foldr (++) ""  ((map ("\t"++) (split (=='\n') s)))

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case breakI p s of 
                 (x,[]) -> insNotNull x []
                 (x,s') -> insNotNull x (w : split p s'')
                        where (w,s'') = breakI p s'
    where insNotNull x xs = if null x then xs else x:xs         

breakI :: (a -> Bool) -> [a] -> ([a], [a])
breakI _ [] = ([],[])
breakI p (x:xs) | p x = ([x],xs)
                | otherwise = (x:ys,zs) 
                      where (ys,zs) = breakI p xs

--funcion para generacion de labels ssm
newLabels :: String -> Int -> [String] ->[String]
newLabels method 0 xs = xs
newLabels method i [] = newLabels method (i-1) ["ET"++method++"0001"]
newLabels method i (x:xs) = newLabels method (i-1) ((increment method x):x:xs)

increment method etiqueta = etM ++ num 4 (show ((read n ::Int)+1))
    where etM = "ET"++method
          n = etiqueta \\ etM
num n xs = (replicate (n-(length xs)) '0') ++ xs

--acceso al espacio reservado de una variable
useSpaceVar :: String -> String -> [String] -> [[String]] -> String
useSpaceVar ac v all local | isJust i = "ldr MP\n" ++ (rep (fromJust i) "lda 0\n") 
                                             ++ ac ++"a "++ (show (fromJust j +1)) ++"\n"
                                | isJust k = "ldr R4\n"++ac++"a "++(show (fromJust k + 1))++"\n"
                                | otherwise = "nop nop nop" 
    where (i,j) = findLocal v local
          k     = findIdent v all 

rep i s =  foldl (++) "" (replicate i s)

--funcion para busqueda de variable local
findLocal :: String -> [[String]] -> (Maybe Int,Maybe Int)
findLocal v locals = case (foldl f (0,Nothing) locals) of
                             (i,Nothing) -> (Nothing,Nothing)
                             (i,Just j)  -> (Just (i-1),Just j)
    where f a ls = case a of
                       (i,Nothing) -> (i+1,findIdent v ls)
                       (i,Just j)  -> (i,Just j)

--funcion para insertar variable al ultimo ambito
insertHeadLast :: a -> [[a]] ->[[a]]
insertHeadLast x1 (xs:xss) = (xs++[x1]):xss 
insertHeadLast x1 [] = [[x1]]

--funcion para call a method
call method  = "bsr ET"++method++"0000\n"

--funcion para comprobacion de name de method
existsMethod method tMethods = if method `elem` tMethods then []
                             else ["Don't exists a method named "++method]

--revision de parameters
primerType :: [Type] -> Maybe Type
primerType [] = Nothing
primerType (x:xs) = Just x

findTypeParam :: String -> [String] -> [[Type]] -> [Type]
findTypeParam method all typesParam |isJust t   = typesParam!!(fromJust t)
                                        |otherwise = []
    where t = findIdent method all

messageNumParam form loc | form > loc = ["faltan "++ show (form - loc) ++" parameters"] 
                         | form < loc = ["sobran "++ show (loc - form) ++" parameters"]
                         | otherwise  = []

loadParams numParam = if numParam==0 then ""
                       else "ldml -" ++show (numParam+1)++" "++(show numParam)++"\n"
                            ++"stml 1 "++(show numParam)++"\n"

--funciones para errores en return
errorReturn method VoidType False = []
errorReturn method VoidType True  = ["the method "++method++" doesn't expect any return"]
errorReturn method type1      False = ["the method "++method++" must have a return in each option"]
errorReturn method type1      True  = []


errorInstruction return = if return then ["instruction unreachable"] else []

--funciones para reducir ambitos sin variables declarateds
linkLocal :: Int -> String -> String 
linkLocal num ins = if num>0 then "link "++(show num)++"\n"++ins++"unlink\n"
                             else ins++" nop\n"

localScope :: Int -> [[a]] -> [[a]]
localScope num locals = if num>0 then []:locals
                                   else locals
commaSeparated :: String -> String
commaSeparated xs = if null xs then xs else ", "++xs

{-# LINE 173 "ContextAnalisis.hs" #-}
-- Expression --------------------------------------------------
data Expression = LInteger (String)
                | LBoolean (String)
                | Variable (String)
                | Operation (Op) (Expression) (Expression)
                | ResultCall (String) (LocalParams)
                deriving ( Show)
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (LInteger _value) =
    (sem_Expression_LInteger _value)
sem_Expression (LBoolean _value) =
    (sem_Expression_LBoolean _value)
sem_Expression (Variable _ident) =
    (sem_Expression_Variable _ident)
sem_Expression (Operation _op _exprI _exprD) =
    (sem_Expression_Operation (sem_Op _op) (sem_Expression _exprI) (sem_Expression _exprD))
sem_Expression (ResultCall _method _parameters) =
    (sem_Expression_ResultCall _method (sem_LocalParams _parameters))
-- semantic domain
type T_Expression = ([String]) ->
                    ([[String]]) ->
                    ([String]) ->
                    ([String]) ->
                    ([[Type]]) ->
                    ([Type]) ->
                    ([Type]) ->
                    ([[Type]]) ->
                    ( Expression,String,([String]),([String]),String,(Maybe Type))
data Inh_Expression = Inh_Expression {aIdDefs_Inh_Expression :: ([String]),idsLocal_Inh_Expression :: ([[String]]),tIdDefs_Inh_Expression :: ([String]),tMethods_Inh_Expression :: ([String]),tTypesParam_Inh_Expression :: ([[Type]]),tTypesRet_Inh_Expression :: ([Type]),typesDefs_Inh_Expression :: ([Type]),typesLocal_Inh_Expression :: ([[Type]])}
data Syn_Expression = Syn_Expression {ast_Syn_Expression :: Expression,errorProgram_Syn_Expression :: String,messages_Syn_Expression :: ([String]),messagesType_Syn_Expression :: ([String]),output_Syn_Expression :: String,type1_Syn_Expression :: (Maybe Type)}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression sem (Inh_Expression _lhsIaIdDefs _lhsIidsLocal _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1) = sem _lhsIaIdDefs _lhsIidsLocal _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal
     in  (Syn_Expression _lhsOast _lhsOerrorProgram _lhsOmessages _lhsOmessagesType _lhsOoutput _lhsOtype1))
sem_Expression_LInteger :: String ->
                           T_Expression
sem_Expression_LInteger value_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Expression
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOtype1 :: (Maybe Type)
              _lhsOmessagesType :: ([String])
              _lhsOast =
                  ({-# LINE 429 "ContextAnalisis.ag" #-}
                   LInteger value_
                   {-# LINE 232 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 430 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 237 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 431 "ContextAnalisis.ag" #-}
                   value_
                   {-# LINE 242 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 432 "ContextAnalisis.ag" #-}
                   "ldc "++ value_ ++"\n"
                   {-# LINE 247 "ContextAnalisis.hs" #-}
                   )
              _lhsOtype1 =
                  ({-# LINE 457 "ContextAnalisis.ag" #-}
                   Just IntegerType
                   {-# LINE 252 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessagesType =
                  ({-# LINE 465 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 257 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1)))
sem_Expression_LBoolean :: String ->
                           T_Expression
sem_Expression_LBoolean value_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Expression
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOtype1 :: (Maybe Type)
              _lhsOmessagesType :: ([String])
              _lhsOast =
                  ({-# LINE 433 "ContextAnalisis.ag" #-}
                   LBoolean value_
                   {-# LINE 280 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 434 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 285 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 435 "ContextAnalisis.ag" #-}
                   value_
                   {-# LINE 290 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 436 "ContextAnalisis.ag" #-}
                   "ldc "++(if value_=="true" then "-1" else "0")++"\n"
                   {-# LINE 295 "ContextAnalisis.hs" #-}
                   )
              _lhsOtype1 =
                  ({-# LINE 458 "ContextAnalisis.ag" #-}
                   Just BooleanType
                   {-# LINE 300 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessagesType =
                  ({-# LINE 465 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 305 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1)))
sem_Expression_Variable :: String ->
                           T_Expression
sem_Expression_Variable ident_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Expression
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOtype1 :: (Maybe Type)
              _lhsOmessagesType :: ([String])
              _lhsOast =
                  ({-# LINE 442 "ContextAnalisis.ag" #-}
                   Variable ident_
                   {-# LINE 328 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 443 "ContextAnalisis.ag" #-}
                   messageUse ident_ _lhsItIdDefs _lhsIidsLocal
                   {-# LINE 333 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 444 "ContextAnalisis.ag" #-}
                   ident_
                   {-# LINE 338 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 445 "ContextAnalisis.ag" #-}
                   useSpaceVar "ld" ident_ _lhsItIdDefs _lhsIidsLocal
                   {-# LINE 343 "ContextAnalisis.hs" #-}
                   )
              _lhsOtype1 =
                  ({-# LINE 459 "ContextAnalisis.ag" #-}
                   findType ident_ _lhsItIdDefs _lhsItypesDefs _lhsIidsLocal _lhsItypesLocal
                   {-# LINE 348 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessagesType =
                  ({-# LINE 465 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 353 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1)))
sem_Expression_Operation :: T_Op ->
                            T_Expression ->
                            T_Expression ->
                            T_Expression
sem_Expression_Operation op_ exprI_ exprD_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Expression
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOtype1 :: (Maybe Type)
              _lhsOmessagesType :: ([String])
              _exprIOaIdDefs :: ([String])
              _exprIOidsLocal :: ([[String]])
              _exprIOtIdDefs :: ([String])
              _exprIOtMethods :: ([String])
              _exprIOtTypesParam :: ([[Type]])
              _exprIOtTypesRet :: ([Type])
              _exprIOtypesDefs :: ([Type])
              _exprIOtypesLocal :: ([[Type]])
              _exprDOaIdDefs :: ([String])
              _exprDOidsLocal :: ([[String]])
              _exprDOtIdDefs :: ([String])
              _exprDOtMethods :: ([String])
              _exprDOtTypesParam :: ([[Type]])
              _exprDOtTypesRet :: ([Type])
              _exprDOtypesDefs :: ([Type])
              _exprDOtypesLocal :: ([[Type]])
              _opIast :: Op
              _opIerrorProgram :: String
              _opImType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
              _opIoutput :: String
              _opItype1 :: (Maybe Type)
              _exprIIast :: Expression
              _exprIIerrorProgram :: String
              _exprIImessages :: ([String])
              _exprIImessagesType :: ([String])
              _exprIIoutput :: String
              _exprIItype1 :: (Maybe Type)
              _exprDIast :: Expression
              _exprDIerrorProgram :: String
              _exprDImessages :: ([String])
              _exprDImessagesType :: ([String])
              _exprDIoutput :: String
              _exprDItype1 :: (Maybe Type)
              _lhsOast =
                  ({-# LINE 437 "ContextAnalisis.ag" #-}
                   Operation _opIast _exprIIast _exprDIast
                   {-# LINE 411 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 438 "ContextAnalisis.ag" #-}
                   _exprIImessages ++ _exprDImessages
                   {-# LINE 416 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 439 "ContextAnalisis.ag" #-}
                   _exprIIerrorProgram ++ _opIerrorProgram
                    ++ _exprDIerrorProgram
                   {-# LINE 422 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 441 "ContextAnalisis.ag" #-}
                   _exprIIoutput++ _exprDIoutput++ _opIoutput
                   {-# LINE 427 "ContextAnalisis.hs" #-}
                   )
              _lhsOtype1 =
                  ({-# LINE 460 "ContextAnalisis.ag" #-}
                   _opItype1
                   {-# LINE 432 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessagesType =
                  ({-# LINE 461 "ContextAnalisis.ag" #-}
                   _opImType  _exprIItype1 _exprDItype1
                              _exprIImessagesType _exprDImessagesType
                   {-# LINE 438 "ContextAnalisis.hs" #-}
                   )
              _exprIOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 443 "ContextAnalisis.hs" #-}
                   )
              _exprIOidsLocal =
                  ({-# LINE 422 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 448 "ContextAnalisis.hs" #-}
                   )
              _exprIOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 453 "ContextAnalisis.hs" #-}
                   )
              _exprIOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 458 "ContextAnalisis.hs" #-}
                   )
              _exprIOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 463 "ContextAnalisis.hs" #-}
                   )
              _exprIOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 468 "ContextAnalisis.hs" #-}
                   )
              _exprIOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 473 "ContextAnalisis.hs" #-}
                   )
              _exprIOtypesLocal =
                  ({-# LINE 423 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 478 "ContextAnalisis.hs" #-}
                   )
              _exprDOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 483 "ContextAnalisis.hs" #-}
                   )
              _exprDOidsLocal =
                  ({-# LINE 422 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 488 "ContextAnalisis.hs" #-}
                   )
              _exprDOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 493 "ContextAnalisis.hs" #-}
                   )
              _exprDOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 498 "ContextAnalisis.hs" #-}
                   )
              _exprDOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 503 "ContextAnalisis.hs" #-}
                   )
              _exprDOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 508 "ContextAnalisis.hs" #-}
                   )
              _exprDOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 513 "ContextAnalisis.hs" #-}
                   )
              _exprDOtypesLocal =
                  ({-# LINE 423 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 518 "ContextAnalisis.hs" #-}
                   )
              ( _opIast,_opIerrorProgram,_opImType,_opIoutput,_opItype1) =
                  op_
              ( _exprIIast,_exprIIerrorProgram,_exprIImessages,_exprIImessagesType,_exprIIoutput,_exprIItype1) =
                  exprI_ _exprIOaIdDefs _exprIOidsLocal _exprIOtIdDefs _exprIOtMethods _exprIOtTypesParam _exprIOtTypesRet _exprIOtypesDefs _exprIOtypesLocal
              ( _exprDIast,_exprDIerrorProgram,_exprDImessages,_exprDImessagesType,_exprDIoutput,_exprDItype1) =
                  exprD_ _exprDOaIdDefs _exprDOidsLocal _exprDOtIdDefs _exprDOtMethods _exprDOtTypesParam _exprDOtTypesRet _exprDOtypesDefs _exprDOtypesLocal
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1)))
sem_Expression_ResultCall :: String ->
                             T_LocalParams ->
                             T_Expression
sem_Expression_ResultCall method_ parameters_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Expression
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _parametersOtypeParam :: ([Type])
              _lhsOtype1 :: (Maybe Type)
              _lhsOmessagesType :: ([String])
              _parametersOaIdDefs :: ([String])
              _parametersOidsLocal :: ([[String]])
              _parametersOtIdDefs :: ([String])
              _parametersOtMethods :: ([String])
              _parametersOtTypesParam :: ([[Type]])
              _parametersOtTypesRet :: ([Type])
              _parametersOtypesDefs :: ([Type])
              _parametersOtypesLocal :: ([[Type]])
              _parametersIast :: LocalParams
              _parametersIerrorProgram :: String
              _parametersImessages :: ([String])
              _parametersInumParam :: Int
              _parametersIoutput :: String
              _lhsOast =
                  ({-# LINE 446 "ContextAnalisis.ag" #-}
                   ResultCall method_ _parametersIast
                   {-# LINE 562 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 447 "ContextAnalisis.ag" #-}
                   existsMethod method_ _lhsItMethods
                   ++ (messageNumParam (length _typeParam) _parametersInumParam)
                   {-# LINE 568 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 449 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 573 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 450 "ContextAnalisis.ag" #-}
                   method_ ++"(" ++ _parametersIerrorProgram ++")"
                   {-# LINE 578 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 451 "ContextAnalisis.ag" #-}
                   _parametersIoutput ++ call method_
                   ++ rep _parametersInumParam "str R7\n"
                   ++"ldr RR\n"
                   {-# LINE 585 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypeParam =
                  ({-# LINE 454 "ContextAnalisis.ag" #-}
                   _typeParam
                   {-# LINE 590 "ContextAnalisis.hs" #-}
                   )
              _typeParam =
                  ({-# LINE 455 "ContextAnalisis.ag" #-}
                   findTypeParam method_ _lhsItMethods _lhsItTypesParam
                   {-# LINE 595 "ContextAnalisis.hs" #-}
                   )
              _lhsOtype1 =
                  ({-# LINE 463 "ContextAnalisis.ag" #-}
                   findType method_ _lhsItMethods _lhsItTypesRet [] []
                   {-# LINE 600 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessagesType =
                  ({-# LINE 464 "ContextAnalisis.ag" #-}
                   _parametersImessages
                   {-# LINE 605 "ContextAnalisis.hs" #-}
                   )
              _parametersOaIdDefs =
                  ({-# LINE 383 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 610 "ContextAnalisis.hs" #-}
                   )
              _parametersOidsLocal =
                  ({-# LINE 387 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 615 "ContextAnalisis.hs" #-}
                   )
              _parametersOtIdDefs =
                  ({-# LINE 381 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 620 "ContextAnalisis.hs" #-}
                   )
              _parametersOtMethods =
                  ({-# LINE 389 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 625 "ContextAnalisis.hs" #-}
                   )
              _parametersOtTypesParam =
                  ({-# LINE 390 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 630 "ContextAnalisis.hs" #-}
                   )
              _parametersOtTypesRet =
                  ({-# LINE 391 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 635 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypesDefs =
                  ({-# LINE 382 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 640 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypesLocal =
                  ({-# LINE 388 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 645 "ContextAnalisis.hs" #-}
                   )
              ( _parametersIast,_parametersIerrorProgram,_parametersImessages,_parametersInumParam,_parametersIoutput) =
                  parameters_ _parametersOaIdDefs _parametersOidsLocal _parametersOtIdDefs _parametersOtMethods _parametersOtTypesParam _parametersOtTypesRet _parametersOtypeParam _parametersOtypesDefs _parametersOtypesLocal
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOmessagesType,_lhsOoutput,_lhsOtype1)))
-- Instruction -------------------------------------------------
data Instruction = Asignement (String) (Expression)
                 | Conditional (Expression) (InstructionsList) (InstructionsList)
                 | ConditionalCycle (Expression) (InstructionsList)
                 | Declaration (Type) (String)
                 | MethodCall (String) (LocalParams)
                 | Return (Expression)
                 deriving ( Show)
-- cata
sem_Instruction :: Instruction ->
                   T_Instruction
sem_Instruction (Asignement _ident _expr) =
    (sem_Instruction_Asignement _ident (sem_Expression _expr))
sem_Instruction (Conditional _cond _ifInstrs _elseInstrs) =
    (sem_Instruction_Conditional (sem_Expression _cond) (sem_InstructionsList _ifInstrs) (sem_InstructionsList _elseInstrs))
sem_Instruction (ConditionalCycle _cond _instrs) =
    (sem_Instruction_ConditionalCycle (sem_Expression _cond) (sem_InstructionsList _instrs))
sem_Instruction (Declaration _type1 _ident) =
    (sem_Instruction_Declaration (sem_Type _type1) _ident)
sem_Instruction (MethodCall _method _parameters) =
    (sem_Instruction_MethodCall _method (sem_LocalParams _parameters))
sem_Instruction (Return _expr) =
    (sem_Instruction_Return (sem_Expression _expr))
-- semantic domain
type T_Instruction = ([String]) ->
                     Bool ->
                     Bool ->
                     ([[String]]) ->
                     ([String]) ->
                     String ->
                     ([String]) ->
                     ([String]) ->
                     ([[Type]]) ->
                     ([Type]) ->
                     ([Type]) ->
                     ([[Type]]) ->
                     ( Bool,Instruction,String,Bool,([[String]]),([String]),([String]),Int,String,([[Type]]))
data Inh_Instruction = Inh_Instruction {aIdDefs_Inh_Instruction :: ([String]),algunReturn_Inh_Instruction :: Bool,faltaReturn_Inh_Instruction :: Bool,idsLocal_Inh_Instruction :: ([[String]]),labels_Inh_Instruction :: ([String]),method_Inh_Instruction :: String,tIdDefs_Inh_Instruction :: ([String]),tMethods_Inh_Instruction :: ([String]),tTypesParam_Inh_Instruction :: ([[Type]]),tTypesRet_Inh_Instruction :: ([Type]),typesDefs_Inh_Instruction :: ([Type]),typesLocal_Inh_Instruction :: ([[Type]])}
data Syn_Instruction = Syn_Instruction {algunReturn_Syn_Instruction :: Bool,ast_Syn_Instruction :: Instruction,errorProgram_Syn_Instruction :: String,faltaReturn_Syn_Instruction :: Bool,idsLocal_Syn_Instruction :: ([[String]]),labels_Syn_Instruction :: ([String]),messages_Syn_Instruction :: ([String]),numVar_Syn_Instruction :: Int,output_Syn_Instruction :: String,typesLocal_Syn_Instruction :: ([[Type]])}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIaIdDefs _lhsIalgunReturn _lhsIfaltaReturn _lhsIidsLocal _lhsIlabels _lhsImethod _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal) =
    (let ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal) = sem _lhsIaIdDefs _lhsIalgunReturn _lhsIfaltaReturn _lhsIidsLocal _lhsIlabels _lhsImethod _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal
     in  (Syn_Instruction _lhsOalgunReturn _lhsOast _lhsOerrorProgram _lhsOfaltaReturn _lhsOidsLocal _lhsOlabels _lhsOmessages _lhsOnumVar _lhsOoutput _lhsOtypesLocal))
sem_Instruction_Asignement :: String ->
                              T_Expression ->
                              T_Instruction
sem_Instruction_Asignement ident_ expr_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOnumVar :: Int
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOidsLocal :: ([[String]])
              _lhsOlabels :: ([String])
              _lhsOtypesLocal :: ([[Type]])
              _exprOaIdDefs :: ([String])
              _exprOidsLocal :: ([[String]])
              _exprOtIdDefs :: ([String])
              _exprOtMethods :: ([String])
              _exprOtTypesParam :: ([[Type]])
              _exprOtTypesRet :: ([Type])
              _exprOtypesDefs :: ([Type])
              _exprOtypesLocal :: ([[Type]])
              _exprIast :: Expression
              _exprIerrorProgram :: String
              _exprImessages :: ([String])
              _exprImessagesType :: ([String])
              _exprIoutput :: String
              _exprItype1 :: (Maybe Type)
              _lhsOast =
                  ({-# LINE 283 "ContextAnalisis.ag" #-}
                   Asignement ident_ _exprIast
                   {-# LINE 738 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 284 "ContextAnalisis.ag" #-}
                   _errorRet ++ (messageUse ident_ _lhsItIdDefs _lhsIidsLocal)
                   ++ (compTypes ("the variable '"++ ident_++"'")
                                 (findType ident_ _lhsItIdDefs _lhsItypesDefs
                                             _lhsIidsLocal _lhsItypesLocal)
                                 _exprItype1
                      ) ++ _exprImessagesType ++ _exprImessages
                   {-# LINE 748 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 290 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 753 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 291 "ContextAnalisis.ag" #-}
                   ident_++" = "++ _exprIerrorProgram++";" ++ mark _error
                   {-# LINE 758 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 292 "ContextAnalisis.ag" #-}
                   _exprIoutput ++ useSpaceVar "st" ident_ _lhsItIdDefs _lhsIidsLocal
                   {-# LINE 763 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 768 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 377 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 773 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 279 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 778 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 280 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 783 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 273 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 788 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 793 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 274 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 798 "ContextAnalisis.hs" #-}
                   )
              _exprOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 803 "ContextAnalisis.hs" #-}
                   )
              _exprOidsLocal =
                  ({-# LINE 422 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 808 "ContextAnalisis.hs" #-}
                   )
              _exprOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 813 "ContextAnalisis.hs" #-}
                   )
              _exprOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 818 "ContextAnalisis.hs" #-}
                   )
              _exprOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 823 "ContextAnalisis.hs" #-}
                   )
              _exprOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 828 "ContextAnalisis.hs" #-}
                   )
              _exprOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 833 "ContextAnalisis.hs" #-}
                   )
              _exprOtypesLocal =
                  ({-# LINE 423 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 838 "ContextAnalisis.hs" #-}
                   )
              ( _exprIast,_exprIerrorProgram,_exprImessages,_exprImessagesType,_exprIoutput,_exprItype1) =
                  expr_ _exprOaIdDefs _exprOidsLocal _exprOtIdDefs _exprOtMethods _exprOtTypesParam _exprOtTypesRet _exprOtypesDefs _exprOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_Instruction_Conditional :: T_Expression ->
                               T_InstructionsList ->
                               T_InstructionsList ->
                               T_Instruction
sem_Instruction_Conditional cond_ ifInstrs_ elseInstrs_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _ifInstrsOlabels :: ([String])
              _condOidsLocal :: ([[String]])
              _condOtypesLocal :: ([[Type]])
              _ifInstrsOidsLocal :: ([[String]])
              _ifInstrsOtypesLocal :: ([[Type]])
              _elseInstrsOidsLocal :: ([[String]])
              _elseInstrsOtypesLocal :: ([[Type]])
              _lhsOidsLocal :: ([[String]])
              _lhsOtypesLocal :: ([[Type]])
              _elseInstrsOalgunReturn :: Bool
              _elseInstrsOfaltaReturn :: Bool
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOnumVar :: Int
              _lhsOlabels :: ([String])
              _condOaIdDefs :: ([String])
              _condOtIdDefs :: ([String])
              _condOtMethods :: ([String])
              _condOtTypesParam :: ([[Type]])
              _condOtTypesRet :: ([Type])
              _condOtypesDefs :: ([Type])
              _ifInstrsOaIdDefs :: ([String])
              _ifInstrsOalgunReturn :: Bool
              _ifInstrsOfaltaReturn :: Bool
              _ifInstrsOmethod :: String
              _ifInstrsOtIdDefs :: ([String])
              _ifInstrsOtMethods :: ([String])
              _ifInstrsOtTypesParam :: ([[Type]])
              _ifInstrsOtTypesRet :: ([Type])
              _ifInstrsOtypesDefs :: ([Type])
              _elseInstrsOaIdDefs :: ([String])
              _elseInstrsOlabels :: ([String])
              _elseInstrsOmethod :: String
              _elseInstrsOtIdDefs :: ([String])
              _elseInstrsOtMethods :: ([String])
              _elseInstrsOtTypesParam :: ([[Type]])
              _elseInstrsOtTypesRet :: ([Type])
              _elseInstrsOtypesDefs :: ([Type])
              _condIast :: Expression
              _condIerrorProgram :: String
              _condImessages :: ([String])
              _condImessagesType :: ([String])
              _condIoutput :: String
              _condItype1 :: (Maybe Type)
              _ifInstrsIalgunReturn :: Bool
              _ifInstrsIast :: InstructionsList
              _ifInstrsIerrorProgram :: String
              _ifInstrsIfaltaReturn :: Bool
              _ifInstrsIidsLocal :: ([[String]])
              _ifInstrsIlabels :: ([String])
              _ifInstrsImessages :: ([String])
              _ifInstrsInumVar :: Int
              _ifInstrsIoutput :: String
              _ifInstrsItypesLocal :: ([[Type]])
              _elseInstrsIalgunReturn :: Bool
              _elseInstrsIast :: InstructionsList
              _elseInstrsIerrorProgram :: String
              _elseInstrsIfaltaReturn :: Bool
              _elseInstrsIidsLocal :: ([[String]])
              _elseInstrsIlabels :: ([String])
              _elseInstrsImessages :: ([String])
              _elseInstrsInumVar :: Int
              _elseInstrsIoutput :: String
              _elseInstrsItypesLocal :: ([[Type]])
              _lhsOast =
                  ({-# LINE 294 "ContextAnalisis.ag" #-}
                   Conditional _condIast _ifInstrsIast _elseInstrsIast
                   {-# LINE 931 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 295 "ContextAnalisis.ag" #-}
                   _error ++ _ifInstrsImessages ++ _elseInstrsImessages
                   {-# LINE 936 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 296 "ContextAnalisis.ag" #-}
                   "if("++ _condIerrorProgram++"){"
                    ++ mark _error ++"\n"++(tab _ifInstrsIerrorProgram)++"}"
                    ++(if null _elseInstrsIerrorProgram then ""
                        else "else{\n"++(tab _elseInstrsIerrorProgram)++"}")
                   {-# LINE 944 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 300 "ContextAnalisis.ag" #-}
                   linkLocal _numVar (_condIoutput ++"brf "
                                     ++(_nLabels!!1)++"\n"
                                     ++ _ifInstrsIoutput++"bra " ++(_nLabels!!0)++"\n" ++(_nLabels!!1)++": "
                                     ++ _elseInstrsIoutput++"nop\n"
                                     ++ (_nLabels!!0)++":")
                   {-# LINE 953 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOlabels =
                  ({-# LINE 305 "ContextAnalisis.ag" #-}
                   _nLabels
                   {-# LINE 958 "ContextAnalisis.hs" #-}
                   )
              _idsLocal =
                  ({-# LINE 306 "ContextAnalisis.ag" #-}
                   localScope _numVar _lhsIidsLocal
                   {-# LINE 963 "ContextAnalisis.hs" #-}
                   )
              _typesLocal =
                  ({-# LINE 307 "ContextAnalisis.ag" #-}
                   localScope _numVar _lhsItypesLocal
                   {-# LINE 968 "ContextAnalisis.hs" #-}
                   )
              _condOidsLocal =
                  ({-# LINE 308 "ContextAnalisis.ag" #-}
                   _idsLocal
                   {-# LINE 973 "ContextAnalisis.hs" #-}
                   )
              _condOtypesLocal =
                  ({-# LINE 309 "ContextAnalisis.ag" #-}
                   _typesLocal
                   {-# LINE 978 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOidsLocal =
                  ({-# LINE 310 "ContextAnalisis.ag" #-}
                   _idsLocal
                   {-# LINE 983 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtypesLocal =
                  ({-# LINE 311 "ContextAnalisis.ag" #-}
                   _typesLocal
                   {-# LINE 988 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOidsLocal =
                  ({-# LINE 312 "ContextAnalisis.ag" #-}
                   _idsLocal
                   {-# LINE 993 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtypesLocal =
                  ({-# LINE 313 "ContextAnalisis.ag" #-}
                   _typesLocal
                   {-# LINE 998 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 314 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1003 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 315 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1008 "ContextAnalisis.hs" #-}
                   )
              _numVar =
                  ({-# LINE 316 "ContextAnalisis.ag" #-}
                   max _ifInstrsInumVar _elseInstrsInumVar
                   {-# LINE 1013 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOalgunReturn =
                  ({-# LINE 317 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1018 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOfaltaReturn =
                  ({-# LINE 318 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1023 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 319 "ContextAnalisis.ag" #-}
                   _ifInstrsIalgunReturn || _elseInstrsIalgunReturn
                   {-# LINE 1028 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 320 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   && (_ifInstrsIfaltaReturn || _elseInstrsIfaltaReturn)
                   {-# LINE 1034 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 372 "ContextAnalisis.ag" #-}
                   _errorRet ++ _condImessages ++ _condImessagesType
                   ++ compTypes ("la condicion "++ _condIerrorProgram)
                                (Just BooleanType) _condItype1
                   {-# LINE 1041 "ContextAnalisis.hs" #-}
                   )
              _nLabels =
                  ({-# LINE 375 "ContextAnalisis.ag" #-}
                   newLabels _lhsImethod 2 _lhsIlabels
                   {-# LINE 1046 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 1051 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 377 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 1056 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _elseInstrsIlabels
                   {-# LINE 1061 "ContextAnalisis.hs" #-}
                   )
              _condOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1066 "ContextAnalisis.hs" #-}
                   )
              _condOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1071 "ContextAnalisis.hs" #-}
                   )
              _condOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1076 "ContextAnalisis.hs" #-}
                   )
              _condOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1081 "ContextAnalisis.hs" #-}
                   )
              _condOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1086 "ContextAnalisis.hs" #-}
                   )
              _condOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1091 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOaIdDefs =
                  ({-# LINE 237 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1096 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOalgunReturn =
                  ({-# LINE 248 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1101 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOfaltaReturn =
                  ({-# LINE 249 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1106 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOmethod =
                  ({-# LINE 247 "ContextAnalisis.ag" #-}
                   _lhsImethod
                   {-# LINE 1111 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtIdDefs =
                  ({-# LINE 235 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1116 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtMethods =
                  ({-# LINE 244 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1121 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtTypesParam =
                  ({-# LINE 245 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1126 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtTypesRet =
                  ({-# LINE 246 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1131 "ContextAnalisis.hs" #-}
                   )
              _ifInstrsOtypesDefs =
                  ({-# LINE 236 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1136 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOaIdDefs =
                  ({-# LINE 237 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1141 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOlabels =
                  ({-# LINE 241 "ContextAnalisis.ag" #-}
                   _ifInstrsIlabels
                   {-# LINE 1146 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOmethod =
                  ({-# LINE 247 "ContextAnalisis.ag" #-}
                   _lhsImethod
                   {-# LINE 1151 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtIdDefs =
                  ({-# LINE 235 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1156 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtMethods =
                  ({-# LINE 244 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1161 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtTypesParam =
                  ({-# LINE 245 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1166 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtTypesRet =
                  ({-# LINE 246 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1171 "ContextAnalisis.hs" #-}
                   )
              _elseInstrsOtypesDefs =
                  ({-# LINE 236 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1176 "ContextAnalisis.hs" #-}
                   )
              ( _condIast,_condIerrorProgram,_condImessages,_condImessagesType,_condIoutput,_condItype1) =
                  cond_ _condOaIdDefs _condOidsLocal _condOtIdDefs _condOtMethods _condOtTypesParam _condOtTypesRet _condOtypesDefs _condOtypesLocal
              ( _ifInstrsIalgunReturn,_ifInstrsIast,_ifInstrsIerrorProgram,_ifInstrsIfaltaReturn,_ifInstrsIidsLocal,_ifInstrsIlabels,_ifInstrsImessages,_ifInstrsInumVar,_ifInstrsIoutput,_ifInstrsItypesLocal) =
                  ifInstrs_ _ifInstrsOaIdDefs _ifInstrsOalgunReturn _ifInstrsOfaltaReturn _ifInstrsOidsLocal _ifInstrsOlabels _ifInstrsOmethod _ifInstrsOtIdDefs _ifInstrsOtMethods _ifInstrsOtTypesParam _ifInstrsOtTypesRet _ifInstrsOtypesDefs _ifInstrsOtypesLocal
              ( _elseInstrsIalgunReturn,_elseInstrsIast,_elseInstrsIerrorProgram,_elseInstrsIfaltaReturn,_elseInstrsIidsLocal,_elseInstrsIlabels,_elseInstrsImessages,_elseInstrsInumVar,_elseInstrsIoutput,_elseInstrsItypesLocal) =
                  elseInstrs_ _elseInstrsOaIdDefs _elseInstrsOalgunReturn _elseInstrsOfaltaReturn _elseInstrsOidsLocal _elseInstrsOlabels _elseInstrsOmethod _elseInstrsOtIdDefs _elseInstrsOtMethods _elseInstrsOtTypesParam _elseInstrsOtTypesRet _elseInstrsOtypesDefs _elseInstrsOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_Instruction_ConditionalCycle :: T_Expression ->
                                    T_InstructionsList ->
                                    T_Instruction
sem_Instruction_ConditionalCycle cond_ instrs_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _instrsOlabels :: ([String])
              _condOidsLocal :: ([[String]])
              _condOtypesLocal :: ([[Type]])
              _instrsOidsLocal :: ([[String]])
              _instrsOtypesLocal :: ([[Type]])
              _lhsOidsLocal :: ([[String]])
              _lhsOtypesLocal :: ([[Type]])
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOnumVar :: Int
              _lhsOlabels :: ([String])
              _condOaIdDefs :: ([String])
              _condOtIdDefs :: ([String])
              _condOtMethods :: ([String])
              _condOtTypesParam :: ([[Type]])
              _condOtTypesRet :: ([Type])
              _condOtypesDefs :: ([Type])
              _instrsOaIdDefs :: ([String])
              _instrsOalgunReturn :: Bool
              _instrsOfaltaReturn :: Bool
              _instrsOmethod :: String
              _instrsOtIdDefs :: ([String])
              _instrsOtMethods :: ([String])
              _instrsOtTypesParam :: ([[Type]])
              _instrsOtTypesRet :: ([Type])
              _instrsOtypesDefs :: ([Type])
              _condIast :: Expression
              _condIerrorProgram :: String
              _condImessages :: ([String])
              _condImessagesType :: ([String])
              _condIoutput :: String
              _condItype1 :: (Maybe Type)
              _instrsIalgunReturn :: Bool
              _instrsIast :: InstructionsList
              _instrsIerrorProgram :: String
              _instrsIfaltaReturn :: Bool
              _instrsIidsLocal :: ([[String]])
              _instrsIlabels :: ([String])
              _instrsImessages :: ([String])
              _instrsInumVar :: Int
              _instrsIoutput :: String
              _instrsItypesLocal :: ([[Type]])
              _lhsOast =
                  ({-# LINE 322 "ContextAnalisis.ag" #-}
                   ConditionalCycle _condIast _instrsIast
                   {-# LINE 1250 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 323 "ContextAnalisis.ag" #-}
                   _error ++ _instrsImessages
                   {-# LINE 1255 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 324 "ContextAnalisis.ag" #-}
                   "while("++ _condIerrorProgram++"){"++ mark _error
                    ++"\n"++(tab _instrsIerrorProgram)++"}"
                   {-# LINE 1261 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 326 "ContextAnalisis.ag" #-}
                   linkLocal _numVar ((_nLabels!!1)++": "
                                      ++ _condIoutput ++"brf " ++(_nLabels!!0)++"\n"
                                      ++ _instrsIoutput ++"bra " ++ (_nLabels!!1)++"\n" ++(_nLabels!!0)++":")
                   {-# LINE 1268 "ContextAnalisis.hs" #-}
                   )
              _instrsOlabels =
                  ({-# LINE 329 "ContextAnalisis.ag" #-}
                   _nLabels
                   {-# LINE 1273 "ContextAnalisis.hs" #-}
                   )
              _idsLocal =
                  ({-# LINE 330 "ContextAnalisis.ag" #-}
                   localScope _numVar _lhsIidsLocal
                   {-# LINE 1278 "ContextAnalisis.hs" #-}
                   )
              _typesLocal =
                  ({-# LINE 331 "ContextAnalisis.ag" #-}
                   localScope _numVar _lhsItypesLocal
                   {-# LINE 1283 "ContextAnalisis.hs" #-}
                   )
              _condOidsLocal =
                  ({-# LINE 332 "ContextAnalisis.ag" #-}
                   _idsLocal
                   {-# LINE 1288 "ContextAnalisis.hs" #-}
                   )
              _condOtypesLocal =
                  ({-# LINE 333 "ContextAnalisis.ag" #-}
                   _typesLocal
                   {-# LINE 1293 "ContextAnalisis.hs" #-}
                   )
              _instrsOidsLocal =
                  ({-# LINE 334 "ContextAnalisis.ag" #-}
                   _idsLocal
                   {-# LINE 1298 "ContextAnalisis.hs" #-}
                   )
              _instrsOtypesLocal =
                  ({-# LINE 335 "ContextAnalisis.ag" #-}
                   _typesLocal
                   {-# LINE 1303 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 336 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1308 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 337 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1313 "ContextAnalisis.hs" #-}
                   )
              _numVar =
                  ({-# LINE 338 "ContextAnalisis.ag" #-}
                   _instrsInumVar
                   {-# LINE 1318 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 339 "ContextAnalisis.ag" #-}
                   False
                   {-# LINE 1323 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 340 "ContextAnalisis.ag" #-}
                   True
                   {-# LINE 1328 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 372 "ContextAnalisis.ag" #-}
                   _errorRet ++ _condImessages ++ _condImessagesType
                   ++ compTypes ("la condicion "++ _condIerrorProgram)
                                (Just BooleanType) _condItype1
                   {-# LINE 1335 "ContextAnalisis.hs" #-}
                   )
              _nLabels =
                  ({-# LINE 375 "ContextAnalisis.ag" #-}
                   newLabels _lhsImethod 2 _lhsIlabels
                   {-# LINE 1340 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 1345 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 377 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 1350 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _instrsIlabels
                   {-# LINE 1355 "ContextAnalisis.hs" #-}
                   )
              _condOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1360 "ContextAnalisis.hs" #-}
                   )
              _condOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1365 "ContextAnalisis.hs" #-}
                   )
              _condOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1370 "ContextAnalisis.hs" #-}
                   )
              _condOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1375 "ContextAnalisis.hs" #-}
                   )
              _condOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1380 "ContextAnalisis.hs" #-}
                   )
              _condOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1385 "ContextAnalisis.hs" #-}
                   )
              _instrsOaIdDefs =
                  ({-# LINE 237 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1390 "ContextAnalisis.hs" #-}
                   )
              _instrsOalgunReturn =
                  ({-# LINE 248 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1395 "ContextAnalisis.hs" #-}
                   )
              _instrsOfaltaReturn =
                  ({-# LINE 249 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1400 "ContextAnalisis.hs" #-}
                   )
              _instrsOmethod =
                  ({-# LINE 247 "ContextAnalisis.ag" #-}
                   _lhsImethod
                   {-# LINE 1405 "ContextAnalisis.hs" #-}
                   )
              _instrsOtIdDefs =
                  ({-# LINE 235 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1410 "ContextAnalisis.hs" #-}
                   )
              _instrsOtMethods =
                  ({-# LINE 244 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1415 "ContextAnalisis.hs" #-}
                   )
              _instrsOtTypesParam =
                  ({-# LINE 245 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1420 "ContextAnalisis.hs" #-}
                   )
              _instrsOtTypesRet =
                  ({-# LINE 246 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1425 "ContextAnalisis.hs" #-}
                   )
              _instrsOtypesDefs =
                  ({-# LINE 236 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1430 "ContextAnalisis.hs" #-}
                   )
              ( _condIast,_condIerrorProgram,_condImessages,_condImessagesType,_condIoutput,_condItype1) =
                  cond_ _condOaIdDefs _condOidsLocal _condOtIdDefs _condOtMethods _condOtTypesParam _condOtTypesRet _condOtypesDefs _condOtypesLocal
              ( _instrsIalgunReturn,_instrsIast,_instrsIerrorProgram,_instrsIfaltaReturn,_instrsIidsLocal,_instrsIlabels,_instrsImessages,_instrsInumVar,_instrsIoutput,_instrsItypesLocal) =
                  instrs_ _instrsOaIdDefs _instrsOalgunReturn _instrsOfaltaReturn _instrsOidsLocal _instrsOlabels _instrsOmethod _instrsOtIdDefs _instrsOtMethods _instrsOtTypesParam _instrsOtTypesRet _instrsOtypesDefs _instrsOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_Instruction_Declaration :: T_Type ->
                               String ->
                               T_Instruction
sem_Instruction_Declaration type1_ ident_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOidsLocal :: ([[String]])
              _lhsOtypesLocal :: ([[Type]])
              _lhsOnumVar :: Int
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOlabels :: ([String])
              _type1Iast :: Type
              _type1IerrorProgram :: String
              _lhsOast =
                  ({-# LINE 341 "ContextAnalisis.ag" #-}
                   Declaration _type1Iast ident_
                   {-# LINE 1468 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 342 "ContextAnalisis.ag" #-}
                   _errorRet ++ messageDecl ident_  _lhsIidsLocal ++ variableVoid ident_ _type1Iast
                   {-# LINE 1473 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 343 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 1478 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 344 "ContextAnalisis.ag" #-}
                   _type1IerrorProgram ++ " "++ ident_ ++ ";"
                     ++ mark _error
                   {-# LINE 1484 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 346 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 1489 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 347 "ContextAnalisis.ag" #-}
                   insertHeadLast ident_ _lhsIidsLocal
                   {-# LINE 1494 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 348 "ContextAnalisis.ag" #-}
                   insertHeadLast _type1Iast _lhsItypesLocal
                   {-# LINE 1499 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 349 "ContextAnalisis.ag" #-}
                   1
                   {-# LINE 1504 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 1509 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 279 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1514 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 280 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1519 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 1524 "ContextAnalisis.hs" #-}
                   )
              ( _type1Iast,_type1IerrorProgram) =
                  type1_
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_Instruction_MethodCall :: String ->
                              T_LocalParams ->
                              T_Instruction
sem_Instruction_MethodCall method_ parameters_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _parametersOtypeParam :: ([Type])
              _lhsOnumVar :: Int
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOidsLocal :: ([[String]])
              _lhsOlabels :: ([String])
              _lhsOtypesLocal :: ([[Type]])
              _parametersOaIdDefs :: ([String])
              _parametersOidsLocal :: ([[String]])
              _parametersOtIdDefs :: ([String])
              _parametersOtMethods :: ([String])
              _parametersOtTypesParam :: ([[Type]])
              _parametersOtTypesRet :: ([Type])
              _parametersOtypesDefs :: ([Type])
              _parametersOtypesLocal :: ([[Type]])
              _parametersIast :: LocalParams
              _parametersIerrorProgram :: String
              _parametersImessages :: ([String])
              _parametersInumParam :: Int
              _parametersIoutput :: String
              _lhsOast =
                  ({-# LINE 350 "ContextAnalisis.ag" #-}
                   MethodCall method_  _parametersIast
                   {-# LINE 1572 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 351 "ContextAnalisis.ag" #-}
                   existsMethod method_ _lhsItMethods ++ _parametersImessages
                   ++ (messageNumParam (length _typeParam) _parametersInumParam)
                   {-# LINE 1578 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 353 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 1583 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 354 "ContextAnalisis.ag" #-}
                   method_ ++"(" ++ _parametersIerrorProgram ++");"++ mark _error
                   {-# LINE 1588 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 355 "ContextAnalisis.ag" #-}
                   _parametersIoutput ++ call method_
                   {-# LINE 1593 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypeParam =
                  ({-# LINE 356 "ContextAnalisis.ag" #-}
                   _typeParam
                   {-# LINE 1598 "ContextAnalisis.hs" #-}
                   )
              _typeParam =
                  ({-# LINE 357 "ContextAnalisis.ag" #-}
                   findTypeParam method_ _lhsItMethods _lhsItTypesParam
                   {-# LINE 1603 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 1608 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 377 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 1613 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 279 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1618 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 280 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1623 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 273 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1628 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 1633 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 274 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1638 "ContextAnalisis.hs" #-}
                   )
              _parametersOaIdDefs =
                  ({-# LINE 383 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1643 "ContextAnalisis.hs" #-}
                   )
              _parametersOidsLocal =
                  ({-# LINE 387 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1648 "ContextAnalisis.hs" #-}
                   )
              _parametersOtIdDefs =
                  ({-# LINE 381 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1653 "ContextAnalisis.hs" #-}
                   )
              _parametersOtMethods =
                  ({-# LINE 389 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1658 "ContextAnalisis.hs" #-}
                   )
              _parametersOtTypesParam =
                  ({-# LINE 390 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1663 "ContextAnalisis.hs" #-}
                   )
              _parametersOtTypesRet =
                  ({-# LINE 391 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1668 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypesDefs =
                  ({-# LINE 382 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1673 "ContextAnalisis.hs" #-}
                   )
              _parametersOtypesLocal =
                  ({-# LINE 388 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1678 "ContextAnalisis.hs" #-}
                   )
              ( _parametersIast,_parametersIerrorProgram,_parametersImessages,_parametersInumParam,_parametersIoutput) =
                  parameters_ _parametersOaIdDefs _parametersOidsLocal _parametersOtIdDefs _parametersOtMethods _parametersOtTypesParam _parametersOtTypesRet _parametersOtypeParam _parametersOtypesDefs _parametersOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_Instruction_Return :: T_Expression ->
                          T_Instruction
sem_Instruction_Return expr_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: Instruction
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOnumVar :: Int
              _lhsOidsLocal :: ([[String]])
              _lhsOlabels :: ([String])
              _lhsOtypesLocal :: ([[Type]])
              _exprOaIdDefs :: ([String])
              _exprOidsLocal :: ([[String]])
              _exprOtIdDefs :: ([String])
              _exprOtMethods :: ([String])
              _exprOtTypesParam :: ([[Type]])
              _exprOtTypesRet :: ([Type])
              _exprOtypesDefs :: ([Type])
              _exprOtypesLocal :: ([[Type]])
              _exprIast :: Expression
              _exprIerrorProgram :: String
              _exprImessages :: ([String])
              _exprImessagesType :: ([String])
              _exprIoutput :: String
              _exprItype1 :: (Maybe Type)
              _lhsOast =
                  ({-# LINE 358 "ContextAnalisis.ag" #-}
                   Return _exprIast
                   {-# LINE 1725 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 359 "ContextAnalisis.ag" #-}
                   _errorRet
                   ++compTypes ("the method "++ _lhsImethod)
                               (findType _lhsImethod _lhsItMethods _lhsItTypesRet [] [])
                               _exprItype1
                   ++ _exprImessages ++ _exprImessagesType
                   {-# LINE 1734 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 364 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 1739 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 365 "ContextAnalisis.ag" #-}
                   "return "++ _exprIerrorProgram ++ ";"
                    ++ mark _error
                   {-# LINE 1745 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 367 "ContextAnalisis.ag" #-}
                   _exprIoutput ++ "str RR\n"
                   ++ (rep (length _lhsIidsLocal) "unlink\n") ++"ret\n"
                   {-# LINE 1751 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 369 "ContextAnalisis.ag" #-}
                   True
                   {-# LINE 1756 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 370 "ContextAnalisis.ag" #-}
                   False
                   {-# LINE 1761 "ContextAnalisis.hs" #-}
                   )
              _errorRet =
                  ({-# LINE 376 "ContextAnalisis.ag" #-}
                   errorInstruction (_lhsIalgunReturn && not _lhsIfaltaReturn)
                   {-# LINE 1766 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 377 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 1771 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 273 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1776 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 1781 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 274 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1786 "ContextAnalisis.hs" #-}
                   )
              _exprOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1791 "ContextAnalisis.hs" #-}
                   )
              _exprOidsLocal =
                  ({-# LINE 422 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1796 "ContextAnalisis.hs" #-}
                   )
              _exprOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 1801 "ContextAnalisis.hs" #-}
                   )
              _exprOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 1806 "ContextAnalisis.hs" #-}
                   )
              _exprOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 1811 "ContextAnalisis.hs" #-}
                   )
              _exprOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 1816 "ContextAnalisis.hs" #-}
                   )
              _exprOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 1821 "ContextAnalisis.hs" #-}
                   )
              _exprOtypesLocal =
                  ({-# LINE 423 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 1826 "ContextAnalisis.hs" #-}
                   )
              ( _exprIast,_exprIerrorProgram,_exprImessages,_exprImessagesType,_exprIoutput,_exprItype1) =
                  expr_ _exprOaIdDefs _exprOidsLocal _exprOtIdDefs _exprOtMethods _exprOtTypesParam _exprOtTypesRet _exprOtypesDefs _exprOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
-- InstructionsList --------------------------------------------
type InstructionsList = [Instruction]
-- cata
sem_InstructionsList :: InstructionsList ->
                        T_InstructionsList
sem_InstructionsList list =
    (Prelude.foldr sem_InstructionsList_Cons sem_InstructionsList_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_InstructionsList = ([String]) ->
                          Bool ->
                          Bool ->
                          ([[String]]) ->
                          ([String]) ->
                          String ->
                          ([String]) ->
                          ([String]) ->
                          ([[Type]]) ->
                          ([Type]) ->
                          ([Type]) ->
                          ([[Type]]) ->
                          ( Bool,InstructionsList,String,Bool,([[String]]),([String]),([String]),Int,String,([[Type]]))
data Inh_InstructionsList = Inh_InstructionsList {aIdDefs_Inh_InstructionsList :: ([String]),algunReturn_Inh_InstructionsList :: Bool,faltaReturn_Inh_InstructionsList :: Bool,idsLocal_Inh_InstructionsList :: ([[String]]),labels_Inh_InstructionsList :: ([String]),method_Inh_InstructionsList :: String,tIdDefs_Inh_InstructionsList :: ([String]),tMethods_Inh_InstructionsList :: ([String]),tTypesParam_Inh_InstructionsList :: ([[Type]]),tTypesRet_Inh_InstructionsList :: ([Type]),typesDefs_Inh_InstructionsList :: ([Type]),typesLocal_Inh_InstructionsList :: ([[Type]])}
data Syn_InstructionsList = Syn_InstructionsList {algunReturn_Syn_InstructionsList :: Bool,ast_Syn_InstructionsList :: InstructionsList,errorProgram_Syn_InstructionsList :: String,faltaReturn_Syn_InstructionsList :: Bool,idsLocal_Syn_InstructionsList :: ([[String]]),labels_Syn_InstructionsList :: ([String]),messages_Syn_InstructionsList :: ([String]),numVar_Syn_InstructionsList :: Int,output_Syn_InstructionsList :: String,typesLocal_Syn_InstructionsList :: ([[Type]])}
wrap_InstructionsList :: T_InstructionsList ->
                         Inh_InstructionsList ->
                         Syn_InstructionsList
wrap_InstructionsList sem (Inh_InstructionsList _lhsIaIdDefs _lhsIalgunReturn _lhsIfaltaReturn _lhsIidsLocal _lhsIlabels _lhsImethod _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal) =
    (let ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal) = sem _lhsIaIdDefs _lhsIalgunReturn _lhsIfaltaReturn _lhsIidsLocal _lhsIlabels _lhsImethod _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs _lhsItypesLocal
     in  (Syn_InstructionsList _lhsOalgunReturn _lhsOast _lhsOerrorProgram _lhsOfaltaReturn _lhsOidsLocal _lhsOlabels _lhsOmessages _lhsOnumVar _lhsOoutput _lhsOtypesLocal))
sem_InstructionsList_Cons :: T_Instruction ->
                             T_InstructionsList ->
                             T_InstructionsList
sem_InstructionsList_Cons hd_ tl_ =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: InstructionsList
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOnumVar :: Int
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOidsLocal :: ([[String]])
              _lhsOlabels :: ([String])
              _lhsOtypesLocal :: ([[Type]])
              _hdOaIdDefs :: ([String])
              _hdOalgunReturn :: Bool
              _hdOfaltaReturn :: Bool
              _hdOidsLocal :: ([[String]])
              _hdOlabels :: ([String])
              _hdOmethod :: String
              _hdOtIdDefs :: ([String])
              _hdOtMethods :: ([String])
              _hdOtTypesParam :: ([[Type]])
              _hdOtTypesRet :: ([Type])
              _hdOtypesDefs :: ([Type])
              _hdOtypesLocal :: ([[Type]])
              _tlOaIdDefs :: ([String])
              _tlOalgunReturn :: Bool
              _tlOfaltaReturn :: Bool
              _tlOidsLocal :: ([[String]])
              _tlOlabels :: ([String])
              _tlOmethod :: String
              _tlOtIdDefs :: ([String])
              _tlOtMethods :: ([String])
              _tlOtTypesParam :: ([[Type]])
              _tlOtTypesRet :: ([Type])
              _tlOtypesDefs :: ([Type])
              _tlOtypesLocal :: ([[Type]])
              _hdIalgunReturn :: Bool
              _hdIast :: Instruction
              _hdIerrorProgram :: String
              _hdIfaltaReturn :: Bool
              _hdIidsLocal :: ([[String]])
              _hdIlabels :: ([String])
              _hdImessages :: ([String])
              _hdInumVar :: Int
              _hdIoutput :: String
              _hdItypesLocal :: ([[Type]])
              _tlIalgunReturn :: Bool
              _tlIast :: InstructionsList
              _tlIerrorProgram :: String
              _tlIfaltaReturn :: Bool
              _tlIidsLocal :: ([[String]])
              _tlIlabels :: ([String])
              _tlImessages :: ([String])
              _tlInumVar :: Int
              _tlIoutput :: String
              _tlItypesLocal :: ([[Type]])
              _lhsOast =
                  ({-# LINE 253 "ContextAnalisis.ag" #-}
                   _hdIast : _tlIast
                   {-# LINE 1933 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 254 "ContextAnalisis.ag" #-}
                   _hdImessages ++ _tlImessages
                   {-# LINE 1938 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 255 "ContextAnalisis.ag" #-}
                   _hdIerrorProgram ++"\n"++ _tlIerrorProgram
                   {-# LINE 1943 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 256 "ContextAnalisis.ag" #-}
                   _hdIoutput ++ _tlIoutput
                   {-# LINE 1948 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 257 "ContextAnalisis.ag" #-}
                   _hdInumVar + _tlInumVar
                   {-# LINE 1953 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 248 "ContextAnalisis.ag" #-}
                   _tlIalgunReturn
                   {-# LINE 1958 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 249 "ContextAnalisis.ag" #-}
                   _tlIfaltaReturn
                   {-# LINE 1963 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 242 "ContextAnalisis.ag" #-}
                   _tlIidsLocal
                   {-# LINE 1968 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 241 "ContextAnalisis.ag" #-}
                   _tlIlabels
                   {-# LINE 1973 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 243 "ContextAnalisis.ag" #-}
                   _tlItypesLocal
                   {-# LINE 1978 "ContextAnalisis.hs" #-}
                   )
              _hdOaIdDefs =
                  ({-# LINE 268 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 1983 "ContextAnalisis.hs" #-}
                   )
              _hdOalgunReturn =
                  ({-# LINE 279 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 1988 "ContextAnalisis.hs" #-}
                   )
              _hdOfaltaReturn =
                  ({-# LINE 280 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 1993 "ContextAnalisis.hs" #-}
                   )
              _hdOidsLocal =
                  ({-# LINE 273 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 1998 "ContextAnalisis.hs" #-}
                   )
              _hdOlabels =
                  ({-# LINE 272 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 2003 "ContextAnalisis.hs" #-}
                   )
              _hdOmethod =
                  ({-# LINE 278 "ContextAnalisis.ag" #-}
                   _lhsImethod
                   {-# LINE 2008 "ContextAnalisis.hs" #-}
                   )
              _hdOtIdDefs =
                  ({-# LINE 266 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2013 "ContextAnalisis.hs" #-}
                   )
              _hdOtMethods =
                  ({-# LINE 275 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2018 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesParam =
                  ({-# LINE 276 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2023 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesRet =
                  ({-# LINE 277 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2028 "ContextAnalisis.hs" #-}
                   )
              _hdOtypesDefs =
                  ({-# LINE 267 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2033 "ContextAnalisis.hs" #-}
                   )
              _hdOtypesLocal =
                  ({-# LINE 274 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 2038 "ContextAnalisis.hs" #-}
                   )
              _tlOaIdDefs =
                  ({-# LINE 237 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 2043 "ContextAnalisis.hs" #-}
                   )
              _tlOalgunReturn =
                  ({-# LINE 248 "ContextAnalisis.ag" #-}
                   _hdIalgunReturn
                   {-# LINE 2048 "ContextAnalisis.hs" #-}
                   )
              _tlOfaltaReturn =
                  ({-# LINE 249 "ContextAnalisis.ag" #-}
                   _hdIfaltaReturn
                   {-# LINE 2053 "ContextAnalisis.hs" #-}
                   )
              _tlOidsLocal =
                  ({-# LINE 242 "ContextAnalisis.ag" #-}
                   _hdIidsLocal
                   {-# LINE 2058 "ContextAnalisis.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 241 "ContextAnalisis.ag" #-}
                   _hdIlabels
                   {-# LINE 2063 "ContextAnalisis.hs" #-}
                   )
              _tlOmethod =
                  ({-# LINE 247 "ContextAnalisis.ag" #-}
                   _lhsImethod
                   {-# LINE 2068 "ContextAnalisis.hs" #-}
                   )
              _tlOtIdDefs =
                  ({-# LINE 235 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2073 "ContextAnalisis.hs" #-}
                   )
              _tlOtMethods =
                  ({-# LINE 244 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2078 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesParam =
                  ({-# LINE 245 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2083 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesRet =
                  ({-# LINE 246 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2088 "ContextAnalisis.hs" #-}
                   )
              _tlOtypesDefs =
                  ({-# LINE 236 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2093 "ContextAnalisis.hs" #-}
                   )
              _tlOtypesLocal =
                  ({-# LINE 243 "ContextAnalisis.ag" #-}
                   _hdItypesLocal
                   {-# LINE 2098 "ContextAnalisis.hs" #-}
                   )
              ( _hdIalgunReturn,_hdIast,_hdIerrorProgram,_hdIfaltaReturn,_hdIidsLocal,_hdIlabels,_hdImessages,_hdInumVar,_hdIoutput,_hdItypesLocal) =
                  hd_ _hdOaIdDefs _hdOalgunReturn _hdOfaltaReturn _hdOidsLocal _hdOlabels _hdOmethod _hdOtIdDefs _hdOtMethods _hdOtTypesParam _hdOtTypesRet _hdOtypesDefs _hdOtypesLocal
              ( _tlIalgunReturn,_tlIast,_tlIerrorProgram,_tlIfaltaReturn,_tlIidsLocal,_tlIlabels,_tlImessages,_tlInumVar,_tlIoutput,_tlItypesLocal) =
                  tl_ _tlOaIdDefs _tlOalgunReturn _tlOfaltaReturn _tlOidsLocal _tlOlabels _tlOmethod _tlOtIdDefs _tlOtMethods _tlOtTypesParam _tlOtTypesRet _tlOtypesDefs _tlOtypesLocal
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
sem_InstructionsList_Nil :: T_InstructionsList
sem_InstructionsList_Nil =
    (\ _lhsIaIdDefs
       _lhsIalgunReturn
       _lhsIfaltaReturn
       _lhsIidsLocal
       _lhsIlabels
       _lhsImethod
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: InstructionsList
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOnumVar :: Int
              _lhsOalgunReturn :: Bool
              _lhsOfaltaReturn :: Bool
              _lhsOidsLocal :: ([[String]])
              _lhsOlabels :: ([String])
              _lhsOtypesLocal :: ([[Type]])
              _lhsOast =
                  ({-# LINE 258 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2132 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 259 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2137 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 260 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2142 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 261 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2147 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumVar =
                  ({-# LINE 262 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 2152 "ContextAnalisis.hs" #-}
                   )
              _lhsOalgunReturn =
                  ({-# LINE 248 "ContextAnalisis.ag" #-}
                   _lhsIalgunReturn
                   {-# LINE 2157 "ContextAnalisis.hs" #-}
                   )
              _lhsOfaltaReturn =
                  ({-# LINE 249 "ContextAnalisis.ag" #-}
                   _lhsIfaltaReturn
                   {-# LINE 2162 "ContextAnalisis.hs" #-}
                   )
              _lhsOidsLocal =
                  ({-# LINE 242 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 2167 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 241 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 2172 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesLocal =
                  ({-# LINE 243 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 2177 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOalgunReturn,_lhsOast,_lhsOerrorProgram,_lhsOfaltaReturn,_lhsOidsLocal,_lhsOlabels,_lhsOmessages,_lhsOnumVar,_lhsOoutput,_lhsOtypesLocal)))
-- LocalParams -------------------------------------------------
type LocalParams = [Expression]
-- cata
sem_LocalParams :: LocalParams ->
                   T_LocalParams
sem_LocalParams list =
    (Prelude.foldr sem_LocalParams_Cons sem_LocalParams_Nil (Prelude.map sem_Expression list))
-- semantic domain
type T_LocalParams = ([String]) ->
                     ([[String]]) ->
                     ([String]) ->
                     ([String]) ->
                     ([[Type]]) ->
                     ([Type]) ->
                     ([Type]) ->
                     ([Type]) ->
                     ([[Type]]) ->
                     ( LocalParams,String,([String]),Int,String)
data Inh_LocalParams = Inh_LocalParams {aIdDefs_Inh_LocalParams :: ([String]),idsLocal_Inh_LocalParams :: ([[String]]),tIdDefs_Inh_LocalParams :: ([String]),tMethods_Inh_LocalParams :: ([String]),tTypesParam_Inh_LocalParams :: ([[Type]]),tTypesRet_Inh_LocalParams :: ([Type]),typeParam_Inh_LocalParams :: ([Type]),typesDefs_Inh_LocalParams :: ([Type]),typesLocal_Inh_LocalParams :: ([[Type]])}
data Syn_LocalParams = Syn_LocalParams {ast_Syn_LocalParams :: LocalParams,errorProgram_Syn_LocalParams :: String,messages_Syn_LocalParams :: ([String]),numParam_Syn_LocalParams :: Int,output_Syn_LocalParams :: String}
wrap_LocalParams :: T_LocalParams ->
                    Inh_LocalParams ->
                    Syn_LocalParams
wrap_LocalParams sem (Inh_LocalParams _lhsIaIdDefs _lhsIidsLocal _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypeParam _lhsItypesDefs _lhsItypesLocal) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOnumParam,_lhsOoutput) = sem _lhsIaIdDefs _lhsIidsLocal _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypeParam _lhsItypesDefs _lhsItypesLocal
     in  (Syn_LocalParams _lhsOast _lhsOerrorProgram _lhsOmessages _lhsOnumParam _lhsOoutput))
sem_LocalParams_Cons :: T_Expression ->
                        T_LocalParams ->
                        T_LocalParams
sem_LocalParams_Cons hd_ tl_ =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypeParam
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: LocalParams
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _tlOtypeParam :: ([Type])
              _lhsOnumParam :: Int
              _hdOaIdDefs :: ([String])
              _hdOidsLocal :: ([[String]])
              _hdOtIdDefs :: ([String])
              _hdOtMethods :: ([String])
              _hdOtTypesParam :: ([[Type]])
              _hdOtTypesRet :: ([Type])
              _hdOtypesDefs :: ([Type])
              _hdOtypesLocal :: ([[Type]])
              _tlOaIdDefs :: ([String])
              _tlOidsLocal :: ([[String]])
              _tlOtIdDefs :: ([String])
              _tlOtMethods :: ([String])
              _tlOtTypesParam :: ([[Type]])
              _tlOtTypesRet :: ([Type])
              _tlOtypesDefs :: ([Type])
              _tlOtypesLocal :: ([[Type]])
              _hdIast :: Expression
              _hdIerrorProgram :: String
              _hdImessages :: ([String])
              _hdImessagesType :: ([String])
              _hdIoutput :: String
              _hdItype1 :: (Maybe Type)
              _tlIast :: LocalParams
              _tlIerrorProgram :: String
              _tlImessages :: ([String])
              _tlInumParam :: Int
              _tlIoutput :: String
              _lhsOast =
                  ({-# LINE 396 "ContextAnalisis.ag" #-}
                   _hdIast : _tlIast
                   {-# LINE 2255 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 397 "ContextAnalisis.ag" #-}
                   _hdImessages ++ _tlImessages
                   ++ compTypes _hdIerrorProgram
                                (primerType _lhsItypeParam) _hdItype1
                   ++ _hdImessagesType
                   {-# LINE 2263 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 401 "ContextAnalisis.ag" #-}
                   _hdIerrorProgram ++ commaSeparated _tlIerrorProgram
                   {-# LINE 2268 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 402 "ContextAnalisis.ag" #-}
                   _hdIoutput ++ _tlIoutput
                   {-# LINE 2273 "ContextAnalisis.hs" #-}
                   )
              _tlOtypeParam =
                  ({-# LINE 403 "ContextAnalisis.ag" #-}
                   drop 1 _lhsItypeParam
                   {-# LINE 2278 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumParam =
                  ({-# LINE 404 "ContextAnalisis.ag" #-}
                   1+ _tlInumParam
                   {-# LINE 2283 "ContextAnalisis.hs" #-}
                   )
              _hdOaIdDefs =
                  ({-# LINE 416 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 2288 "ContextAnalisis.hs" #-}
                   )
              _hdOidsLocal =
                  ({-# LINE 422 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 2293 "ContextAnalisis.hs" #-}
                   )
              _hdOtIdDefs =
                  ({-# LINE 414 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2298 "ContextAnalisis.hs" #-}
                   )
              _hdOtMethods =
                  ({-# LINE 424 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2303 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesParam =
                  ({-# LINE 425 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2308 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesRet =
                  ({-# LINE 426 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2313 "ContextAnalisis.hs" #-}
                   )
              _hdOtypesDefs =
                  ({-# LINE 415 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2318 "ContextAnalisis.hs" #-}
                   )
              _hdOtypesLocal =
                  ({-# LINE 423 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 2323 "ContextAnalisis.hs" #-}
                   )
              _tlOaIdDefs =
                  ({-# LINE 383 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 2328 "ContextAnalisis.hs" #-}
                   )
              _tlOidsLocal =
                  ({-# LINE 387 "ContextAnalisis.ag" #-}
                   _lhsIidsLocal
                   {-# LINE 2333 "ContextAnalisis.hs" #-}
                   )
              _tlOtIdDefs =
                  ({-# LINE 381 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2338 "ContextAnalisis.hs" #-}
                   )
              _tlOtMethods =
                  ({-# LINE 389 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2343 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesParam =
                  ({-# LINE 390 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2348 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesRet =
                  ({-# LINE 391 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2353 "ContextAnalisis.hs" #-}
                   )
              _tlOtypesDefs =
                  ({-# LINE 382 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2358 "ContextAnalisis.hs" #-}
                   )
              _tlOtypesLocal =
                  ({-# LINE 388 "ContextAnalisis.ag" #-}
                   _lhsItypesLocal
                   {-# LINE 2363 "ContextAnalisis.hs" #-}
                   )
              ( _hdIast,_hdIerrorProgram,_hdImessages,_hdImessagesType,_hdIoutput,_hdItype1) =
                  hd_ _hdOaIdDefs _hdOidsLocal _hdOtIdDefs _hdOtMethods _hdOtTypesParam _hdOtTypesRet _hdOtypesDefs _hdOtypesLocal
              ( _tlIast,_tlIerrorProgram,_tlImessages,_tlInumParam,_tlIoutput) =
                  tl_ _tlOaIdDefs _tlOidsLocal _tlOtIdDefs _tlOtMethods _tlOtTypesParam _tlOtTypesRet _tlOtypeParam _tlOtypesDefs _tlOtypesLocal
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOnumParam,_lhsOoutput)))
sem_LocalParams_Nil :: T_LocalParams
sem_LocalParams_Nil =
    (\ _lhsIaIdDefs
       _lhsIidsLocal
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypeParam
       _lhsItypesDefs
       _lhsItypesLocal ->
         (let _lhsOast :: LocalParams
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOnumParam :: Int
              _lhsOast =
                  ({-# LINE 405 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2389 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 406 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2394 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 407 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2399 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 408 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2404 "ContextAnalisis.hs" #-}
                   )
              _lhsOnumParam =
                  ({-# LINE 409 "ContextAnalisis.ag" #-}
                   0
                   {-# LINE 2409 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOnumParam,_lhsOoutput)))
-- Member ------------------------------------------------------
data Member = Atribute (Type) (String)
            | Method (Type) (String) (ParametersList) (InstructionsList)
            deriving ( Show)
-- cata
sem_Member :: Member ->
              T_Member
sem_Member (Atribute _type1 _ident) =
    (sem_Member_Atribute (sem_Type _type1) _ident)
sem_Member (Method _type1 _ident _parametersList _instrs) =
    (sem_Member_Method (sem_Type _type1) _ident (sem_ParametersList _parametersList) (sem_InstructionsList _instrs))
-- semantic domain
type T_Member = ([String]) ->
                ([String]) ->
                ([String]) ->
                ([String]) ->
                ([[Type]]) ->
                ([Type]) ->
                ([Type]) ->
                ( Member,String,([String]),([String]),([String]),([String]),String,([Type]),([Type]),([[Type]]))
data Inh_Member = Inh_Member {aIdDefs_Inh_Member :: ([String]),labels_Inh_Member :: ([String]),tIdDefs_Inh_Member :: ([String]),tMethods_Inh_Member :: ([String]),tTypesParam_Inh_Member :: ([[Type]]),tTypesRet_Inh_Member :: ([Type]),typesDefs_Inh_Member :: ([Type])}
data Syn_Member = Syn_Member {ast_Syn_Member :: Member,errorProgram_Syn_Member :: String,idDefs_Syn_Member :: ([String]),labels_Syn_Member :: ([String]),messages_Syn_Member :: ([String]),method_Syn_Member :: ([String]),output_Syn_Member :: String,typeRet_Syn_Member :: ([Type]),types_Syn_Member :: ([Type]),typesParam_Syn_Member :: ([[Type]])}
wrap_Member :: T_Member ->
               Inh_Member ->
               Syn_Member
wrap_Member sem (Inh_Member _lhsIaIdDefs _lhsIlabels _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethod,_lhsOoutput,_lhsOtypeRet,_lhsOtypes,_lhsOtypesParam) = sem _lhsIaIdDefs _lhsIlabels _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs
     in  (Syn_Member _lhsOast _lhsOerrorProgram _lhsOidDefs _lhsOlabels _lhsOmessages _lhsOmethod _lhsOoutput _lhsOtypeRet _lhsOtypes _lhsOtypesParam))
sem_Member_Atribute :: T_Type ->
                       String ->
                       T_Member
sem_Member_Atribute type1_ ident_ =
    (\ _lhsIaIdDefs
       _lhsIlabels
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs ->
         (let _lhsOast :: Member
              _lhsOidDefs :: ([String])
              _lhsOtypes :: ([Type])
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOmethod :: ([String])
              _lhsOtypesParam :: ([[Type]])
              _lhsOtypeRet :: ([Type])
              _lhsOlabels :: ([String])
              _type1Iast :: Type
              _type1IerrorProgram :: String
              _lhsOast =
                  ({-# LINE 146 "ContextAnalisis.ag" #-}
                   Atribute _type1Iast ident_
                   {-# LINE 2466 "ContextAnalisis.hs" #-}
                   )
              _lhsOidDefs =
                  ({-# LINE 147 "ContextAnalisis.ag" #-}
                   [ident_]
                   {-# LINE 2471 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypes =
                  ({-# LINE 148 "ContextAnalisis.ag" #-}
                   [_type1Iast]
                   {-# LINE 2476 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 149 "ContextAnalisis.ag" #-}
                   messageDecl ident_ [_lhsIaIdDefs] ++ variableVoid ident_ _type1Iast
                   {-# LINE 2481 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 150 "ContextAnalisis.ag" #-}
                   _error
                   {-# LINE 2486 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 151 "ContextAnalisis.ag" #-}
                   _type1IerrorProgram ++" "++ ident_++";"
                    ++ mark _error
                   {-# LINE 2492 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 153 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2497 "ContextAnalisis.hs" #-}
                   )
              _lhsOmethod =
                  ({-# LINE 154 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2502 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesParam =
                  ({-# LINE 155 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2507 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypeRet =
                  ({-# LINE 156 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2512 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 137 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 2517 "ContextAnalisis.hs" #-}
                   )
              ( _type1Iast,_type1IerrorProgram) =
                  type1_
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethod,_lhsOoutput,_lhsOtypeRet,_lhsOtypes,_lhsOtypesParam)))
sem_Member_Method :: T_Type ->
                     String ->
                     T_ParametersList ->
                     T_InstructionsList ->
                     T_Member
sem_Member_Method type1_ ident_ parametersList_ instrs_ =
    (\ _lhsIaIdDefs
       _lhsIlabels
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs ->
         (let _lhsOast :: Member
              _lhsOmessages :: ([String])
              _lhsOidDefs :: ([String])
              _lhsOtypes :: ([Type])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _instrsOidsLocal :: ([[String]])
              _instrsOtypesLocal :: ([[Type]])
              _lhsOmethod :: ([String])
              _lhsOtypesParam :: ([[Type]])
              _lhsOtypeRet :: ([Type])
              _instrsOmethod :: String
              _instrsOlabels :: ([String])
              _instrsOalgunReturn :: Bool
              _instrsOfaltaReturn :: Bool
              _lhsOlabels :: ([String])
              _instrsOaIdDefs :: ([String])
              _instrsOtIdDefs :: ([String])
              _instrsOtMethods :: ([String])
              _instrsOtTypesParam :: ([[Type]])
              _instrsOtTypesRet :: ([Type])
              _instrsOtypesDefs :: ([Type])
              _type1Iast :: Type
              _type1IerrorProgram :: String
              _parametersListIast :: ParametersList
              _parametersListIerrorProgram :: String
              _parametersListIids :: ([String])
              _parametersListImessages :: ([String])
              _parametersListInumParam :: Int
              _parametersListItype1 :: ([Type])
              _instrsIalgunReturn :: Bool
              _instrsIast :: InstructionsList
              _instrsIerrorProgram :: String
              _instrsIfaltaReturn :: Bool
              _instrsIidsLocal :: ([[String]])
              _instrsIlabels :: ([String])
              _instrsImessages :: ([String])
              _instrsInumVar :: Int
              _instrsIoutput :: String
              _instrsItypesLocal :: ([[Type]])
              _lhsOast =
                  ({-# LINE 157 "ContextAnalisis.ag" #-}
                   Method _type1Iast  ident_  _parametersListIast _instrsIast
                   {-# LINE 2578 "ContextAnalisis.hs" #-}
                   )
              _error =
                  ({-# LINE 158 "ContextAnalisis.ag" #-}
                   errorReturn ident_ _type1Iast (_instrsIalgunReturn)
                   ++ _parametersListImessages
                   {-# LINE 2584 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 160 "ContextAnalisis.ag" #-}
                   _error ++ _instrsImessages
                   {-# LINE 2589 "ContextAnalisis.hs" #-}
                   )
              _lhsOidDefs =
                  ({-# LINE 161 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2594 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypes =
                  ({-# LINE 162 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2599 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 163 "ContextAnalisis.ag" #-}
                   _type1IerrorProgram ++" "++ ident_++"("
                    ++ _parametersListIerrorProgram++"){"
                    ++ mark _error ++"\n"
                    ++ (tab _instrsIerrorProgram)++"}"
                   {-# LINE 2607 "ContextAnalisis.hs" #-}
                   )
              _methodLabel =
                  ({-# LINE 167 "ContextAnalisis.ag" #-}
                   "ET"++ ident_ ++ "0000"
                   {-# LINE 2612 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 168 "ContextAnalisis.ag" #-}
                   _methodLabel ++": link "++(show _numLoc)++"\n"
                    ++ loadParams _parametersListInumParam
                    ++ _instrsIoutput ++(if _instrsIfaltaReturn
                                          then "unlink\n" ++"ret\n"
                                          else "")
                   {-# LINE 2621 "ContextAnalisis.hs" #-}
                   )
              _instrsOidsLocal =
                  ({-# LINE 173 "ContextAnalisis.ag" #-}
                   [_parametersListIids]
                   {-# LINE 2626 "ContextAnalisis.hs" #-}
                   )
              _instrsOtypesLocal =
                  ({-# LINE 174 "ContextAnalisis.ag" #-}
                   [_parametersListItype1]
                   {-# LINE 2631 "ContextAnalisis.hs" #-}
                   )
              _numLoc =
                  ({-# LINE 175 "ContextAnalisis.ag" #-}
                   length (head _instrsIidsLocal)
                   {-# LINE 2636 "ContextAnalisis.hs" #-}
                   )
              _lhsOmethod =
                  ({-# LINE 176 "ContextAnalisis.ag" #-}
                   [ident_]
                   {-# LINE 2641 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesParam =
                  ({-# LINE 177 "ContextAnalisis.ag" #-}
                   [_parametersListItype1]
                   {-# LINE 2646 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypeRet =
                  ({-# LINE 178 "ContextAnalisis.ag" #-}
                   [_type1Iast]
                   {-# LINE 2651 "ContextAnalisis.hs" #-}
                   )
              _instrsOmethod =
                  ({-# LINE 179 "ContextAnalisis.ag" #-}
                   ident_
                   {-# LINE 2656 "ContextAnalisis.hs" #-}
                   )
              _instrsOlabels =
                  ({-# LINE 180 "ContextAnalisis.ag" #-}
                   [_methodLabel]
                   {-# LINE 2661 "ContextAnalisis.hs" #-}
                   )
              _instrsOalgunReturn =
                  ({-# LINE 181 "ContextAnalisis.ag" #-}
                   False
                   {-# LINE 2666 "ContextAnalisis.hs" #-}
                   )
              _instrsOfaltaReturn =
                  ({-# LINE 182 "ContextAnalisis.ag" #-}
                   True
                   {-# LINE 2671 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 137 "ContextAnalisis.ag" #-}
                   _instrsIlabels
                   {-# LINE 2676 "ContextAnalisis.hs" #-}
                   )
              _instrsOaIdDefs =
                  ({-# LINE 237 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 2681 "ContextAnalisis.hs" #-}
                   )
              _instrsOtIdDefs =
                  ({-# LINE 235 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2686 "ContextAnalisis.hs" #-}
                   )
              _instrsOtMethods =
                  ({-# LINE 244 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2691 "ContextAnalisis.hs" #-}
                   )
              _instrsOtTypesParam =
                  ({-# LINE 245 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2696 "ContextAnalisis.hs" #-}
                   )
              _instrsOtTypesRet =
                  ({-# LINE 246 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2701 "ContextAnalisis.hs" #-}
                   )
              _instrsOtypesDefs =
                  ({-# LINE 236 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2706 "ContextAnalisis.hs" #-}
                   )
              ( _type1Iast,_type1IerrorProgram) =
                  type1_
              ( _parametersListIast,_parametersListIerrorProgram,_parametersListIids,_parametersListImessages,_parametersListInumParam,_parametersListItype1) =
                  parametersList_
              ( _instrsIalgunReturn,_instrsIast,_instrsIerrorProgram,_instrsIfaltaReturn,_instrsIidsLocal,_instrsIlabels,_instrsImessages,_instrsInumVar,_instrsIoutput,_instrsItypesLocal) =
                  instrs_ _instrsOaIdDefs _instrsOalgunReturn _instrsOfaltaReturn _instrsOidsLocal _instrsOlabels _instrsOmethod _instrsOtIdDefs _instrsOtMethods _instrsOtTypesParam _instrsOtTypesRet _instrsOtypesDefs _instrsOtypesLocal
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethod,_lhsOoutput,_lhsOtypeRet,_lhsOtypes,_lhsOtypesParam)))
-- MembersList -------------------------------------------------
type MembersList = [Member]
-- cata
sem_MembersList :: MembersList ->
                   T_MembersList
sem_MembersList list =
    (Prelude.foldr sem_MembersList_Cons sem_MembersList_Nil (Prelude.map sem_Member list))
-- semantic domain
type T_MembersList = ([String]) ->
                     ([String]) ->
                     ([String]) ->
                     ([String]) ->
                     ([[Type]]) ->
                     ([Type]) ->
                     ([Type]) ->
                     ( MembersList,String,([String]),([String]),([String]),([String]),String,([Type]),([[Type]]),([Type]))
data Inh_MembersList = Inh_MembersList {aIdDefs_Inh_MembersList :: ([String]),labels_Inh_MembersList :: ([String]),tIdDefs_Inh_MembersList :: ([String]),tMethods_Inh_MembersList :: ([String]),tTypesParam_Inh_MembersList :: ([[Type]]),tTypesRet_Inh_MembersList :: ([Type]),typesDefs_Inh_MembersList :: ([Type])}
data Syn_MembersList = Syn_MembersList {ast_Syn_MembersList :: MembersList,errorProgram_Syn_MembersList :: String,idDefs_Syn_MembersList :: ([String]),labels_Syn_MembersList :: ([String]),messages_Syn_MembersList :: ([String]),methods_Syn_MembersList :: ([String]),output_Syn_MembersList :: String,types_Syn_MembersList :: ([Type]),typesParam_Syn_MembersList :: ([[Type]]),typesRet_Syn_MembersList :: ([Type])}
wrap_MembersList :: T_MembersList ->
                    Inh_MembersList ->
                    Syn_MembersList
wrap_MembersList sem (Inh_MembersList _lhsIaIdDefs _lhsIlabels _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethods,_lhsOoutput,_lhsOtypes,_lhsOtypesParam,_lhsOtypesRet) = sem _lhsIaIdDefs _lhsIlabels _lhsItIdDefs _lhsItMethods _lhsItTypesParam _lhsItTypesRet _lhsItypesDefs
     in  (Syn_MembersList _lhsOast _lhsOerrorProgram _lhsOidDefs _lhsOlabels _lhsOmessages _lhsOmethods _lhsOoutput _lhsOtypes _lhsOtypesParam _lhsOtypesRet))
sem_MembersList_Cons :: T_Member ->
                        T_MembersList ->
                        T_MembersList
sem_MembersList_Cons hd_ tl_ =
    (\ _lhsIaIdDefs
       _lhsIlabels
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs ->
         (let _lhsOast :: MembersList
              _lhsOidDefs :: ([String])
              _lhsOtypes :: ([Type])
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _tlOaIdDefs :: ([String])
              _lhsOoutput :: String
              _lhsOmethods :: ([String])
              _lhsOtypesParam :: ([[Type]])
              _lhsOtypesRet :: ([Type])
              _lhsOlabels :: ([String])
              _hdOaIdDefs :: ([String])
              _hdOlabels :: ([String])
              _hdOtIdDefs :: ([String])
              _hdOtMethods :: ([String])
              _hdOtTypesParam :: ([[Type]])
              _hdOtTypesRet :: ([Type])
              _hdOtypesDefs :: ([Type])
              _tlOlabels :: ([String])
              _tlOtIdDefs :: ([String])
              _tlOtMethods :: ([String])
              _tlOtTypesParam :: ([[Type]])
              _tlOtTypesRet :: ([Type])
              _tlOtypesDefs :: ([Type])
              _hdIast :: Member
              _hdIerrorProgram :: String
              _hdIidDefs :: ([String])
              _hdIlabels :: ([String])
              _hdImessages :: ([String])
              _hdImethod :: ([String])
              _hdIoutput :: String
              _hdItypeRet :: ([Type])
              _hdItypes :: ([Type])
              _hdItypesParam :: ([[Type]])
              _tlIast :: MembersList
              _tlIerrorProgram :: String
              _tlIidDefs :: ([String])
              _tlIlabels :: ([String])
              _tlImessages :: ([String])
              _tlImethods :: ([String])
              _tlIoutput :: String
              _tlItypes :: ([Type])
              _tlItypesParam :: ([[Type]])
              _tlItypesRet :: ([Type])
              _lhsOast =
                  ({-# LINE 107 "ContextAnalisis.ag" #-}
                   _hdIast : _tlIast
                   {-# LINE 2797 "ContextAnalisis.hs" #-}
                   )
              _lhsOidDefs =
                  ({-# LINE 108 "ContextAnalisis.ag" #-}
                   _hdIidDefs ++ _tlIidDefs
                   {-# LINE 2802 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypes =
                  ({-# LINE 109 "ContextAnalisis.ag" #-}
                   _hdItypes ++ _tlItypes
                   {-# LINE 2807 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 110 "ContextAnalisis.ag" #-}
                   _hdImessages ++ _tlImessages
                   {-# LINE 2812 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 111 "ContextAnalisis.ag" #-}
                   " \n"++ _hdIerrorProgram ++"\n"++ _tlIerrorProgram
                   {-# LINE 2817 "ContextAnalisis.hs" #-}
                   )
              _tlOaIdDefs =
                  ({-# LINE 112 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs ++ _hdIidDefs
                   {-# LINE 2822 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 113 "ContextAnalisis.ag" #-}
                   _hdIoutput ++ _tlIoutput
                   {-# LINE 2827 "ContextAnalisis.hs" #-}
                   )
              _lhsOmethods =
                  ({-# LINE 114 "ContextAnalisis.ag" #-}
                   _hdImethod ++ _tlImethods
                   {-# LINE 2832 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesParam =
                  ({-# LINE 115 "ContextAnalisis.ag" #-}
                   _hdItypesParam ++ _tlItypesParam
                   {-# LINE 2837 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesRet =
                  ({-# LINE 116 "ContextAnalisis.ag" #-}
                   _hdItypeRet ++ _tlItypesRet
                   {-# LINE 2842 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 98 "ContextAnalisis.ag" #-}
                   _tlIlabels
                   {-# LINE 2847 "ContextAnalisis.hs" #-}
                   )
              _hdOaIdDefs =
                  ({-# LINE 131 "ContextAnalisis.ag" #-}
                   _lhsIaIdDefs
                   {-# LINE 2852 "ContextAnalisis.hs" #-}
                   )
              _hdOlabels =
                  ({-# LINE 137 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 2857 "ContextAnalisis.hs" #-}
                   )
              _hdOtIdDefs =
                  ({-# LINE 129 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2862 "ContextAnalisis.hs" #-}
                   )
              _hdOtMethods =
                  ({-# LINE 141 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2867 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesParam =
                  ({-# LINE 142 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2872 "ContextAnalisis.hs" #-}
                   )
              _hdOtTypesRet =
                  ({-# LINE 143 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2877 "ContextAnalisis.hs" #-}
                   )
              _hdOtypesDefs =
                  ({-# LINE 130 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2882 "ContextAnalisis.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 98 "ContextAnalisis.ag" #-}
                   _hdIlabels
                   {-# LINE 2887 "ContextAnalisis.hs" #-}
                   )
              _tlOtIdDefs =
                  ({-# LINE 90 "ContextAnalisis.ag" #-}
                   _lhsItIdDefs
                   {-# LINE 2892 "ContextAnalisis.hs" #-}
                   )
              _tlOtMethods =
                  ({-# LINE 102 "ContextAnalisis.ag" #-}
                   _lhsItMethods
                   {-# LINE 2897 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesParam =
                  ({-# LINE 103 "ContextAnalisis.ag" #-}
                   _lhsItTypesParam
                   {-# LINE 2902 "ContextAnalisis.hs" #-}
                   )
              _tlOtTypesRet =
                  ({-# LINE 104 "ContextAnalisis.ag" #-}
                   _lhsItTypesRet
                   {-# LINE 2907 "ContextAnalisis.hs" #-}
                   )
              _tlOtypesDefs =
                  ({-# LINE 91 "ContextAnalisis.ag" #-}
                   _lhsItypesDefs
                   {-# LINE 2912 "ContextAnalisis.hs" #-}
                   )
              ( _hdIast,_hdIerrorProgram,_hdIidDefs,_hdIlabels,_hdImessages,_hdImethod,_hdIoutput,_hdItypeRet,_hdItypes,_hdItypesParam) =
                  hd_ _hdOaIdDefs _hdOlabels _hdOtIdDefs _hdOtMethods _hdOtTypesParam _hdOtTypesRet _hdOtypesDefs
              ( _tlIast,_tlIerrorProgram,_tlIidDefs,_tlIlabels,_tlImessages,_tlImethods,_tlIoutput,_tlItypes,_tlItypesParam,_tlItypesRet) =
                  tl_ _tlOaIdDefs _tlOlabels _tlOtIdDefs _tlOtMethods _tlOtTypesParam _tlOtTypesRet _tlOtypesDefs
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethods,_lhsOoutput,_lhsOtypes,_lhsOtypesParam,_lhsOtypesRet)))
sem_MembersList_Nil :: T_MembersList
sem_MembersList_Nil =
    (\ _lhsIaIdDefs
       _lhsIlabels
       _lhsItIdDefs
       _lhsItMethods
       _lhsItTypesParam
       _lhsItTypesRet
       _lhsItypesDefs ->
         (let _lhsOast :: MembersList
              _lhsOidDefs :: ([String])
              _lhsOtypes :: ([Type])
              _lhsOmessages :: ([String])
              _lhsOerrorProgram :: String
              _lhsOoutput :: String
              _lhsOmethods :: ([String])
              _lhsOtypesParam :: ([[Type]])
              _lhsOtypesRet :: ([Type])
              _lhsOlabels :: ([String])
              _lhsOast =
                  ({-# LINE 117 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2941 "ContextAnalisis.hs" #-}
                   )
              _lhsOidDefs =
                  ({-# LINE 118 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2946 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypes =
                  ({-# LINE 119 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2951 "ContextAnalisis.hs" #-}
                   )
              _lhsOmessages =
                  ({-# LINE 120 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2956 "ContextAnalisis.hs" #-}
                   )
              _lhsOerrorProgram =
                  ({-# LINE 121 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2961 "ContextAnalisis.hs" #-}
                   )
              _lhsOoutput =
                  ({-# LINE 122 "ContextAnalisis.ag" #-}
                   ""
                   {-# LINE 2966 "ContextAnalisis.hs" #-}
                   )
              _lhsOmethods =
                  ({-# LINE 123 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2971 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesParam =
                  ({-# LINE 124 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2976 "ContextAnalisis.hs" #-}
                   )
              _lhsOtypesRet =
                  ({-# LINE 125 "ContextAnalisis.ag" #-}
                   []
                   {-# LINE 2981 "ContextAnalisis.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 98 "ContextAnalisis.ag" #-}
                   _lhsIlabels
                   {-# LINE 2986 "ContextAnalisis.hs" #-}
                   )
          in  ( _lhsOast,_lhsOerrorProgram,_lhsOidDefs,_lhsOlabels,_lhsOmessages,_lhsOmethods,_lhsOoutput,_lhsOtypes,_lhsOtypesParam,_lhsOtypesRet)))
-- Op ----------------------------------------------------------
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
-- cata
sem_Op :: Op ->
          T_Op
sem_Op (Or) =
    (sem_Op_Or)
sem_Op (And) =
    (sem_Op_And)
sem_Op (Equal) =
    (sem_Op_Equal)
sem_Op (NotEqual) =
    (sem_Op_NotEqual)
sem_Op (Less) =
    (sem_Op_Less)
sem_Op (Greater) =
    (sem_Op_Greater)
sem_Op (LessEqual) =
    (sem_Op_LessEqual)
sem_Op (GreaterEqual) =
    (sem_Op_GreaterEqual)
sem_Op (Add) =
    (sem_Op_Add)
sem_Op (Sub) =
    (sem_Op_Sub)
sem_Op (Product) =
    (sem_Op_Product)
sem_Op (Division) =
    (sem_Op_Division)
sem_Op (Module) =
    (sem_Op_Module)
-- semantic domain
type T_Op = ( Op,String,(Maybe Type->Maybe Type -> [String] -> [String] -> [String]),String,(Maybe Type))
data Inh_Op = Inh_Op {}
data Syn_Op = Syn_Op {ast_Syn_Op :: Op,errorProgram_Syn_Op :: String,mType_Syn_Op :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String]),output_Syn_Op :: String,type1_Syn_Op :: (Maybe Type)}
wrap_Op :: T_Op ->
           Inh_Op ->
           Syn_Op
wrap_Op sem (Inh_Op) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1) = sem
     in  (Syn_Op _lhsOast _lhsOerrorProgram _lhsOmType _lhsOoutput _lhsOtype1))
sem_Op_Or :: T_Op
sem_Op_Or =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 495 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3053 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 496 "ContextAnalisis.ag" #-}
              messageType1 "||" BooleanType
              {-# LINE 3058 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 497 "ContextAnalisis.ag" #-}
              "||"
              {-# LINE 3063 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 498 "ContextAnalisis.ag" #-}
              "or\n"
              {-# LINE 3068 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Or
              {-# LINE 3073 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3078 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_And :: T_Op
sem_Op_And =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 499 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3091 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 500 "ContextAnalisis.ag" #-}
              messageType1 "&&" BooleanType
              {-# LINE 3096 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 501 "ContextAnalisis.ag" #-}
              "&&"
              {-# LINE 3101 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 502 "ContextAnalisis.ag" #-}
              "and\n"
              {-# LINE 3106 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              And
              {-# LINE 3111 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3116 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Equal :: T_Op
sem_Op_Equal =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 503 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3129 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 504 "ContextAnalisis.ag" #-}
              messageType2 "=="
              {-# LINE 3134 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 505 "ContextAnalisis.ag" #-}
              "=="
              {-# LINE 3139 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 506 "ContextAnalisis.ag" #-}
              "eq\n"
              {-# LINE 3144 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Equal
              {-# LINE 3149 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3154 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_NotEqual :: T_Op
sem_Op_NotEqual =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 507 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3167 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 508 "ContextAnalisis.ag" #-}
              messageType2 "!="
              {-# LINE 3172 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 509 "ContextAnalisis.ag" #-}
              "!="
              {-# LINE 3177 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 510 "ContextAnalisis.ag" #-}
              "ne\n"
              {-# LINE 3182 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              NotEqual
              {-# LINE 3187 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3192 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Less :: T_Op
sem_Op_Less =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 511 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3205 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 512 "ContextAnalisis.ag" #-}
              messageType3 "<"
              {-# LINE 3210 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 513 "ContextAnalisis.ag" #-}
              "<"
              {-# LINE 3215 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 514 "ContextAnalisis.ag" #-}
              "lt\n"
              {-# LINE 3220 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Less
              {-# LINE 3225 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3230 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Greater :: T_Op
sem_Op_Greater =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 515 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3243 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 516 "ContextAnalisis.ag" #-}
              messageType3 ">"
              {-# LINE 3248 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 517 "ContextAnalisis.ag" #-}
              ">"
              {-# LINE 3253 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 518 "ContextAnalisis.ag" #-}
              "gt\n"
              {-# LINE 3258 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Greater
              {-# LINE 3263 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3268 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_LessEqual :: T_Op
sem_Op_LessEqual =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 519 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3281 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 520 "ContextAnalisis.ag" #-}
              messageType3 "<="
              {-# LINE 3286 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 521 "ContextAnalisis.ag" #-}
              "<="
              {-# LINE 3291 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 522 "ContextAnalisis.ag" #-}
              "le\n"
              {-# LINE 3296 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              LessEqual
              {-# LINE 3301 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3306 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_GreaterEqual :: T_Op
sem_Op_GreaterEqual =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 523 "ContextAnalisis.ag" #-}
              Just BooleanType
              {-# LINE 3319 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 524 "ContextAnalisis.ag" #-}
              messageType3 ">="
              {-# LINE 3324 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 525 "ContextAnalisis.ag" #-}
              ">="
              {-# LINE 3329 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 526 "ContextAnalisis.ag" #-}
              "ge\n"
              {-# LINE 3334 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              GreaterEqual
              {-# LINE 3339 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3344 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Add :: T_Op
sem_Op_Add =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 475 "ContextAnalisis.ag" #-}
              Just IntegerType
              {-# LINE 3357 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 476 "ContextAnalisis.ag" #-}
              messageType1 "+" IntegerType
              {-# LINE 3362 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 477 "ContextAnalisis.ag" #-}
              "+"
              {-# LINE 3367 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 478 "ContextAnalisis.ag" #-}
              "add\n"
              {-# LINE 3372 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Add
              {-# LINE 3377 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3382 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Sub :: T_Op
sem_Op_Sub =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 479 "ContextAnalisis.ag" #-}
              Just IntegerType
              {-# LINE 3395 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 480 "ContextAnalisis.ag" #-}
              messageType1 "-" IntegerType
              {-# LINE 3400 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 481 "ContextAnalisis.ag" #-}
              "-"
              {-# LINE 3405 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 482 "ContextAnalisis.ag" #-}
              "sub\n"
              {-# LINE 3410 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Sub
              {-# LINE 3415 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3420 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Product :: T_Op
sem_Op_Product =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 483 "ContextAnalisis.ag" #-}
              Just IntegerType
              {-# LINE 3433 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 484 "ContextAnalisis.ag" #-}
              messageType1 "*" IntegerType
              {-# LINE 3438 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 485 "ContextAnalisis.ag" #-}
              "*"
              {-# LINE 3443 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 486 "ContextAnalisis.ag" #-}
              "mul\n"
              {-# LINE 3448 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Product
              {-# LINE 3453 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3458 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Division :: T_Op
sem_Op_Division =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 487 "ContextAnalisis.ag" #-}
              Just IntegerType
              {-# LINE 3471 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 488 "ContextAnalisis.ag" #-}
              messageType1 "/" IntegerType
              {-# LINE 3476 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 489 "ContextAnalisis.ag" #-}
              "/"
              {-# LINE 3481 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 490 "ContextAnalisis.ag" #-}
              "div\n"
              {-# LINE 3486 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Division
              {-# LINE 3491 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3496 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
sem_Op_Module :: T_Op
sem_Op_Module =
    (let _lhsOtype1 :: (Maybe Type)
         _lhsOmType :: (Maybe Type->Maybe Type -> [String] -> [String] -> [String])
         _lhsOerrorProgram :: String
         _lhsOoutput :: String
         _lhsOast :: Op
         _lhsOtype1 =
             ({-# LINE 491 "ContextAnalisis.ag" #-}
              Just IntegerType
              {-# LINE 3509 "ContextAnalisis.hs" #-}
              )
         _lhsOmType =
             ({-# LINE 492 "ContextAnalisis.ag" #-}
              messageType1 "%" IntegerType
              {-# LINE 3514 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 493 "ContextAnalisis.ag" #-}
              "%"
              {-# LINE 3519 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 494 "ContextAnalisis.ag" #-}
              "mod\n"
              {-# LINE 3524 "ContextAnalisis.hs" #-}
              )
         _ast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              Module
              {-# LINE 3529 "ContextAnalisis.hs" #-}
              )
         _lhsOast =
             ({-# LINE 468 "ContextAnalisis.ag" #-}
              _ast
              {-# LINE 3534 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmType,_lhsOoutput,_lhsOtype1))
-- Parameter ---------------------------------------------------
data Parameter = Parameter (Type) (String)
               deriving ( Show)
-- cata
sem_Parameter :: Parameter ->
                 T_Parameter
sem_Parameter (Parameter _type1 _ident) =
    (sem_Parameter_Parameter (sem_Type _type1) _ident)
-- semantic domain
type T_Parameter = ( Parameter,String,String,([String]),Type)
data Inh_Parameter = Inh_Parameter {}
data Syn_Parameter = Syn_Parameter {ast_Syn_Parameter :: Parameter,errorProgram_Syn_Parameter :: String,id_Syn_Parameter :: String,messages_Syn_Parameter :: ([String]),type1_Syn_Parameter :: Type}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOid,_lhsOmessages,_lhsOtype1) = sem
     in  (Syn_Parameter _lhsOast _lhsOerrorProgram _lhsOid _lhsOmessages _lhsOtype1))
sem_Parameter_Parameter :: T_Type ->
                           String ->
                           T_Parameter
sem_Parameter_Parameter type1_ ident_ =
    (let _lhsOast :: Parameter
         _lhsOerrorProgram :: String
         _lhsOtype1 :: Type
         _lhsOid :: String
         _lhsOmessages :: ([String])
         _type1Iast :: Type
         _type1IerrorProgram :: String
         _lhsOast =
             ({-# LINE 227 "ContextAnalisis.ag" #-}
              Parameter _type1Iast ident_
              {-# LINE 3569 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 228 "ContextAnalisis.ag" #-}
              _type1IerrorProgram++" "++ ident_
              {-# LINE 3574 "ContextAnalisis.hs" #-}
              )
         _lhsOtype1 =
             ({-# LINE 229 "ContextAnalisis.ag" #-}
              _type1Iast
              {-# LINE 3579 "ContextAnalisis.hs" #-}
              )
         _lhsOid =
             ({-# LINE 230 "ContextAnalisis.ag" #-}
              ident_
              {-# LINE 3584 "ContextAnalisis.hs" #-}
              )
         _lhsOmessages =
             ({-# LINE 231 "ContextAnalisis.ag" #-}
              variableVoid ident_ _type1Iast
              {-# LINE 3589 "ContextAnalisis.hs" #-}
              )
         ( _type1Iast,_type1IerrorProgram) =
             type1_
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOid,_lhsOmessages,_lhsOtype1))
-- ParametersList ----------------------------------------------
type ParametersList = [Parameter]
-- cata
sem_ParametersList :: ParametersList ->
                      T_ParametersList
sem_ParametersList list =
    (Prelude.foldr sem_ParametersList_Cons sem_ParametersList_Nil (Prelude.map sem_Parameter list))
-- semantic domain
type T_ParametersList = ( ParametersList,String,([String]),([String]),Int,([Type]))
data Inh_ParametersList = Inh_ParametersList {}
data Syn_ParametersList = Syn_ParametersList {ast_Syn_ParametersList :: ParametersList,errorProgram_Syn_ParametersList :: String,ids_Syn_ParametersList :: ([String]),messages_Syn_ParametersList :: ([String]),numParam_Syn_ParametersList :: Int,type1_Syn_ParametersList :: ([Type])}
wrap_ParametersList :: T_ParametersList ->
                       Inh_ParametersList ->
                       Syn_ParametersList
wrap_ParametersList sem (Inh_ParametersList) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOids,_lhsOmessages,_lhsOnumParam,_lhsOtype1) = sem
     in  (Syn_ParametersList _lhsOast _lhsOerrorProgram _lhsOids _lhsOmessages _lhsOnumParam _lhsOtype1))
sem_ParametersList_Cons :: T_Parameter ->
                           T_ParametersList ->
                           T_ParametersList
sem_ParametersList_Cons hd_ tl_ =
    (let _lhsOast :: ParametersList
         _lhsOerrorProgram :: String
         _lhsOnumParam :: Int
         _lhsOtype1 :: ([Type])
         _lhsOids :: ([String])
         _lhsOmessages :: ([String])
         _hdIast :: Parameter
         _hdIerrorProgram :: String
         _hdIid :: String
         _hdImessages :: ([String])
         _hdItype1 :: Type
         _tlIast :: ParametersList
         _tlIerrorProgram :: String
         _tlIids :: ([String])
         _tlImessages :: ([String])
         _tlInumParam :: Int
         _tlItype1 :: ([Type])
         _lhsOast =
             ({-# LINE 206 "ContextAnalisis.ag" #-}
              _hdIast : _tlIast
              {-# LINE 3635 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 207 "ContextAnalisis.ag" #-}
              _hdIerrorProgram ++ commaSeparated _tlIerrorProgram
              {-# LINE 3640 "ContextAnalisis.hs" #-}
              )
         _lhsOnumParam =
             ({-# LINE 208 "ContextAnalisis.ag" #-}
              1 + _tlInumParam
              {-# LINE 3645 "ContextAnalisis.hs" #-}
              )
         _lhsOtype1 =
             ({-# LINE 209 "ContextAnalisis.ag" #-}
              _hdItype1 : _tlItype1
              {-# LINE 3650 "ContextAnalisis.hs" #-}
              )
         _lhsOids =
             ({-# LINE 210 "ContextAnalisis.ag" #-}
              _hdIid : _tlIids
              {-# LINE 3655 "ContextAnalisis.hs" #-}
              )
         _lhsOmessages =
             ({-# LINE 211 "ContextAnalisis.ag" #-}
              _hdImessages ++ _tlImessages
              {-# LINE 3660 "ContextAnalisis.hs" #-}
              )
         ( _hdIast,_hdIerrorProgram,_hdIid,_hdImessages,_hdItype1) =
             hd_
         ( _tlIast,_tlIerrorProgram,_tlIids,_tlImessages,_tlInumParam,_tlItype1) =
             tl_
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOids,_lhsOmessages,_lhsOnumParam,_lhsOtype1))
sem_ParametersList_Nil :: T_ParametersList
sem_ParametersList_Nil =
    (let _lhsOast :: ParametersList
         _lhsOerrorProgram :: String
         _lhsOnumParam :: Int
         _lhsOtype1 :: ([Type])
         _lhsOids :: ([String])
         _lhsOmessages :: ([String])
         _lhsOast =
             ({-# LINE 212 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3678 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 213 "ContextAnalisis.ag" #-}
              ""
              {-# LINE 3683 "ContextAnalisis.hs" #-}
              )
         _lhsOnumParam =
             ({-# LINE 214 "ContextAnalisis.ag" #-}
              0
              {-# LINE 3688 "ContextAnalisis.hs" #-}
              )
         _lhsOtype1 =
             ({-# LINE 215 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3693 "ContextAnalisis.hs" #-}
              )
         _lhsOids =
             ({-# LINE 216 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3698 "ContextAnalisis.hs" #-}
              )
         _lhsOmessages =
             ({-# LINE 217 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3703 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOids,_lhsOmessages,_lhsOnumParam,_lhsOtype1))
-- Program -----------------------------------------------------
data Program = Class (String) (MembersList)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Class _name _membersList) =
    (sem_Program_Class _name (sem_MembersList _membersList))
-- semantic domain
type T_Program = ( Program,String,([String]),String)
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {ast_Syn_Program :: Program,errorProgram_Syn_Program :: String,messages_Syn_Program :: ([String]),output_Syn_Program :: String}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOoutput) = sem
     in  (Syn_Program _lhsOast _lhsOerrorProgram _lhsOmessages _lhsOoutput))
sem_Program_Class :: String ->
                     T_MembersList ->
                     T_Program
sem_Program_Class name_ membersList_ =
    (let _lhsOast :: Program
         _lhsOmessages :: ([String])
         _lhsOerrorProgram :: String
         _membersListOtIdDefs :: ([String])
         _membersListOtypesDefs :: ([Type])
         _membersListOaIdDefs :: ([String])
         _lhsOoutput :: String
         _membersListOlabels :: ([String])
         _membersListOtMethods :: ([String])
         _membersListOtTypesParam :: ([[Type]])
         _membersListOtTypesRet :: ([Type])
         _membersListIast :: MembersList
         _membersListIerrorProgram :: String
         _membersListIidDefs :: ([String])
         _membersListIlabels :: ([String])
         _membersListImessages :: ([String])
         _membersListImethods :: ([String])
         _membersListIoutput :: String
         _membersListItypes :: ([Type])
         _membersListItypesParam :: ([[Type]])
         _membersListItypesRet :: ([Type])
         _lhsOast =
             ({-# LINE 74 "ContextAnalisis.ag" #-}
              Class name_ _membersListIast
              {-# LINE 3752 "ContextAnalisis.hs" #-}
              )
         _lhsOmessages =
             ({-# LINE 75 "ContextAnalisis.ag" #-}
              _membersListImessages
              {-# LINE 3757 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 76 "ContextAnalisis.ag" #-}
              "class "++ name_ ++"{\n"++(tab _membersListIerrorProgram)++"}"
              {-# LINE 3762 "ContextAnalisis.hs" #-}
              )
         _membersListOtIdDefs =
             ({-# LINE 77 "ContextAnalisis.ag" #-}
              _membersListIidDefs
              {-# LINE 3767 "ContextAnalisis.hs" #-}
              )
         _membersListOtypesDefs =
             ({-# LINE 78 "ContextAnalisis.ag" #-}
              _membersListItypes
              {-# LINE 3772 "ContextAnalisis.hs" #-}
              )
         _membersListOaIdDefs =
             ({-# LINE 79 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3777 "ContextAnalisis.hs" #-}
              )
         _lhsOoutput =
             ({-# LINE 80 "ContextAnalisis.ag" #-}
              "link " ++(show _numAtrib)++"\n"++"ldrr R4 MP\n"
               ++(if "main" `elem` _membersListImethods then call "main" else "")
               ++"bra END\n"++ _membersListIoutput++"END: halt\n"
              {-# LINE 3784 "ContextAnalisis.hs" #-}
              )
         _membersListOlabels =
             ({-# LINE 83 "ContextAnalisis.ag" #-}
              []
              {-# LINE 3789 "ContextAnalisis.hs" #-}
              )
         _numAtrib =
             ({-# LINE 84 "ContextAnalisis.ag" #-}
              length _membersListIidDefs
              {-# LINE 3794 "ContextAnalisis.hs" #-}
              )
         _membersListOtMethods =
             ({-# LINE 85 "ContextAnalisis.ag" #-}
              _membersListImethods
              {-# LINE 3799 "ContextAnalisis.hs" #-}
              )
         _membersListOtTypesParam =
             ({-# LINE 86 "ContextAnalisis.ag" #-}
              _membersListItypesParam
              {-# LINE 3804 "ContextAnalisis.hs" #-}
              )
         _membersListOtTypesRet =
             ({-# LINE 87 "ContextAnalisis.ag" #-}
              _membersListItypesRet
              {-# LINE 3809 "ContextAnalisis.hs" #-}
              )
         ( _membersListIast,_membersListIerrorProgram,_membersListIidDefs,_membersListIlabels,_membersListImessages,_membersListImethods,_membersListIoutput,_membersListItypes,_membersListItypesParam,_membersListItypesRet) =
             membersList_ _membersListOaIdDefs _membersListOlabels _membersListOtIdDefs _membersListOtMethods _membersListOtTypesParam _membersListOtTypesRet _membersListOtypesDefs
     in  ( _lhsOast,_lhsOerrorProgram,_lhsOmessages,_lhsOoutput))
-- Type --------------------------------------------------------
data Type = IntegerType
          | BooleanType
          | VoidType
          deriving ( Eq,Show)
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (IntegerType) =
    (sem_Type_IntegerType)
sem_Type (BooleanType) =
    (sem_Type_BooleanType)
sem_Type (VoidType) =
    (sem_Type_VoidType)
-- semantic domain
type T_Type = ( Type,String)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {ast_Syn_Type :: Type,errorProgram_Syn_Type :: String}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOast,_lhsOerrorProgram) = sem
     in  (Syn_Type _lhsOast _lhsOerrorProgram))
sem_Type_IntegerType :: T_Type
sem_Type_IntegerType =
    (let _lhsOast :: Type
         _lhsOerrorProgram :: String
         _lhsOast =
             ({-# LINE 190 "ContextAnalisis.ag" #-}
              IntegerType
              {-# LINE 3845 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 191 "ContextAnalisis.ag" #-}
              "int"
              {-# LINE 3850 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram))
sem_Type_BooleanType :: T_Type
sem_Type_BooleanType =
    (let _lhsOast :: Type
         _lhsOerrorProgram :: String
         _lhsOast =
             ({-# LINE 192 "ContextAnalisis.ag" #-}
              BooleanType
              {-# LINE 3860 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 193 "ContextAnalisis.ag" #-}
              "boolean"
              {-# LINE 3865 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram))
sem_Type_VoidType :: T_Type
sem_Type_VoidType =
    (let _lhsOast :: Type
         _lhsOerrorProgram :: String
         _lhsOast =
             ({-# LINE 194 "ContextAnalisis.ag" #-}
              VoidType
              {-# LINE 3875 "ContextAnalisis.hs" #-}
              )
         _lhsOerrorProgram =
             ({-# LINE 195 "ContextAnalisis.ag" #-}
              "void"
              {-# LINE 3880 "ContextAnalisis.hs" #-}
              )
     in  ( _lhsOast,_lhsOerrorProgram))