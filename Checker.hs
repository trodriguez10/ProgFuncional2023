----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores,
-- o la lista de errores encontrados en otro caso.
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type

instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

-- SYNTAX AUXILIAR FUNCTIONS

-- Nombre de Typed Var
getVarName :: TypedVar -> Name
getVarName = fst

-- Type de TypedVar
getVarType :: TypedVar -> Type
getVarType = snd

getFunDefTypedFun :: FunDef -> TypedFun
getFunDefTypedFun (FunDef typedFun _ _) = typedFun

-- Nombre de Typed Fun
getFunTypeName :: TypedFun -> Name
getFunTypeName = fst

-- Typos de TypedFun
getFunSig :: TypedFun -> Sig
getFunSig = snd

-- Nombres de lista FunNames
getFunNames :: [FunDef] -> [Name]
getFunNames = map (getFunTypeName . getFunDefTypedFun)

-- Función auxiliar para obtener la lista de argumentos de una signatura
getSigTypes :: Sig -> [Type]
getSigTypes (Sig types _) = types

-- -- Función auxiliar para obtener el tipo de retorno de una signatura
getSigRetType :: Sig -> Type
getSigRetType (Sig _ retType) = retType

getSigParams :: TypedFun -> [Type]
getSigParams (name, Sig params _) = params

-- Función auxiliar para obtener los argumentos de una FunDef
getFunArgs :: FunDef -> [Name]
getFunArgs (FunDef _ args _) = args

-- Función auxiliar para verificar si hay elementos duplicados en una lista
-- Nub function: https://hoogle.haskell.org/?hoogle=nub
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- Función auxiliar para obtener los nombres duplicados en una lista
-- \\ substractor: https://hoogle.haskell.org/?hoogle=%5C%5C&scope=set%3Astackage
getDuplicates :: Eq a => [a] -> [a]
getDuplicates xs = xs \\ nub xs

isOk :: Checked -> Bool
isOk Ok = True
isOk _  = False

isWrong :: Checked -> Bool
isWrong (Wrong _) = True
isWrong _  = False

getVarNames :: [TypedVar] -> [Name]
getVarNames = map fst

isArithmeticOp :: Op -> Bool
isArithmeticOp Add = True
isArithmeticOp Sub = True
isArithmeticOp Mult = True
isArithmeticOp Div = True
isArithmeticOp _ = False

-- REFERENCE https://hoogle.haskell.org/

-- 4.1 Duplicated Declarations

checkDuplicatedNames :: Program -> Checked
checkDuplicatedNames (Program defs _) =
  let dupFunctionNames = getDuplicates (getFunNames defs)
      dupVariableNames = concatMap (getDuplicates . getFunArgs) defs
      duplicatedNames = dupFunctionNames ++ dupVariableNames
  in if null duplicatedNames
    then Ok
    else Wrong (map Duplicated duplicatedNames)


-- 4.2 Number of parameters
-- Defs => Typed Fun => Sig => [Type] LENGTH

checkParamNum :: Program -> Checked
checkParamNum (Program defs _) =
  let paramErrors = concatMap checkFunParams defs
  in if null paramErrors
     then Ok
     else Wrong paramErrors

checkFunParams :: FunDef -> [Error]
checkFunParams (FunDef typedFun args _) =
  let sigParams = getSigParams typedFun
      numSigParams = length sigParams
      numFunArgs = length args
  in if numSigParams /= numFunArgs
     then [ArgNumDef (getFunTypeName typedFun) numSigParams numFunArgs]
     else []


-- 4.3 Non Declared Names
-- Defs -> FunDef -> [Name]

-- ARREGLAR QUE NO DISTINGUE ENTRE FUNCIONES Y VARIABLES
checkNonDeclaredNames :: Program -> Checked
checkNonDeclaredNames (Program defs expr) =
  let declaredFunNames = getFunNames defs
      undefinedNamesInFuns = concatMap (getUndefinedNamesInFun declaredFunNames) defs 
      mainUsedNames = getUsedNames expr
      undefinedMainNames = filter (`notElem` declaredFunNames) mainUsedNames
      undefinedNames = undefinedNamesInFuns ++ undefinedMainNames
  in if null undefinedNames
     then Ok
     else Wrong (map Undefined undefinedNames)

getUndefinedNamesInFun :: [Name] -> FunDef -> [Name]
getUndefinedNamesInFun declaredFunNames (FunDef _ funVarNames expr) = 
  let usedNames = getUsedNames expr
      availableNames = declaredFunNames ++ funVarNames
  in filter (`notElem` availableNames) usedNames

getUsedVarNames :: Expr -> [Name]
getUsedVarNames (Var name) = [name]
getUsedVarNames (IntLit  _) = []
getUsedVarNames (BoolLit _) = []
getUsedVarNames (Infix _ expr expr') = getUsedVarNames expr ++ getUsedVarNames expr'
getUsedVarNames (If expr expr' expr'') = getUsedVarNames expr ++ getUsedVarNames expr' ++ getUsedVarNames expr''
getUsedVarNames (Let (name, _) bindExpr bodyExpr) = getUsedVarNames bindExpr ++ filter (/= name) (getUsedVarNames bodyExpr)
getUsedVarNames (App name exprs) = [] ++ concatMap getUsedVarNames exprs

getUsedFunNames :: Expr -> [Name]
getUsedFunNames (Var name) = []
getUsedFunNames (IntLit  _) = []
getUsedFunNames (BoolLit _) = []
getUsedFunNames (Infix _ expr expr') = getUsedFunNames expr ++ getUsedFunNames expr'
getUsedFunNames (If expr expr' expr'') = getUsedFunNames expr ++ getUsedFunNames expr' ++ getUsedFunNames expr''
getUsedFunNames (Let _ bindExpr bodyExpr) = getUsedFunNames bindExpr ++ getUsedFunNames bodyExpr
getUsedFunNames (App name exprs) = [name] ++ concatMap getUsedFunNames exprs

getUsedNames :: Expr -> [Name]
getUsedNames (Var name) = [name]
getUsedNames (IntLit  _) = []
getUsedNames (BoolLit _) = []
getUsedNames (Infix _ expr expr') = getUsedNames expr ++ getUsedNames expr'
getUsedNames (If expr expr' expr'') = getUsedNames expr ++ getUsedNames expr' ++ getUsedNames expr''
getUsedNames (Let (name, _) bindExpr bodyExpr) = getUsedNames bindExpr ++ filter (/= name) (getUsedNames bodyExpr)
getUsedNames (App name exprs) = [name] ++ concatMap getUsedNames exprs


-- 4.4 Type CHeck
-- SCOPE => ENV.

checkTypes :: Program -> Checked
checkTypes (Program defs expr) =
  let defsErrors = concatMap checkValidFun defs
      mainErrors = checkValidExpr [] expr
      allErrors = defsErrors ++ mainErrors
  in if null allErrors
     then Ok
     else Wrong allErrors

checkValidFun :: FunDef -> [Error]
checkValidFun (FunDef (name, sig) names expr) = 
  let env = zip names (getSigTypes sig)
      retTypeError = if isIntegerType (getSigRetType sig) == isIntegerExpr env expr then [] else (if isIntegerType (getSigRetType sig) then [Expected TyInt TyBool] else [Expected TyBool TyInt])
      exprErrors = checkValidExpr [] expr
  in exprErrors ++ retTypeError

isIntegerType :: Type -> Bool
isIntegerType TyInt = True
isIntegerType TyBool = False

-- OJO CON EL OP, ESTA BIEN NO CONSIDERAR LOS EXPR??
isIntegerExpr :: Env -> Expr -> Bool
isIntegerExpr env (Var name) = isIntegerVar env name
isIntegerExpr _ (IntLit  _) = True
isIntegerExpr _ (BoolLit _) = False
isIntegerExpr _ (Infix op _ _) = isIntegerOp op
isIntegerExpr env (If _ expr _) = isIntegerExpr env expr -- Asumimos que el tipo de retorno de un if es la primer expresion
isIntegerExpr env (Let (name, sig) expr expr') = undefined
isIntegerExpr env (App name xs) = undefined

isIntegerVar :: Env -> Name -> Bool
isIntegerVar env name = 
  let correspondingEnvVar = filter (elem name . getVarName) env -- Asumimos que no es vacio porque fue checkeado en la etapa de checkNonDelcared
  in isIntegerType (getVarType (head correspondingEnvVar))

-- CAMBIAR PARA NO REPETIR CODIGO
isIntegerOp :: Op -> Bool
isIntegerOp Add = True
isIntegerOp Sub = True
isIntegerOp Mult = True  
isIntegerOp Div = True  
isIntegerOp _ = False

checkValidExpr :: Env -> Expr -> [Error]
checkValidExpr _ (Var _) = [] -- El parser no permite meter un algo que no sea un String/Name en un Var
checkValidExpr _ (IntLit _) = [] -- El parser no permite meter un Bool en un IntLit por definicion
checkValidExpr _ (BoolLit _) = [] -- El parser no permite meter un Integer en un BoolLit por definicion
checkValidExpr env (Infix op expr expr') = checkInfix env op expr expr'
checkValidExpr env (If condExpr expr expr') = checkIf env condExpr expr expr'
checkValidExpr env (Let (name, sig) expr expr') = undefined
checkValidExpr env (App name xs) = undefined

checkInfix :: Env -> Op -> Expr -> Expr -> [Error]
checkInfix env op expr expr'
  | isArithmetic == True =
    let expr1Errors = checkValidExpr env expr
        expr2Errors = checkValidExpr env expr'
        opError = if isIntegerExpr env expr && isIntegerExpr env expr' then [] else (if isIntegerExpr env expr then [Expected TyInt TyBool] else (if isIntegerExpr env expr' then [Expected TyInt TyBool] else [Expected TyInt TyBool, Expected TyInt TyBool]))
    in opError ++ expr1Errors ++ expr2Errors
  | otherwise =
    let expr1Errors = checkValidExpr env expr
        expr2Errors = checkValidExpr env expr'
        opError = checkSameTypeExpr env expr expr'
    in opError ++ expr1Errors ++ expr2Errors
  where isArithmetic = isArithmeticOp op
  
checkIf :: Env -> Expr -> Expr -> Expr -> [Error]
checkIf env condExpr expr expr' = 
  let expr1Errors = checkValidExpr env condExpr
      condChecked = if isIntegerExpr env condExpr then [Expected TyBool TyInt] else []
      expr2Errors = checkValidExpr env expr
      expr3Errors = checkValidExpr env expr'
      matchError =  checkSameTypeExpr env expr expr'
  in expr1Errors ++ condChecked ++ expr2Errors ++ expr3Errors ++ matchError
  
checkSameTypeExpr :: Env -> Expr -> Expr -> [Error]
checkSameTypeExpr env expr expr' = if isIntegerExpr env expr == isIntegerExpr env expr' then [] else (if isIntegerExpr env expr then [Expected TyInt TyBool] else [Expected TyBool TyInt])

getErrorsList :: [Checked] -> [Error]
getErrorsList [] = []
getErrorsList xs = concat [errors | (Wrong errors) <-xs, isWrong (Wrong errors)]

getErrors :: Checked -> [Error]
getErrors Ok = []
getErrors (Wrong errors) = errors

-- CHECK PROGRAM debe cortar dependiendo de cada etapa, si sale OK sigue, si sale mal, corta
checkProgram :: Program -> Checked
checkProgram program =
  case checkDuplicatedNames program of
    Ok ->
      case checkParamNum program of
        Ok ->
          case checkNonDeclaredNames program of
            Ok -> 
              case checkTypes program of
                Ok -> Ok
                Wrong errors -> Wrong (getErrors (checkTypes program))
            Wrong errors -> Wrong (getErrors (checkNonDeclaredNames program))
        Wrong errors -> Wrong (getErrors (checkParamNum program))
    Wrong errors -> Wrong (getErrors (checkDuplicatedNames program))



-- TESTS DE CHEQUEOS
-- Definición de funciones y programa de ejemplo

-- Definis un type def, le diste tipo a func1 :: TyInt -> TyBool -> TyInt
myFunction1 :: TypedFun
myFunction1 = ("func1", Sig [TyInt, TyBool] TyInt)

myFunction2 :: TypedFun
myFunction2 = ("func2", Sig [TyBool, TyBool, TyInt] TyBool)

myFunction3 :: TypedFun
myFunction3 = ("func3", Sig [] TyBool)

-- myFunction tiene la declaracion de la funxion,
-- lo otro son los nombres de variables de la funcion
-- lo ultimo es lo que hace la funcion
funDef1 :: FunDef
funDef1 = FunDef myFunction1 ["x", "y"] (Var "x")

funDef2 :: FunDef
funDef2 = FunDef myFunction2 ["x", "y", "z"] (If (BoolLit True) (IntLit 5) (Var "func1"))-- ?

funDef3 :: FunDef
funDef3 = FunDef myFunction3 [] (BoolLit True)

program :: Program
program = Program [funDef1, funDef2, funDef3] (If (BoolLit False) (IntLit 5) (App "func1" [IntLit 3, BoolLit False]))

program2 :: Program
program2 = Program [] (If (BoolLit False) (Infix Add (IntLit 2) (IntLit 3)) (IntLit 4))

-- Función para mostrar el resultado Checked
showChecked :: Checked -> String
showChecked Ok = "Ok"
showChecked (Wrong errors) = "Wrong: " ++ show errors

-- Caso de prueba
testCheckParamNum :: IO ()
testCheckParamNum = putStrLn (showChecked (checkParamNum program))

testCheckDuplicatedName :: IO ()
testCheckDuplicatedName = putStrLn (showChecked (checkDuplicatedNames program))

testCheckNonDeclared :: IO ()
testCheckNonDeclared = putStrLn (showChecked (checkNonDeclaredNames program))

testCheckTypes :: IO ()
testCheckTypes = putStrLn (showChecked (checkTypes program2))

testCheckProgram :: Program -> IO ()
testCheckProgram p = putStrLn (showChecked (checkProgram p))

-- tests fallan cunado declaro dos varaibles ( FALTA IMPLEMENETAR ESO )

-- FUnciona si pongo Let b y luego llamo a un A que no existe
-- (Let ("b", TyInt) (IntLit 10) (App "add" [Var "a", IntLit 5]))
