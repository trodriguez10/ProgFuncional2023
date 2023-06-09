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

checkNonDeclaredNames :: Program -> Checked
checkNonDeclaredNames (Program defs expr) =
  let declaredFunNames = getFunNames defs
      undefinedNamesInFuns = concatMap (getUndefinedNamesInFun declaredFunNames) defs
      mainUsedNames = map (\(name, _) -> name) $ getUsedNames expr
      undefinedMainNames = filter (`notElem` declaredFunNames) mainUsedNames
      undefinedNames = undefinedNamesInFuns ++ undefinedMainNames
  in if null undefinedNames
     then Ok
     else Wrong (map Undefined undefinedNames)

getUndefinedNamesInFun :: [Name] -> FunDef -> [Name]
getUndefinedNamesInFun declaredFunNames (FunDef _ funVarNames expr) =
  let usedNames = getUsedNames expr
  in map (\(name, _) -> name) $ filter (\(name, isVar) -> if isVar then name `notElem` funVarNames else name `notElem` declaredFunNames) usedNames

getUsedNames :: Expr -> [(Name, Bool)]
getUsedNames (Var name) = [(name, True)]
getUsedNames (IntLit  _) = []
getUsedNames (BoolLit _) = []
getUsedNames (Infix _ expr expr') = getUsedNames expr ++ getUsedNames expr'
getUsedNames (If expr expr' expr'') = getUsedNames expr ++ getUsedNames expr' ++ getUsedNames expr''
getUsedNames (Let (name, _) bindExpr bodyExpr) = getUsedNames bindExpr ++ filter (\(usedName, _) -> usedName /= name) (getUsedNames bodyExpr)
getUsedNames (App name exprs) = [(name, False)] ++ concatMap getUsedNames exprs


-- 4.4 Type CHeck

checkTypes :: Program -> Checked
checkTypes (Program defs expr) =
  let defsErrors = concatMap (checkValidFun defs) defs
      mainErrors = checkValidExpr defs [] expr
      allErrors = defsErrors ++ mainErrors
  in if null allErrors
     then Ok
     else Wrong allErrors

checkValidFun :: [FunDef] -> FunDef -> [Error]
checkValidFun defs (FunDef (name, sig) names expr) =
  let env = zip names (getSigTypes sig)
      retTypeError = if isIntegerType (getSigRetType sig) == isIntegerExpr defs env expr then [] else (if isIntegerType (getSigRetType sig) then [Expected TyInt TyBool] else [Expected TyBool TyInt])
      exprErrors = checkValidExpr defs env expr
  in exprErrors ++ retTypeError

isIntegerType :: Type -> Bool
isIntegerType TyInt = True
isIntegerType TyBool = False

isIntegerExpr :: [FunDef] -> Env -> Expr -> Bool
isIntegerExpr _ env (Var name) = isIntegerVar env name
isIntegerExpr _ _ (IntLit  _) = True
isIntegerExpr _ _ (BoolLit _) = False
isIntegerExpr _ _ (Infix op _ _) = isIntegerOp op -- Asumimos que el tipo de una operacion esta dado por la operacion y no por sus valores
isIntegerExpr defs env (If _ expr _) = isIntegerExpr defs env expr -- Asumimos que el tipo de retorno de un if es la primer expresion
isIntegerExpr defs env (Let (name, varType) expr expr') = isIntegerExpr defs (updateEnv env name varType) expr' -- El tipo de reotrno es el tipo de la expr' actualizando el env con la nueva variable
isIntegerExpr defs env (App funName xs) = 
  let correspondingFun = filter ((==) funName . getFunTypeName . getFunDefTypedFun) defs -- Asumimos que no es vacio porque fue checkeado en la etapa de checkNonDelcared
  in isIntegerType ((getSigRetType . getFunSig . getFunDefTypedFun) (head correspondingFun))

isIntegerVar :: Env -> Name -> Bool
isIntegerVar env name =
  let correspondingEnvVar = filter ((==) name .  getVarName) env -- Asumimos que no es vacio porque fue checkeado en la etapa de checkNonDelcared
  in isIntegerType (getVarType (head correspondingEnvVar))

isIntegerOp :: Op -> Bool
isIntegerOp = isArithmeticOp

checkValidExpr :: [FunDef] -> Env -> Expr -> [Error]
checkValidExpr _ _ (Var _) = [] -- El parser no permite meter un algo que no sea un String/Name en un Var
checkValidExpr _ _ (IntLit _) = [] -- El parser no permite meter un Bool en un IntLit por definicion
checkValidExpr _ _ (BoolLit _) = [] -- El parser no permite meter un Integer en un BoolLit por definicion
checkValidExpr defs env (Infix op expr expr') = checkInfix defs env op expr expr'
checkValidExpr defs env (If condExpr expr expr') = checkIf defs env condExpr expr expr'
checkValidExpr defs env (Let (name, varType) expr expr') = checkLet defs env name varType expr expr'
checkValidExpr defs env (App funName xs) = checkApp defs env funName xs

checkInfix :: [FunDef] -> Env -> Op -> Expr -> Expr -> [Error]
checkInfix defs env op expr expr'
  | isArithmetic == True =
    let expr1Errors = checkValidExpr defs env expr
        expr2Errors = checkValidExpr defs env expr'
        opError = if isIntegerExpr defs env expr && isIntegerExpr defs env expr' then [] else (if isIntegerExpr defs env expr then [Expected TyInt TyBool] else (if isIntegerExpr defs env expr' then [Expected TyInt TyBool] else [Expected TyInt TyBool, Expected TyInt TyBool]))
    in opError ++ expr1Errors ++ expr2Errors
  | otherwise =
    let expr1Errors = checkValidExpr defs env expr
        expr2Errors = checkValidExpr defs env expr'
        opError = checkSameTypeExpr defs env expr expr'
    in opError ++ expr1Errors ++ expr2Errors
  where isArithmetic = isArithmeticOp op

checkIf :: [FunDef] -> Env -> Expr -> Expr -> Expr -> [Error]
checkIf defs env condExpr expr expr' =
  let expr1Errors = checkValidExpr defs env condExpr
      condChecked = if isIntegerExpr defs env condExpr then [Expected TyBool TyInt] else []
      expr2Errors = checkValidExpr defs env expr
      expr3Errors = checkValidExpr defs env expr'
      matchError =  checkSameTypeExpr defs env expr expr'
  in expr1Errors ++ condChecked ++ expr2Errors ++ expr3Errors ++ matchError

checkSameTypeExpr :: [FunDef] -> Env -> Expr -> Expr -> [Error]
checkSameTypeExpr defs env expr expr' = if isIntegerExpr defs env expr == isIntegerExpr defs env expr' then [] else (if isIntegerExpr defs env expr then [Expected TyInt TyBool] else [Expected TyBool TyInt])

checkLet :: [FunDef] -> Env -> Name -> Type -> Expr -> Expr -> [Error]
checkLet defs env name varType expr expr' =
  let exprErrors = checkValidExpr defs env expr
      letErrors = checkSameTypeExpr' defs env varType expr
      expr'Errors = checkValidExpr defs (updateEnv env name varType) expr'
  in exprErrors ++ letErrors ++ expr'Errors 

checkSameTypeExpr' :: [FunDef] -> Env -> Type -> Expr -> [Error]
checkSameTypeExpr' defs env varType expr = if isIntegerType varType == isIntegerExpr defs env expr then [] else (if isIntegerType varType then [Expected TyInt TyBool] else [Expected TyBool TyInt])

updateEnv :: Env -> Name -> Type -> Env
updateEnv env name varType = (name, varType) : (filter ((/=) name .  getVarName) env)

checkApp :: [FunDef] -> Env -> Name -> [Expr] -> [Error]
checkApp defs env funName exprs =
  let correspondingFun = filter ((==) funName . getFunTypeName . getFunDefTypedFun) defs -- Asumimos que no es vacio porque fue checkeado en la etapa de checkNonDelcared
      funParamsTypes = (getSigParams . getFunDefTypedFun) (head correspondingFun)
      appError = if length funParamsTypes == length exprs then [] else [ArgNumApp funName (length funParamsTypes) (length exprs)]
      pairs = zip funParamsTypes exprs
      paramsErrors = concatMap (\(funParamType, expr) -> checkSameTypeExpr' defs env funParamType expr) pairs
      exprsErrors = concatMap (checkValidExpr defs env) exprs
  in appError ++ paramsErrors ++ exprsErrors

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
myFunction2 = ("func2", Sig [TyBool, TyInt, TyInt] TyBool)

myFunction3 :: TypedFun
myFunction3 = ("func3", Sig [] TyBool)

-- myFunction tiene la declaracion de la funxion,
-- lo otro son los nombres de variables de la funcion
-- lo ultimo es lo que hace la funcion
funDef1 :: FunDef
funDef1 = FunDef myFunction1 ["x", "y"] (Infix Add (Var "x") (IntLit 3))

funDef2 :: FunDef
funDef2 = FunDef myFunction2 ["x", "y", "z"]  (Infix Eq (Infix Eq (BoolLit False) (BoolLit True)) (BoolLit False))

funDef3 :: FunDef
funDef3 = FunDef myFunction3 [] (BoolLit True)

program :: Program
program = Program [funDef1, funDef2, funDef3] (If (BoolLit False) (IntLit 5) (App "func1" [IntLit 3, BoolLit False]))

program2 :: Program
program2 = Program [funDef1, funDef2] (Let ("x", TyBool) (Infix Eq (BoolLit False) (BoolLit True)) (Infix Eq (Var "x") (BoolLit True)))

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
testCheckTypes = putStrLn (showChecked (checkTypes program))

testCheckProgram :: Program -> IO ()
testCheckProgram p = putStrLn (showChecked (checkProgram p))

-- tests fallan cunado declaro dos varaibles ( FALTA IMPLEMENETAR ESO )

-- FUnciona si pongo Let b y luego llamo a un A que no existe
-- (Let ("b", TyInt) (IntLit 10) (App "add" [Var "a", IntLit 5]))
