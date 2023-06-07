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

-- Extra imports
import Data.Typeable

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
-- getSigRetType :: Sig -> Type
-- getSigRetType (Sig _ retType) = retType

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

typeCheck :: Expr -> Checked
typeCheck = undefined
-- typeCheck (Var name) =
-- typeCheck (IntLit  integer) = if isInteger integer then Ok else Wrong [Expected TyInt TyBool]
-- typeCheck (BoolLit bool) = typeOf bool == typeOf True
-- typeCheck (Infix Op expr expr')
-- typeCheck (If Expr expr expr')
-- typeCheck (Let TypedVar expr expr')
-- typeCheck (App name xs) =


checkTypes :: Program -> Checked
checkTypes (Program defs expr) =
  let funcNames = getFunNames defs
      varNames = concatMap getFunArgs defs
      allNames = funcNames ++ varNames
      usedNames = getUsedNames expr
      undefinedNames = filter (`notElem` allNames) usedNames
  in if null undefinedNames
     then Ok
     else Wrong (map Undefined undefinedNames)

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
            Ok -> Ok
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

testCheckProgram :: IO ()
testCheckProgram = putStrLn (showChecked (checkProgram program))

-- tests fallan cunado declaro dos varaibles ( FALTA IMPLEMENETAR ESO )

-- FUnciona si pongo Let b y luego llamo a un A que no existe
-- (Let ("b", TyInt) (IntLit 10) (App "add" [Var "a", IntLit 5]))
