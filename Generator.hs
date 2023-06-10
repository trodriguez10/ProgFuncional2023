----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Data.List

-- CODE GENERATOR


{--
FIGURA 1, declaracio nde Fn sin otras funciones
main = 23 + 4


GENERACION ESPERADA:

# include < stdio .h >
int main () {
printf ( " % d \ n " ,(23 + 4)); }



FIGURA 2, Main con funciont
double :: ( Int ) -> Int
double ( x ) = 2 * x
main = 23 + double (2)

GENERACION ESPERADA:

# include < stdio .h >
int _double ( int _x ){
return ((2 * _x )); };
int main () {
printf ( " % d \ n " ,(23 + _double (2))); }


FIGURA 3:
foo :: ( Int , Bool ) -> Int
foo (x , b ) = if b then x * x else x + 2
main = 23 + foo (2 , True ) + foo (3 , False )

GENERACION ESPERADA:

# include < stdio .h >
int _foo ( int _x , int _b ){
return ( _b ?( _x * _x ):( _x + 2)); };
int main () {
printf ( " % d \ n " ,((23 + _foo (2 ,1)) + _foo (3 ,0))); }

--}

{--
Hay que armar el #include < stdio.h > ... ???????

El encabezado de funcion y los identificadores  (En encabezado de funcion y el codigo, van con prefix _)

EL IF, LO CAMBIO POR UN OPERADOR TERNARIO   cond ? (expr1) : (expr2)


Luego, siempre int main () {
  printf ( "%d\n", ( MAIN DECLARADO EN EL PROGRAMA FUN) ;   )
}
--}


genProgram :: Program -> String
genProgram (Program defs expr) =
  let
    header = getCHeader
    funcDefs = map parseFunDefToC defs
    mainExpr = undefined -- chequeo del main
  in header ++ funcDefs ++ mainExpr


--  FunDef TypedFun [Name]  Expr
-- Sig [Type] Type -- Typos de entrada y de salida

-- Definir Chequeo de EXpresion => CADA FUNCION debe seguir la estructura mostrada arriba
-- Es probable que sea [String]
parseFunDefToC :: FunDef -> String
parseFunDefToC (FunDef typedFun@(name, sig) names expr) =
  let
    funcBody = translateExprToC expr
    funcRetTypeAndName = getFunctionSignature typedFun
    funcVarDeclarations = signatureVariables sig names
  in funcRetTypeAndName ++ " " ++ funcVarDeclarations ++ " {\n" ++ funcBody -- falta return y cerrar }

getFunctionSignature :: TypedFun -> String    -- definida en checker
getFunctionSignature (name, sig) = getSigRetType sig ++ " _" ++ name

-- Zipear Signature y var names => Int x, Bool Y....
signatureVariables :: Sig -> [Name] -> [String]
signatureVariables sig names =
  let structure = zip (getSigTypes sig) (names)
  in map (\(sig, name)-> " " ++ translateTypeToCType sig ++ " _" ++ name  ++ " ") structure

-- puede ser concatMap enve de map

getSigTypes :: Sig -> [Type]
getSigTypes (Sig types _) = types

getSigRetType :: Sig -> Type
getSigRetType (Sig _ retType) = retType

translateTypeToCType :: Type -> String
translateTypeToCType (IntLit _) = "int"
translateTypeToCType (BoolLit _) = "bool" -- No vi bools hasta ahora, capaz los 2 son ints


translateExprToC :: Expr -> String
translateExprToC (Var name) = "_" ++ name
translateExprToC (IntLit _) = undefined
translateExprToC (BoolLit False) = "0"
translateExprToC (BoolLit True) = "1"
translateExprToC (Infix op expr expr') = undefined
translateExprToC (If condExpr expr expr') = undefined
translateExprToC (Let (name, varType) expr expr') = undefined
translateExprToC  (App funName xs) = undefined

getCHeader :: String           -- Arma el header de C++
getCHeader = "include < stdio .h > \n"

opToCOperator :: Op -> String
opToCOperator Add = "+"
opToCOperator Sub = "-"
opToCOperator Mult = "*"
opToCOperator Div = "/"
opToCOperator Eq = "=="
opToCOperator NEq = "!="
opToCOperator GTh = ">" -- puede estar dado vuelta
opToCOperator LTh = "<" -- puede estar dado vuelta
opToCOperator GEq = ">=" -- puede estar dado vuelta
opToCOperator LEq = "<=" -- puede estar dado vuelta
