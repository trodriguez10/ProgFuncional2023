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
  let header = ["#include <stdio.h>"]
      funcDefs = concatMap parseFunDefToC defs
      mainExpr = parseMainToC expr
  in if null funcDefs 
     then unlines (header ++ mainExpr)
     else unlines (header ++ funcDefs ++ mainExpr)

right :: Either a b -> b
right (Right x) = x

parseMainToC :: Expr -> [String]
parseMainToC expr = 
  let letDefs = parseLetFunDefs 0 expr
  in if null letDefs
     then ["int main() {"] ++ ["printf(\"%d\\n\"," ++ parseExprToC 0 expr ++ "); }"]
     else ["int main() {"] ++ letDefs ++ ["printf(\"%d\\n\"," ++ parseExprToC 0 expr ++ "); }"]

parseFunDefToC :: FunDef -> [String]
parseFunDefToC (FunDef (name, sig) names expr) =
  let
    funcRetTypeAndName = parseFunctionSignatureToC (name, sig)
    funcVarDeclarations = joinWithComma $ parseFunctionVariablesToC names
    letDefs = parseLetFunDefs 0 expr
    funcBody = parseExprToC 0 expr
  in if null letDefs
     then [funcRetTypeAndName ++ "(" ++ funcVarDeclarations ++ ")" ++ "{"] ++ ["return (" ++ funcBody ++ "); };"]
     else [funcRetTypeAndName ++ "(" ++ funcVarDeclarations ++ ")" ++ "{"] ++ letDefs ++ ["return (" ++ funcBody ++ "); };"]

parseFunctionSignatureToC :: TypedFun -> String
parseFunctionSignatureToC (name, _) = "int" ++ " _" ++ name

parseFunctionVariablesToC :: [Name] -> [String]
parseFunctionVariablesToC names = map (\name -> "int _" ++ name) names

joinWithComma :: [String] -> String
joinWithComma = intercalate ","

parseLetFunDefs :: Int -> Expr -> [String]
parseLetFunDefs numberLets (Infix op expr expr') = 
  let exprLets = numberOfLets expr
      newNumberLets = numberLets + exprLets
  in parseLetFunDefs numberLets expr ++ parseLetFunDefs newNumberLets expr'
parseLetFunDefs numberLets (If condExpr expr expr') = 
  let condExprLets = numberOfLets condExpr
      newNumberLets = numberLets + condExprLets
      exprLets = numberOfLets expr
      newNewNumberLets = newNumberLets + exprLets
  in parseLetFunDefs numberLets condExpr ++ parseLetFunDefs newNumberLets expr ++ parseLetFunDefs newNewNumberLets expr'
parseLetFunDefs numberLets (Let (name, _) expr expr') = 
  let exprLets = numberOfLets expr
      newNumberLets = numberLets + exprLets
      expr'Lets = numberOfLets expr'
      newNewNumberLets = newNumberLets + expr'Lets
      letNumber = show newNewNumberLets
      exprLetDefs = parseLetFunDefs numberLets expr
      expr'LetDefs = parseLetFunDefs newNumberLets expr'
  in if null expr'LetDefs
     then if null exprLetDefs 
          then ["int _let" ++ letNumber ++ "(int _" ++ name ++ "){"] ++ ["return (" ++ parseExprToC newNumberLets expr' ++ "); };"]
          else exprLetDefs ++ ["int _let" ++ letNumber ++ "(int _" ++ name ++ "){"] ++ ["return (" ++ parseExprToC newNumberLets expr' ++ "); };"]
     else if null exprLetDefs 
          then ["int _let" ++ letNumber ++ "(int _" ++ name ++ "){"] ++ expr'LetDefs ++ ["return (" ++ parseExprToC newNumberLets expr' ++ "); };"]
          else exprLetDefs ++ ["int _let" ++ letNumber ++ "(int _" ++ name ++ "){"] ++ expr'LetDefs ++ ["return (" ++ parseExprToC newNumberLets expr' ++ "); };"]
parseLetFunDefs numberLets (App _ xs) = concatMap (\(x,y) -> parseLetFunDefs x y) $ zip (scanl (\newNumberLets expr -> (newNumberLets + numberOfLets expr)) numberLets xs) xs
parseLetFunDefs _ _ = []

parseExprToC :: Int -> Expr -> String
parseExprToC _ (Var name) = "_" ++ name
parseExprToC _ (IntLit value) = show value
parseExprToC _ (BoolLit False) = "0"
parseExprToC _ (BoolLit True) = "1"
parseExprToC numberLets (Infix op expr expr') = 
  let exprLets = numberOfLets expr
      newNumberLets = numberLets + exprLets
  in "(" ++ parseExprToC numberLets expr ++ parseOperatorToC op ++ parseExprToC newNumberLets expr' ++ ")"
parseExprToC numberLets (If condExpr expr expr') = 
  let condExprLets = numberOfLets condExpr
      newNumberLets = numberLets + condExprLets
      exprLets = numberOfLets expr
      newNewNumberLets = newNumberLets + exprLets
  in parseExprToC numberLets condExpr ++ "?" ++ parseExprToC newNumberLets expr ++ ":" ++ parseExprToC newNewNumberLets expr'
parseExprToC numberLets (Let _ expr expr') = 
  let exprLets = numberOfLets expr
      newNumberLets = numberLets + exprLets
      expr'Lets = numberOfLets expr'
      newNewNumberLets = newNumberLets + expr'Lets
      letNumber = show newNewNumberLets
  in "_let" ++ letNumber ++ "(" ++ parseExprToC numberLets expr ++ ")"
parseExprToC numberLets (App funName xs) = "_" ++ funName ++ "(" ++ joinWithComma (map (\(x,y) -> parseExprToC x y) $ zip (scanl (\newNumberLets expr -> (newNumberLets + numberOfLets expr)) numberLets xs) xs) ++ ")"

numberOfLets :: Expr -> Int
numberOfLets (Infix _ expr expr') = numberOfLets expr + numberOfLets expr'
numberOfLets (If condExpr expr expr') = numberOfLets condExpr + numberOfLets expr + numberOfLets expr'
numberOfLets (Let _ expr expr') = 1 + numberOfLets expr'
numberOfLets (App _ exprs) = sum (map numberOfLets exprs)
numberOfLets _ = 0

parseOperatorToC :: Op -> String
parseOperatorToC Add = " + "
parseOperatorToC Sub = " - "
parseOperatorToC Mult = " * "
parseOperatorToC Div = " / "
parseOperatorToC Eq = "=="
parseOperatorToC NEq = "!="
parseOperatorToC GTh = ">"
parseOperatorToC LTh = "<"
parseOperatorToC GEq = ">="
parseOperatorToC LEq = "<="
