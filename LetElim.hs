----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es
-- un literal entero o booleano. En ese caso se
-- sustituyen las ocurrencias de x en e2 por e1,
-- o sea, e2[e1/x].
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List

-- ELIMINACION DE LETs

letElimP :: Program -> Program
letElimP (Program defs expr) = Program (map letElimDef defs) (letElimExpr expr)

letElimDef :: FunDef -> FunDef
letElimDef (FunDef typedFun names expr) = FunDef typedFun names (letElimExpr expr)

letElimExpr :: Expr -> Expr
letElimExpr (Var name) = (Var name)  -- capaz llevar a expr al fondo y devolver expr nomas => PATTERN MATCHING
letElimExpr expr@(IntLit _) = expr -- Dos de las sintaxis validas para esta parte
letElimExpr expr@(BoolLit _) = expr
letElimExpr (Infix op expr expr') = Infix op (letElimExpr expr) (letElimExpr expr')
letElimExpr (If condExpr expr expr') = If (letElimExpr condExpr) (letElimExpr expr) (letElimExpr expr')
letElimExpr (Let (name, _) expr expr') =
  let substExpr = subst name expr expr' -- Chequear Free variables
  in letElimExpr substExpr -- lo puse asi por que seguramente haya que hacer algo mas, si no, nos podemos ahorrar el let in
letElimExpr (App name args) = App name (map letElimExpr args)


-- CHEQUEAR LO DE VARIABLES LIGADAS
subst :: Name -> Expr -> Expr -> Expr
subst = undefined
