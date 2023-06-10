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
letElimExpr (Infix op expr expr') = Infix op (letElimExpr expr) (letElimExpr expr')
letElimExpr (If condExpr expr expr') = If (letElimExpr condExpr) (letElimExpr expr) (letElimExpr expr')
letElimExpr (Let (name, varType) expr expr') =
  let isLiteral = checkLiteral expr
  in if isLiteral
     then letElimExpr (subst name (letElimExpr expr) expr')
     else Let (name, varType) (letElimExpr expr) (letElimExpr expr')
letElimExpr (App name args) = App name (map letElimExpr args)
letElimExpr expr = expr

checkLiteral :: Expr -> Bool
checkLiteral (IntLit _) = True
checkLiteral (BoolLit _) = True
checkLiteral (Let (name, _) expr expr') = checkLiteral expr && isVar name expr'
checkLiteral _ = False

isVar :: Name -> Expr -> Bool
isVar name (Var name') = name == name'
isVar name (Let (name', _) expr expr') = isVar name expr && isVar name' expr'
isVar _ _ = False

subst :: Name -> Expr -> Expr -> Expr
subst name expr (Var name') = if name == name' then expr else (Var name')
subst name expr (Infix op expr' expr'') = Infix op (subst name expr expr') (subst name expr expr'')
subst name expr (If condExpr expr' expr'') = If (subst name expr condExpr) (subst name expr expr') (subst name expr expr'')
subst name expr (Let (name', varType) expr' expr'') = 
  let bodyExpr = if name == name' then expr'' else subst name expr expr''
  in Let (name', varType) (subst name expr expr') bodyExpr
subst name expr (App name' args) = App name' (map (subst name expr) args)
subst _ _ expr' = expr'






-- TESTS DE CHEQUEOS

-- let x = 3 in x + 2 * x
program1 :: Program
program1 = Program [] (Let ("x", TyInt) (IntLit 3) (Infix Add (Var "x") (Infix Mult (IntLit 2) (Var "x"))))

-- let x = 3 * 4 in x + 2
program2 :: Program
program2 = Program [] (Let ("x", TyInt) (Infix Mult (IntLit 3) (IntLit 4)) (Infix Add (Var "x") (IntLit 2)))

-- let x = 3 in let x = 4 in x + 2
program3 :: Program
program3 = Program [] (Let ("x", TyInt) (IntLit 3) (Let ("x", TyInt) (IntLit 4) (Infix Add (Var "x") (IntLit 2))))

-- let x = 3 in let y = x in y + 2
program4 :: Program
program4 = Program [] (Let ("x", TyInt) (IntLit 3) (Let ("y", TyInt) (Var "x") (Infix Add (Var "y") (IntLit 2))))

-- let x = 3 in let x = 4 + x in x + 2
program5 :: Program
program5 = Program [] (Let ("x", TyInt) (IntLit 3) (Let ("x", TyInt) (Infix Add (IntLit 4) (Var "x")) (Infix Add (Var "x") (IntLit 2))))

-- let x = (let y = 4 in y) in x + 2
program6 :: Program
program6 = Program [] (Let ("x", TyInt) (Let ("y", TyInt) (IntLit 4) (Var "y")) (Infix Add (Var "x") (IntLit 2)))

-- let x = 5 + (let y = 4 in y) in x + 2
program7 :: Program
program7 = Program [] (Let ("x", TyInt) (Infix Add (IntLit 5) (Let ("y", TyInt) (IntLit 4) (Var "y"))) (Infix Add (Var "x") (IntLit 2)))

-- let x = (let y = 4 in y + 3) in x + 2
program8 :: Program
program8 = Program [] (Let ("x", TyInt) (Let ("y", TyInt) (IntLit 4) (Infix Add (Var "y") (IntLit 3))) (Infix Add (Var "x") (IntLit 2)))

testCheckProgram :: Program -> IO ()
testCheckProgram p = putStrLn (show (letElimP p))
