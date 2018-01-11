module ReplacementRules where

import SymbolicParser
import Data.List
import Data.Function

replaceTerm :: Expression -> Expression
replaceTerm (CExpression "+" [Number 0, val]) = val
replaceTerm (CExpression "+" [expr]) = expr
replaceTerm (CExpression "+" (Number 0 : opps)) = CExpression "+" opps
replaceTerm (CExpression "+" [opp, CExpression "+" opps]) = CExpression "+" (opp:opps)
replaceTerm (CExpression "+" opps) = subRule2 $ subRule1 $ CExpression "+" $ concatMap flattenAddition opps
   where flattenAddition (CExpression "+" opps) = opps
         flattenAddition expr                   = [expr]
         subRule1 (CExpression "+" opps) = CExpression "+" $ filter (not . (\x -> isNum x && x == Number 0)) opps
         subRule2 (CExpression "+" opps)
           | length (maximumBy (compare `on` length) groups) > 1 =
             CExpression "+" $ map (\expr -> CExpression "*" [Number $ fromIntegral $ length expr, head expr]) groups
           | otherwise = CExpression "+" opps
             where groups = group $ sort $ map sortExpr opps
replaceTerm (CExpression "*" [expr]) = expr
replaceTerm (CExpression "*" (Number 0 : opps)) = Number 0
replaceTerm (CExpression "*" [Number 1, val]) = val
replaceTerm (CExpression "*" (Number 1 : opps)) = CExpression "*" opps
replaceTerm (CExpression "*" [Number n, CExpression "*" opps]) = CExpression "*" (Number n:opps)
replaceTerm (CExpression "*" [Var v, CExpression "*" opps]) = CExpression "*" (Var v:opps)
replaceTerm (CExpression "*" [CExpression "*" opps1, CExpression "*" opps2]) = CExpression "*" (opps1 ++ opps2)
replaceTerm (CExpression "*" [expr1@(CExpression "exp" _), expr2@(CExpression "*" _)]) =
    CExpression "*" [expr1,expr2]
replaceTerm (CExpression "*" [CExpression "*" opps, expr@(CExpression "log" _)]) =
    CExpression "*" (expr:opps)
replaceTerm (CExpression "*" [CExpression "*" opps, expr@(CExpression "sin" _)]) =
    CExpression "*" (expr:opps)
replaceTerm (CExpression "*" [CExpression "*" opps, expr@(CExpression "cos" _)]) =
    CExpression "*" (expr:opps)
replaceTerm (CExpression "*" [CExpression "*" opps, expr@(CExpression _ [_])]) =
    CExpression "*" (expr:opps)
replaceTerm (CExpression "*" [CExpression "+" opps1, expr@(CExpression "+" opps2)]) =
    CExpression "+" $ map (\opp -> CExpression "*" [opp, expr]) opps1
replaceTerm (CExpression "*" (Number n1 : Number n2 : opps)) =
    CExpression "*" (Number (n1 * n2) : opps)
replaceTerm (CExpression "*" [Number n, CExpression "/" [Number m, expr]]) =
    CExpression "/" [Number (n*m), expr]
replaceTerm (CExpression "*" [Number n, CExpression "/" [expr, Number m]]) =
    CExpression "*" [Number (n/m), expr]
replaceTerm (CExpression "*" [Var var, CExpression "/" [Number 1, expr]]) =
    CExpression "/" [Var var, expr]
replaceTerm (CExpression "*" [expr1@(CExpression _ _), CExpression "/" [Number 1, expr2]]) =
    CExpression "/" [expr1, expr2]
replaceTerm (CExpression "*" [Var var, CExpression "+" opps]) =
    CExpression "+" $ map (\opp -> CExpression "*" [Var var, opp]) opps
replaceTerm (CExpression "*" [Number n, CExpression "+" opps]) =
    CExpression "+" $ map (\opp -> CExpression "*" [Number n, opp]) opps
replaceTerm (CExpression "*" opps) = CExpression "*" $ filter (not . (\expr -> isNum expr && expr == Number 1)) opps
replaceTerm (CExpression "/" [expr, Number n]) = CExpression "*" [Number (1/n), expr]
replaceTerm (CExpression "/" (expr : Number n : exprs)) = CExpression "/" (CExpression "*" [expr, Number (1 / n)] : exprs)
replaceTerm expr@(CExpression "/" [CExpression "*" opps, expr1])
    | expr1 `elem` opps = CExpression "*" $ delete expr1 opps
    | otherwise = expr
replaceTerm val = val

sortExpr :: Expression -> Expression
sortExpr (Number n) = Number n
sortExpr (Var var)  = Var var
sortExpr (CExpression "*" opps) = CExpression "*" $ sort opps
sortExpr (CExpression "+" opps) = CExpression "+" $ sort opps
sortExpr expr = expr