module Integration where

import Control.Monad
import Control.Monad.Error
import Data.Ratio

import SymbolicParser
import SymbolicError

allVars  :: [String]
allVars = [[x,y] | x <- ['A'..'Z'], y <- ['1'..'9']]

usedVars :: Expression -> [String]
usedVars (Number _) = []
usedVars (Var var) = [var]
usedVars (CExpression _ opps) = concatMap usedVars opps

findAvailableVar :: Expression -> String
findAvailableVar e = head [x | x <- allVars, x `notElem` usedVars e]

isPolynomial :: Expression -> Bool
isPolynomial (Number _) = True
isPolynomial (Var _) = True
isPolynomial (CExpression "+" opps) = all isPolynomial opps
isPolynomial (CExpression "*" opps) = all isPolynomial opps
isPolynomial (CExpression "-" opps) = all isPolynomial opps
isPolynomial _ = False

isVar :: Expression -> Bool
isVar (Var _) = True
isVar _       = False

isSpecialForm :: String -> Expression -> Bool
isSpecialForm var (CExpression "*" opps) = all (\opp -> isNum opp || (isVar opp && Var var == opp)) opps 
isSpecialForm _ _ = False

integrate :: [Expression] -> ThrowsError Expression
integrate [] = throwError $ NumArgs 2 []
integrate singleVal@[_] = throwError $ NumArgs 2 singleVal
integrate [Number n, Var v] = return $ CExpression "*" [Number n, Var v]
integrate [Var v1, Var v2] = return $ if v1 == v2
                                        then CExpression "/" [
                                              CExpression "*" [Var v1, Var v1],
                                                               Number 2]
                                        else CExpression "*" [Var v1, Var v2]
integrate [CExpression "*" [Number n, expr], Var var] =
    return $ CExpression "*" [Number n, extractValue $ integrate [expr, Var var]]
integrate [CExpression "+" opps, Var var] =
    return $ CExpression "+" $ map (\x -> extractValue $ integrate [x, Var var]) opps
integrate [expr@(CExpression op [Var v1]), Var v2]
    | v1 /= v2 = return $  CExpression "*" [expr, Var v2]
    | op == "sin" = return $ CExpression "*" [Number (-1), CExpression "cos" [Var v2]]
    | op == "cos" = return $ CExpression "cos" [Var v2]
    | op == "sqrt" = return $ CExpression "*" [CExpression "**" [Var v2, Number (3/2)],
                                               Number (2/3)]
integrate [expr@(CExpression op opps), Var var]
    | var `notElem` usedVars expr = return $ CExpression "*" [Var var, expr]
    | isSpecialForm var expr = return $ CExpression "*" (Number (1 % (toInteger (length (filter isVar opps)) + 1)) : Var var : opps)
integrate [expr, Var _] = throwError $ BadSpecialForm "Can't integrate" expr
integrate [_, v] = throwError $ BadSpecialForm "Not a variable" v 

--integrate [CExpression]
-- Най-добре интегрирането да се извършва без добавяне на константа, тъй като
-- това само ще усложни правилата за заменяне при опростяване без да доведе до
-- някакви ползи
--integrate [Number n, Var v] = return $ CExpression "+" [
--                                        CExpression "*" [Number n, Var v],
--                                        Var $ findAvailableVar $ Var v]