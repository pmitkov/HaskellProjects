module Differentiation where

import Control.Monad.Error

import SymbolicParser
import SymbolicError

differentiate :: [Expression] -> ThrowsError Expression
differentiate [] = throwError $ NumArgs 2 []
differentiate singleVal@[_] = throwError $ NumArgs 2 singleVal
differentiate [Number _ , Var var] =return $ Number 0
differentiate [Var v1, Var v2] =return $ Number $ if v1 == v2 then 1 else 0
differentiate [CExpression op opps, Var var] = maybe (throwError $ NotFunction "Not differentiable function" op)
                                                     (\f -> f opps var)
                                                     (lookup op differentiationOperators)                                                
differentiate badForm = throwError $ NumArgs 2 badForm          

differentiationOperators :: [(String,[Expression] -> String -> ThrowsError Expression)]
differentiationOperators = [("+", plusDifferentiation),
                            ("-", minusDifferentiation),
                            ("*", multDifferentiation),
                            ("/", divisionDifferentiation),
                            ("log", logDifferentiation),
                            ("sqrt", sqrtDifferentiation),
                            ("sin", sinDifferentiation),
                            ("cos", cosDifferentiation),
                            ("exp", expDifferentiation),
                            ("**", exptDifferentiation)]

plusDifferentiation :: [Expression] -> String -> ThrowsError Expression
plusDifferentiation opps var = return $ CExpression "+" $  map (\opp -> extractValue $ differentiate [opp, Var var]) opps

minusDifferentiation :: [Expression] -> String -> ThrowsError Expression
minusDifferentiation opps var = return $ CExpression "-" $ map (\opp -> extractValue $ differentiate [opp, Var var]) opps

multDifferentiation :: [Expression] -> String -> ThrowsError Expression
multDifferentiation opps var = return $ CExpression "+" $
                                        map (\(exp,exps) -> CExpression "*" ((extractValue $ differentiate [exp, Var var]):exps))
                                            [(opps !! (i-1), take (i-1) opps ++ drop i opps) | i <- [1..length opps]]

divisionDifferentiation :: [Expression] -> String -> ThrowsError Expression
divisionDifferentiation [opp1,opp2] var = return $ CExpression "/" [
                                                    CExpression "-" [
                                                     CExpression "*" [extractValue $ differentiate [opp1, Var var], opp2],
                                                     CExpression "*" [opp1, extractValue $ differentiate [opp2, Var var]]],
                                                    CExpression "**" [opp2,Number 2]]

logDifferentiation :: [Expression] -> String -> ThrowsError Expression
logDifferentiation [opp] var = return $ CExpression "/" [
                                         extractValue $ differentiate [opp, Var var],
                                         opp]

sqrtDifferentiation :: [Expression] -> String -> ThrowsError Expression
sqrtDifferentiation [opp] var = return $ CExpression "/" [
                                          extractValue $ differentiate [opp, Var var],
                                          CExpression "*" [
                                           Number 2,
                                           CExpression "sqrt" [opp]]]

sinDifferentiation :: [Expression] -> String -> ThrowsError Expression
sinDifferentiation [opp] var = return $ CExpression "*" [
                                         CExpression "cos" [opp],
                                         extractValue $ differentiate [opp, Var var]]

cosDifferentiation :: [Expression] -> String -> ThrowsError Expression
cosDifferentiation [opp] var = return $ CExpression "*" [
                                         Number (-1),
                                         CExpression "sin" [opp],
                                         extractValue $ differentiate [opp, Var var]]

expDifferentiation :: [Expression] -> String -> ThrowsError Expression
expDifferentiation [opp] var = return $ CExpression "*" [
                                         extractValue $ differentiate [opp, Var var],
                                         CExpression "exp" [opp]]

exptDifferentiation :: [Expression] -> String -> ThrowsError Expression
exptDifferentiation [opp1, opp2] var = return $ CExpression "*" [
                                                 CExpression "exp" [
                                                  CExpression "*" [
                                                   opp2,
                                                   CExpression "log" [opp1]]],
                                                 CExpression "+" [
                                                  CExpression "*" [
                                                   extractValue $ differentiate [opp2, Var var],
                                                   CExpression "log" [opp1]],
                                                  CExpression "*" [
                                                    opp2,
                                                    extractValue $ differentiate [
                                                     CExpression "log" [opp1],
                                                     Var var]]]]