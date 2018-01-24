module Symbolic where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char
import Data.List
import Data.Ratio
import Data.Function hiding (on)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import System.IO

import SymbolicParser
import ReplacementRules
import SymbolicError
import Differentiation
import Integration

e :: Ratio Integer
e = realToFrac $ exp 1
         
eval :: (M.Map String (Ratio Integer)) -> Expression -> ThrowsError Expression
eval env (Number num) = return $ Number num
eval env (CExpression op opps) = mapM (eval env) opps >>= apply op
eval env (Var var) = case M.lookup var env of
                      Just val -> return $ Number val
                      Nothing  -> return $ Var var

evalExpr :: String -> ThrowsError Expression
evalExpr input = case parse parseExpr "symbolic" input of
         Left err -> throwError $ Parser err
         Right val -> eval M.empty val >>= (\x -> simplify [x])

apply :: Operator -> [Expression] -> ThrowsError Expression
apply op opps = maybe (throwError $ NotFunction "Unrecognized primitive function" op)
                      ($ opps)
                      (lookup op operators)

operators :: [(String, [Expression] -> ThrowsError Expression)]
operators = [("+", binaryExpression "+" (+)),
             ("-", binaryNonCommutativeExpression "-" (-)),
             ("*", binaryExpression "*" (*)),
             ("/", binaryNonCommutativeExpression "/" (/)),
             ("log", floatingUnaryExpression "log" log),
             ("sqrt", floatingUnaryExpression "sqrt" sqrt),
             ("sin", floatingUnaryExpression "sin" sin),
             ("tan", floatingUnaryExpression "tan" tan),
             ("cos", floatingUnaryExpression "cos" cos),
             ("exp", floatingUnaryExpression "exp" exp),
             ("**", floatingBinaryExpression "**" (**)),
             ("diff", (\x -> differentiate x >>= eval M.empty >>= (\y -> simplify [y]))),
             ("int", (\x -> integrate x >>= eval M.empty >>= (\y -> simplify [y]))),
             ("simp", (\x -> simplify x >>= eval M.empty >>= (\y -> simplify [y])))]

binaryExpression :: Operator -> (Ratio Integer -> Ratio Integer -> Ratio Integer) -> [Expression] -> ThrowsError Expression
binaryExpression opName op [] = throwError $ NumArgs 2 []
binaryExpression opName op singleVal@[_] = throwError $ NumArgs 2 singleVal
binaryExpression opName op opps
    | null symbolic = mapM unpack numeric >>= return . Number . foldl1 op
    | null numeric  = return $ CExpression opName symbolic
    | otherwise     = return $ CExpression opName $ (Number $ foldl1 op $ map (\(Number a) -> a) numeric) : symbolic
    where (numeric, symbolic) = partition isNum opps

binaryNonCommutativeExpression :: Operator -> (Ratio Integer -> Ratio Integer -> Ratio Integer) -> [Expression] -> ThrowsError Expression
binaryNonCommutativeExpression opName op [] = throwError $ NumArgs 2 []
binaryNonCommutativeExpression opName op singleVal@[_] = throwError $ NumArgs 2 singleVal
binaryNonCommutativeExpression opName op opps
    | length operands == 1 = return $ head operands
    | otherwise = return $ CExpression opName $ fold opps
    where foldExpr [] = []
          foldExpr [expr] = [expr]
          foldExpr (Number n1 : Number n2 : xs) = Number (op n1 n2) : foldExpr xs
          foldExpr (x:xs) = x : foldExpr xs
          fold expr
              | expr /= foldExpr expr = fold $ foldExpr expr
              | otherwise = expr
          operands = fold opps

floatingUnaryExpression :: (Real a, Floating a) => Operator -> (a -> a) -> [Expression] -> ThrowsError Expression
floatingUnaryExpression opName op [] = throwError $ NumArgs 1 []
floatingUnaryExpression opName op [opp]
    | isNum opp = unpack opp >>= return . Number . realToFrac . op . fromRational
    | otherwise = return $ CExpression opName [opp]
floatingUnaryExpression _ _ opps = throwError $ NumArgs 1 opps

floatingBinaryExpression :: (Real a, Floating a) => Operator -> (a -> a -> a) -> [Expression] -> ThrowsError Expression
floatingBinaryExpression opName op [] = throwError $ NumArgs 2 []
floatingBinaryExpression opName op singleVal@[_] = throwError $ NumArgs 2 singleVal
floatingBinaryExpression opName op opps
    | null symbolic = mapM unpack numeric >>= return . Number . realToFrac . foldl1 op . map realToFrac
    | null numeric  = return $ CExpression opName symbolic
    | otherwise     = return $ CExpression opName $ 
                      (Number $ realToFrac . foldl1 op $ map (\(Number n) -> realToFrac n) numeric) : symbolic
    where (numeric, symbolic) = partition isNum opps

simplify :: [Expression] -> ThrowsError Expression
simplify [] = throwError $ NumArgs 1 []
simplify [val@(Number n)] = return val
simplify [val@(Var v)] = return val
simplify [expr@(CExpression op opps)] =
  let simplified = replaceTerm (CExpression op $ map (\opp -> extractValue $ simplify [opp]) opps)
  in if expr /= simplified then simplify [simplified] else return $ expr
simplify badForm = throwError $ NumArgs 1 badForm

-- Factorization schema
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f a b = g (f a) (f b)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = ([(x:)] <*> rest) ++ rest
    where rest = subsets xs

commonFactors :: [Expression] -> [Expression]
commonFactors = foldl1 intersect . map getOperands

formCommonFactor :: [Expression] -> Expression
formCommonFactor [expr] = expr
formCommonFactor exprs = CExpression "*" exprs

factorizeTerms :: [Expression] -> [[Expression]]
factorizeTerms [] = []
factorizeTerms [expr] = [[expr]]
factorizeTerms exprs
    | length longestSeries == 1 = [exprs]
    | otherwise = longestSeries : factorizeTerms (exprs \\ longestSeries)
    where longestSeries = maximumBy (compare `on` length) [expr | expr <- subsets exprs, not $ null expr, 
                                                                  hasCommonFactors expr]
          hasCommonFactors = not . null . commonFactors 

formExpression :: [[Expression]] -> Expression
formExpression [exprs]
    | length exprs == 1 = head exprs
    | null common  = CExpression "+" exprs
    | otherwise = CExpression "*" [formCommonFactor common,
                                   CExpression "+" $ mapTerms exprs]
    where common = commonFactors exprs
          mapTerms = map (\(CExpression _ opps) -> generateExpression opps)
          generateExpression opps
              | length (opps \\ common) == 0 = Number 1
              | length (opps \\ common) == 1 = head (opps \\ common)
              | otherwise = CExpression "*" (opps \\ common)
formExpression exprs = CExpression "+" $ map (\expr -> formExpression [expr]) exprs

factorize :: Expression -> Expression
factorize expr@(CExpression "+" opps)
    | all (\opp -> isCExpression opp && getOperator opp == "*") opps = formExpression $ factorizeTerms opps
    | otherwise = expr
factorize expr = expr

-- Общ делител
commonDivisor :: Expression -> Expression
commonDivisor (CExpression "+" [CExpression "/" [opp1, opp2], CExpression "/" [opp3, opp4]]) =
               CExpression "/" [CExpression "+" [CExpression "*" [opp1, opp4],
                                                 CExpression "*" [opp3, opp2]],
                                CExpression "*" [opp2, opp4]]

simplifyDivision :: Expression -> Expression
simplifyDivision expr@(CExpression "/" [expr1@(CExpression "*" opps1), expr2@(CExpression "*" opps2)])
    | not $ null commons = generateExpressions 
    | otherwise = expr
    where commons = commonFactors [expr1,expr2]
          generateExpressions
              | null (opps1 \\ commons) && null (opps2 \\ commons) = CExpression "*" commons
              | null (opps1 \\ commons) = CExpression "/" [Number 1, CExpression "*" (opps2 \\ commons)]
              | null (opps2 \\ commons) = CExpression "*" (opps1 \\ commons)
              | otherwise = CExpression "*" (commons ++ [CExpression "/" [CExpression "*"
                                                                  (opps1 \\ commons),
                                                                         CExpression "*"
                                                                  (opps2 \\ commons)]])
simplifyDivision expr@(CExpression "/" [CExpression "+" opps1, CExpression "+" opps2])
    | all isMultiplyExpr (opps1 ++ opps2) = factorizeDivision opps1 opps2
    | otherwise = expr
simplifyDivision expr@(CExpression "/" [expr1@(CExpression "*" opps1), CExpression "+" opps2])
    | all isMultiplyExpr opps2 = inverseFraction $ factorizeDivisionMultiply opps2 opps1
    | otherwise = expr
simplifyDivision expr@(CExpression "/" [CExpression "+" opps1, expr2@(CExpression "*" opps2)])
    | all isMultiplyExpr opps1 = factorizeDivisionMultiply opps1 opps2
    | otherwise = expr
simplifyDivision expr = expr

isMultiplyExpr :: Expression -> Bool
isMultiplyExpr expr = isCExpression expr && getOperator expr == "*"

factorizeDivision :: [Expression] -> [Expression] -> Expression
factorizeDivision opps1 opps2
    | null commons = CExpression "/" [CExpression "+" opps1, CExpression "+" opps2]
    | otherwise =  CExpression "/" [CExpression "+" $ mapTerms opps1,
                                    CExpression "+" $ mapTerms opps2]
    where commons = intersect (commonFactors opps1) (commonFactors opps2)
          mapTerms = map (\(CExpression _ opps) -> generateExpression opps)
          generateExpression opps
              | length (opps \\ commons) == 0 = Number 1
              | length (opps \\ commons) == 1 = head (opps \\ commons)
              | otherwise = CExpression "*" (opps \\ commons)

factorizeDivisionMultiply :: [Expression] -> [Expression] -> Expression
factorizeDivisionMultiply opps1 opps2
    | null commons = CExpression "/" [CExpression "+" opps1, CExpression "+" opps2]
    | otherwise = CExpression "/" [CExpression "+" $ mapTerms opps1,
                                   CExpression "*" (opps2 \\ commons)]
    where commons = intersect (commonFactors opps1) opps2
          mapTerms = map (\(CExpression _ opps) -> generateExpression opps)
          generateExpression opps
              | length (opps \\ commons) == 0 = Number 1
              | length (opps \\ commons) == 1 = head (opps \\ commons)
              | otherwise = CExpression "*" (opps \\ commons)

inverseFraction :: Expression -> Expression
inverseFraction (CExpression "/" [expr1, expr2]) = CExpression "/" [expr2, expr1]
inverseFraction _ = error "Not a fraction"
