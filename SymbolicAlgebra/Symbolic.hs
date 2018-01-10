module Symbolic where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char
import Data.List
import Data.Ratio
import Data.Function
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
             ("-", binaryExpression "-" (-)),
             ("*", binaryExpression "*" (*)),
             ("/", binaryExpression "/" (/)),
             ("log", floatingUnaryExpression "log" log),
             ("sqrt", floatingUnaryExpression "sqrt" sqrt),
             ("sin", floatingUnaryExpression "sin" sin),
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

floatingUnaryExpression :: (Real a, Floating a) => Operator -> (a -> a) -> [Expression] -> ThrowsError Expression
floatingUnaryExpression opName op [] = throwError $ NumArgs 1 []
floatingUnaryExpression opName op [opp]
    | isNum opp = unpack opp >>= return . Number . realToFrac . op . fromRational
    | otherwise = return $ CExpression opName [opp]

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