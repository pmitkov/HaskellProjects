module SymbolicParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Data.Char

type Operator = String
    
data Expression = Var String
                | Number (Ratio Integer)
                | CExpression Operator [Expression] deriving (Eq, Ord)

isNum :: Expression -> Bool
isNum (Number _) = True
isNum _          = False

getOperator :: Expression -> String
getOperator (CExpression op _) = op
getOperator _ = error "Argument is not a complex expression. Use isCExpression to check"

getOperands :: Expression -> [Expression]
getOperands (CExpression _ opps) = opps
getOperands _ = error "Argument is not a complex expression. Use isCexpression to check"

isCExpression :: Expression -> Bool
isCExpression (CExpression _ _) = True
isCExpression _ = False

showExpr :: Expression -> String
showExpr (Number num) = if denominator num == 1 then show $ numerator num else show num
showExpr (Var var)    = var
showExpr (CExpression op opps) = "(" ++ op ++ " " ++ (unwords . map showExpr) opps ++ ")"

instance Show Expression where
  show = showExpr

spaces :: Parser ()
spaces = skipMany1 space

operator :: Parser String
operator = string "log"
        <|> try (string "simp")
        <|> try (string "sin")
        <|> string "cos"
        <|> string "+"
        <|> string "-"
        <|> try (string "**")
        <|> string "*"
        <|> string "/"
        <|> string "tan"
        <|> string "sqrt"
        <|> string "diff"
        <|> string "exp"
        <|> string "int"

parseVar :: Parser Expression
parseVar = do
             x <- satisfy isUpper
             y <- satisfy isDigit
             return $ Var [x,y]

parseNumber :: Parser Expression
parseNumber = (Number . (% 1) . read) <$> many1 digit

parseCExpression :: Parser Expression
parseCExpression = do
  char '('
  op    <- endBy operator spaces
  opps  <- sepBy parseExpr spaces
  char ')'
  return $ CExpression (head op) opps

parseExpr :: Parser Expression
parseExpr = parseVar
         <|> parseNumber
         <|> parseCExpression

-- Simple error handling case
readExpr :: String -> Either String Expression
readExpr input = case parse parseExpr "symbolic" input of
         Left err -> Left $ "No match: " ++ show err
         Right val -> Right val
