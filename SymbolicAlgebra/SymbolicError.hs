module SymbolicError where

import Control.Monad
import Control.Monad.Error
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (spaces)

import SymbolicParser

data ExpressionError = NumArgs Integer [Expression]
                     | Parser ParseError
                     | NotFunction String String
                     | BadSpecialForm String Expression
                     | UnboundVar String String
                     | Default String

showError :: ExpressionError -> String
showError (UnboundVar message var) = message ++ ": " ++ var
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show ExpressionError where
    show = showError

instance Error ExpressionError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either ExpressionError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unpack :: Expression -> ThrowsError (Ratio Integer)
unpack (Number n) = return n
unpack (Var var) = throwError $ UnboundVar "variable" var