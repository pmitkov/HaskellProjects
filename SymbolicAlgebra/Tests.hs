module Tests where

import Test.HUnit
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

import Symbolic
import ReplacementRules
import SymbolicError
import SymbolicParser
import Integration

isException :: ThrowsError Expression -> Bool
isException (Left _)  = True
isException (Right _) = False

simpleEvalAdd = TestCase $ do
        assertBool "(+ 3 5) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(+ 3 5) is number" (isNum val)
        let Number num = val
        assertEqual "(+ 3 5)" (3 + 5) num
        let Right (Number val) = evalExpr "(+ (+ 3 1) (+ 4 3))"
        assertEqual "(+ (+ 3 1) (+ 4 3))" ((3 + 1) + (4 + 3)) val
        let Right (Number val) = evalExpr "(+ 1 2 3)"
        assertEqual "(+ 1 2 3)" (1 + 2 + 3) val
    where evaled = evalExpr "(+ 3 5)"

simpleEvalSub = TestCase $ do
        assertBool "(- 3 5) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(- 3 5) is number" (isNum val)
        let Number num = val
        assertEqual "(- 3 5)" (3 - 5) num
        let Right (Number val) = evalExpr "(- (- 3 1) (- 4 3))"
        assertEqual "(- (- 3 1) (- 4 3))" ((3 - 1) - (4 - 3)) val
        let Right (Number val) = evalExpr "(- 1 2 3)"
        assertEqual "(- 1 2 3)" (1 - 2 - 3) val
    where evaled = evalExpr "(- 3 5)"

simpleEvalMult = TestCase $ do
        assertBool "(* 2 10) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(* 2 10) is number" (isNum val)
        let Number num = val
        assertEqual "(* 2 10)" (2 * 10) num
        let Right (Number val) = evalExpr "(* (* 2 4) (* 3 1))"
        assertEqual "(* (* 2 4) (* 3 1))" (2 * 4 * 3 * 1) val
        let Right (Number val) = evalExpr "(* 2 3 4)"
        assertEqual "(* 2 3 4)" (2 * 3 * 4) val
    where evaled = evalExpr "(* 2 10)"

simpleEvalDiv = TestCase $ do
        assertBool "(/ 3 7) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(/ 3 7) is number" (isNum val)
        let Number num = val
        assertEqual "(/ 3 7)" (3 / 7) num
        let Right (Number val) = evalExpr "(/ 2 (/ 3 4))"
        assertEqual "(/ 2 (/ 3 4))" (2 / (3 / 4)) val
        let Right (Number val) = evalExpr "(/ 2 3 4)"
        assertEqual "(/ 2 3 4)" (2 / 3 / 4) val
    where evaled = evalExpr "(/ 3 7)"

simpleEvalExp = TestCase $ do
        assertBool "(exp 2) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(exp 2) is number" (isNum val)
        let Number num = val
        assertEqual "(exp 2)" (realToFrac $ exp 2) num
        let Right (Number val) = evalExpr "(exp (exp 2))"
        assertEqual "(exp (exp 2))" (realToFrac $ exp (exp 2)) val
        let Right (Number val) = evalExpr "(exp (+ 2 2))"
        assertEqual "(exp (+ 2 2))" (realToFrac $ exp (2 + 2)) val
    where evaled = evalExpr "(exp 2)"

simpleEvalExpt = TestCase $ do
        assertBool "(** 2 3) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(** 2 3) is number" (isNum val)
        let Number num = val
        assertEqual "(** 2 3)" (2 ^ 3) num
        let Right (Number val) = evalExpr "(** 2 (** 2 2))"
        assertEqual "(** 2 (** 2 2))" (2 ^ 2 ^ 2) val
    where evaled = evalExpr "(** 2 3)"

simpleEvalLog = TestCase $ do
        assertBool "(log 5) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(log 5) is number" (isNum val)
        let Number num = val
        assertEqual "(log 5)" (realToFrac $ log 5) num
        let Right (Number val) = evalExpr "(log (log 5))"
        assertEqual "(log (log 5))" (realToFrac $ log (log 5)) val
    where evaled = evalExpr "(log 5)"

simpleEvalSqrt = TestCase $ do
        assertBool "(sqrt 2) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(sqrt 2) is number" (isNum val)
        let Number num = val
        assertEqual "(sqrt 2)" (realToFrac $ sqrt 2) num
        let Right (Number val) = evalExpr "(sqrt (sqrt 2))"
        assertEqual "(sqrt (sqrt 2))" (realToFrac $ sqrt (sqrt 2)) val
    where evaled = evalExpr "(sqrt 2)"

simpleEvalSin = TestCase $ do
        assertBool "(sin 5) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(sin 5) is number" (isNum val)
        let Number num = val
        assertEqual "(sin 5)" (realToFrac $ sin 5) num
        let Right (Number val) = evalExpr "(sin (sin 5))"
        assertEqual "(sin (sin 5))" (realToFrac $ sin (sin 5)) val
    where evaled = evalExpr "(sin 5)"

simpleEvalCos = TestCase $ do
        assertBool "(cos 5) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(cos 5) is number" (isNum val)
        let Number num = val
        assertEqual "(cos 5)" (realToFrac $ cos 5) num
        let Right (Number val) = evalExpr "(cos (cos 5))"
        assertEqual "(cos (cos 5))" (realToFrac $ cos (cos 5)) val
    where evaled = evalExpr "(cos 5)"

complexEvalAddAndSub = TestCase $ do
        assertBool "(+ (- 5 6) (- 4 10)) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(+ (- 5 6) (- 4 10)) is number)" (isNum val)
        let Number num = val
        assertEqual "(+ (- 5 6) (- 4 10))" ((5 - 6) + (4 - 10)) num
    where evaled = evalExpr "(+ (- 5 6) (- 4 10))"

complexEvalAddAndDiv = TestCase $ do
        assertBool "(+ (/ 2 3) (/ 4 5)) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(+ (/ 2 3) (/ 4 5)) is number)" (isNum val)
        let Number num = val
        assertEqual "(+ (/ 2 3) (/ 4 5))" ((2 / 3) + (4 / 5)) num
        let Right (Number val) = evalExpr "(+ (/ 2 3) (/ 4 5) (/ 7 8))"
        assertEqual "(+ (/ 2 3) (/ 4 5) (/ 7 8))" ((2 / 3) + (4 / 5) + (7 / 8)) val
        let Right (Number val) = evalExpr "(/ (+ 2 3) (+ 4 5))"
        assertEqual "(/ (+ 2 3) (+ 4 5))" ((2 + 3) / (4 + 5)) val
    where evaled = evalExpr "(+ (/ 2 3) (/ 4 5))"

complexEvalAddAndMult = TestCase $ do
        assertBool "(+ (* 2 3) (* 4 6)) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(+ (* 2 3) (* 4 6)) is number)" (isNum val)
        let Number num = val
        assertEqual "(+ (* 2 3) (* 4 6))" ((2 * 3) + (4 * 6)) num
        let Right (Number val) = evalExpr "(+ (* 2 3) (* 4 5) (* 7 8))"
        assertEqual "(+ (* 2 3) (* 4 5) (* 7 8))" ((2 * 3) + (4 * 5) + (7 * 8)) val
        let Right (Number val) = evalExpr "(* (+ 2 3) (+ 4 5) (+ 6 7))"
        assertEqual "(* (+ 2 3) (+ 4 5) (+ 6 7))" ((2 + 3) * (4 + 5) * (6 + 7)) val
    where evaled = evalExpr "(+ (* 2 3) (* 4 6))"

complexEvalDivAndMult = TestCase $ do
        assertBool "(/ (* 2 3) (* 4 6)) results in no exception" (not $ isException evaled)
        let Right val = evaled
        assertBool "(/ (* 2 3) (* 4 6)) is number)" (isNum val)
        let Number num = val
        assertEqual "(/ (* 2 3) (* 4 6))" ((2 * 3) / (4 * 6)) num
        let Right (Number val) = evalExpr "(/ (/ 2 3) (* (/ 5 6) 12))"
        assertEqual "(/ (/ 2 3) (* (/ 5 6) 12))" ((2 / 3) / ((5 / 6) * 12)) val
        let Right (Number val) = evalExpr "(* 3 4 (/ (* 4 5) 15))"
        assertEqual "(* (+ 2 3) (+ 4 5) (+ 6 7))" (3 * 4 * ((4 * 5) / 15)) val
    where evaled = evalExpr "(/ (* 2 3) (* 4 6))"

complexEvalAllOperators1 = TestCase $ do
        let Right (Number val) = evalExpr "(/ (** 2 3) (- (/ 5 6) 12))"
        assertEqual  "(/ (** 2 3) (- (/ 5 6) 12))" val ((2 ^ 3) / ((5 / 6) - 12))
        let Right (Number val) = evalExpr "(sin (** 2 6))"
        assertEqual "(sin (** 2 6))" (realToFrac $ sin (2 ^ 6)) val
        let Right (Number val) = evalExpr "(cos (** 2 6))"
        assertEqual "(cos (** 2 6))" (realToFrac $ cos (2 ^ 6)) val
        let Right (Number val) = evalExpr "(exp (* 2 (+ 4 (sin 6))))"
        assertEqual "(exp (* 2 (+ 4 (sin 6))))" (realToFrac $ exp (2 * (4 + sin 6))) val
        let Right (Number val) = evalExpr "(+ (sin 6) 3)"
        assertEqual "(+ (sin 6) 3)" (realToFrac $ sin 6 + 3) val
        let Right (Number val) = evalExpr "(/ (sin 4) (sin 4))"
        assertEqual "(/ (sin 4) (sin 4))" 1 val
        let Right (Number val) = evalExpr "(** (+ 2 2) 3)"
        assertEqual "(** (+ 2 2) 3)" ((2 + 2) ^ 3) val
        let Right (Number val) = evalExpr "(+ (** 2 3) (** 2 4))"
        assertEqual "(+ (** 2 3) (** 2 4))" (2 ^ 3 + 2 ^ 4) val
        let Right (Number val) = evalExpr "(/ 1 (** 2 3))"
        assertEqual "(/ 1 (** 2 3))" (1 / 2 ^ 3) val
        let Right (Number val) = evalExpr "(sqrt (+ 2 2))"
        assertEqual "(sqrt (+ 2 2))" (realToFrac $ sqrt (2 + 2)) val
        let Right (Number val) = evalExpr "(+ (sqrt 5) (sqrt 5))"
        assertEqual "(+ (sqrt 5) (sqrt 5))" (realToFrac $ sqrt 5 + sqrt 5) val
        let Right (Number val) = evalExpr "(+ (sqrt (** 2 2)) (sqrt (** 3 2)))"
        assertEqual "(+ (sqrt (** 2 2)) (sqrt (** 3 2)))" (realToFrac $ sqrt (2 ^ 2) + sqrt (3 ^ 2)) val
        let Right (Number val) = evalExpr "(cos (** 2 (+ 2 2)))"
        assertEqual "(cos (** 2 (+ 2 2)))" (realToFrac $ cos (2 ^ (2 + 2))) val

substitutionWithOneVar = TestCase $ do
        let Right expr = evalExpr "(+ X1 3)"
            Right (Number val) = eval (M.fromList [("X1", 10)]) expr 
        assertEqual "(+ X1 3)" (10 + 3) val
        let Right expr = evalExpr "(* X1 X1)"
            Right (Number val) = eval (M.fromList [("X1", 10)]) expr 
        assertEqual "(* X1 X1)" (10 * 10) val
        let Right expr = evalExpr "(** X1 X1)"
            Right (Number val) = eval (M.fromList [("X1", 10)]) expr 
        assertEqual "(** X1 X1)" (10 ^ 10) val
        let Right expr = evalExpr "(* X1 15)"
            Right (Number val) = eval (M.fromList [("X1", 0)]) expr 
        assertEqual "(* X1 15)" (0 * 15) val
        let Right expr = evalExpr "(/ X1 X1)"
            Right (Number val) = eval (M.fromList [("X1", 10)]) expr 
        assertEqual "(/ X1 X1)" (10 / 10) val

substitutionWithTwoVars = TestCase $ do
        let Right expr = evalExpr "(+ X1 X2)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4)]) expr
        assertEqual "(+ X1 X2)" (3 + 4) val
        let Right expr = evalExpr "(* X1 X2)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4)]) expr
        assertEqual "(* X1 X2)" (3 * 4) val
        let Right expr = evalExpr "(** X1 X2)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4)]) expr
        assertEqual "(** X1 X2)" (3 ^ 4) val
        let Right expr = evalExpr "(+ X1 (* X2 15))"
            Right (Number val) = eval (M.fromList [("X1", 15), ("X2", 10)]) expr
        assertEqual "(+ X1 (* X2 15))" (15 + (10 * 15)) val
        let Right expr = evalExpr "(* (sin X1) X2)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4)]) expr
        assertEqual "(* (sin X1) X2)" (realToFrac $ sin 3 * 4) val

substitutionWithThreeVars = TestCase $ do
        let Right expr = evalExpr "(+ X1 X2 X3)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4), ("X3", 5)]) expr
        assertEqual "(+ X1 X2 X3)" (3 + 4 + 5) val
        let Right expr = evalExpr "(* X1 X2 X3)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4), ("X3", 5)]) expr
        assertEqual "(* X1 X2 X3)" (3 * 4 * 5) val
        let Right expr = evalExpr "(+ (** X1 X2) X3)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 2), ("X3", 4)]) expr
        assertEqual "(+ (** X1 X2) X3)" (3 ^ 2 + 4) val
        let Right expr = evalExpr "(/ X1 X2 X3)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4), ("X3", 5)]) expr
        assertEqual "(/ X1 X2 X3)" (3 / 4 / 5) val
        let Right expr = evalExpr "(- X1 X2 X3)"
            Right (Number val) = eval (M.fromList [("X1", 3), ("X2", 4), ("X3", 5)]) expr
        assertEqual "(- X1 X2 X3)" (3 - 4 - 5) val

parseErrorException = TestCase $ do
        assertBool "(+ 3 4" $ isParseError $ evalExpr "(+ 3 4"
        assertBool "+ 3 4)" $ isParseError $ evalExpr "+ 3 4)"
        assertBool "+ 3 4" $ isParseError $ evalExpr "+ 3 4"
        assertBool "(3 + 4)" $ isParseError $ evalExpr "(3 + 4)"
        assertBool "(+ 3 - 4)" $ isParseError $ evalExpr "(+ 3 - 4)"

        assertBool "(/ (+ 3 4)" $ isParseError $ evalExpr "(/ (+ 3 4)"
        assertBool "X11" $ isParseError $ evalExpr "X11"
        assertBool "(+ X11 3)" $ isParseError $ evalExpr "(+ X11 3)"
        assertBool "(+ x1 3)" $ isParseError $ evalExpr "(+ x1 3)"
        assertBool "(+ X 3)" $ isParseError $ evalExpr "(+ X 3)"
    where isParseError (Left (Parser _)) = True
          isParseError _                 = False

numArgsException = TestCase $ do
        assertBool "(+ 1)" $ isNumArgsN 2 (evalExpr "(+ 1)")
        assertBool "(* 1)" $ isNumArgsN 2 (evalExpr "(* 1)")
        assertBool "(- 1)" $ isNumArgsN 2 (evalExpr "(- 1)")
        assertBool "(/ 1)" $ isNumArgsN 2 (evalExpr "(/ 1)")
        assertBool "(** 1)" $ isNumArgsN 2 (evalExpr "(** 1)")
        assertBool "(sqrt 1 2)" $ isNumArgsN 1 (evalExpr "(sqrt 1 2)")
        assertBool "(sin 1 2)" $ isNumArgsN 1 (evalExpr "(sin 1 2)")
        assertBool "(cos 1 2)" $ isNumArgsN 1 (evalExpr "(cos 1 2)")
        assertBool "(exp 1 2)" $ isNumArgsN 1 (evalExpr "(exp 1 2)")
        assertBool "(log 1 2)" $ isNumArgsN 1 (evalExpr "(log 1 2)")
        assertBool "(tan 1 2)" $ isNumArgsN 1 (evalExpr "(tan 1 2)")
    where isNumArgsN n res = case res of
                                (Left (NumArgs m _)) -> n == m
                                _                    -> False

singleVarDifferentiation = TestCase $ do
        let Right expr = evalExpr "(diff (+ X1 1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (+ X1 1) X1)" (1 + 0) val
        let Right expr = evalExpr "(diff (+ (* X1 X1) 3) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (+ (* X1 X1) 3) X1)" (2 * 3) val
        let Right expr = evalExpr "(diff (+ X1 (* X1 X1)) X1)"
            Right (Number val) = eval (M.fromList [("X1", 5)]) expr
        assertEqual "(diff (+ X1 (* X1 X1)) X1)" (1 + 2 * 5) val
        let Right expr = evalExpr "(diff (* 15 X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (* 15 X1) X1)" (15 * 1) val
        let Right expr = evalExpr "(diff (sin X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (sin X1) X1)" (realToFrac $ cos 3) val

        let Right expr = evalExpr "(diff (cos X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (cos X1) X1)" (realToFrac $ -(sin 3)) val
        let Right expr = evalExpr "(diff (sqrt X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 4)]) expr
        assertEqual "(diff (sqrt X1) X1)" (realToFrac $ 1 / (2 * sqrt 4)) val
        let Right expr = evalExpr "(diff (exp X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (exp X1) X1)" (realToFrac $ exp 3) val
        let Right expr = evalExpr "(diff (exp X1) X1)"
            Right (Number val) = eval (M.fromList [("X1", 15)]) expr
        assertEqual "(diff (exp X1) X1)" (realToFrac $ exp 15) val
        let Right expr = evalExpr "(diff (- 1 (* X1 X1 X1)) X1)"
            Right (Number val) = eval (M.fromList [("X1", 3)]) expr
        assertEqual "(diff (+ X1 1) X1)" (- (3 * 3 ^ 2)) val

simpleEvaluationTests = TestList [TestLabel "simpleEvalAdd"  simpleEvalAdd,
                                  TestLabel "simpleEvalSub"  simpleEvalSub,
                                  TestLabel "simpleEvalMult" simpleEvalMult,
                                  TestLabel "simpleEvalDiv"  simpleEvalDiv,
                                  TestLabel "simpleEvalExp"  simpleEvalExp,
                                  TestLabel "simpleEvalExpt" simpleEvalExpt,
                                  TestLabel "simpleEvalLog"  simpleEvalLog,
                                  TestLabel "simpleEvalSqrt" simpleEvalSqrt,
                                  TestLabel "simpleEvalSin"  simpleEvalSin,
                                  TestLabel "simpleEvalCos"  simpleEvalCos]

complexEvaluationTests = TestList [TestLabel "complexEvalAddAndSub"  complexEvalAddAndSub,
                                   TestLabel "complexEvalAddAndDiv"  complexEvalAddAndDiv,
                                   TestLabel "complexEvalAddAndMult" complexEvalAddAndMult,
                                   TestLabel "complexEvalAllOperators1" complexEvalAllOperators1]

substitutionTests = TestList [TestLabel "substitutionWithOneVar"    substitutionWithOneVar,
                              TestLabel "substitutionWithTwoVars"   substitutionWithTwoVars,
                              TestLabel "substitutionWithThreeVars" substitutionWithThreeVars]

exceptionTests = TestList [TestLabel "parseErrorException" parseErrorException,
                           TestLabel "numArgsException"    numArgsException]

differentiationTests = TestList [TestLabel "singleVarDifferentiation" singleVarDifferentiation]