module Tests where

import Test.HUnit
import Symbolic

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

-- Tests to be done
-- More complex evaluation tests
-- Substitution with values tests
-- Tests for exceptions
-- Tests for exceptions with proper text
-- Differentiation tests
-- Integration tests

-- Additional modifications to the code
-- Improve Integration
-- Add Binary non comutative and associative functions
-- Fix Division to be non binary operation

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
