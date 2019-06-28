module Spec where

import Test.Framework ( defaultMainWithOpts, testGroup, TestOptions, RunnerOptions
                      , topt_maximum_generated_tests, ropt_test_options
                      )
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Foldable

--import Spec
import Lib
import Data.HashMap.Strict as H (HashMap, empty, singleton, toList, fromList,
                                 insert, lookup, union, delete, null)

main :: IO ()    
main = do {
       ; let empty_test_opts = mempty :: TestOptions
       ; let my_test_opts = empty_test_opts { topt_maximum_generated_tests = Just 1000 }
       ; let empty_runner_opts = mempty :: RunnerOptions
       ; let my_runner_opts = empty_runner_opts { ropt_test_options = Just my_test_opts }
       ; defaultMainWithOpts tests my_runner_opts
       }

tests =
  [ testGroup "=G= Lift Functions"
    [ testProperty "=P= Lifts Boolean Operations (2 points)"          liftBool_prop
    , testProperty "=P= Lifts Comparison Operations (2 points)"       liftComp_prop
    ]
  , testGroup "=G= eval Function"
    [ testProperty "=P= Constant Expressions (2 points)"              constExp_prop
    , testProperty "=P= Variable Expressions (2 points)"                varExp_prop
    , testProperty "=P= Integer Operation Expressions (3 points)"     intOpExp_prop
    , testProperty "=P= Comparison Operation Expressions (3 points)" compOpExp_prop
    , testProperty "=P= Boolean Operation Expressions (3 points)"    boolOpExp_prop
    , testProperty "=P= If Expressions (3 points)"                       ifExp_prop
    , testProperty "=P= Function Expressions (3 points)"                funExp_prop
    , testProperty "=P= Function Application -- 1 (4 points)"          appExp1_prop
    , testProperty "=P= Function Application -- 2 (4 points)"          appExp2_prop
    , testProperty "=P= Let Expressions --1 (4 points)"                letExp1_prop
    , testProperty "=P= Let Expressions --2 (4 points)"                letExp2_prop
    ]
  , testGroup "=G= exec Function"
    [ testProperty "=P= Assign Statements (2 points)"                  setStmt_prop
    , testProperty "=P= Sequence Statements (3 points)"                seqStmt_prop
    , testProperty "=P= If Statements (3 points)"                       ifStmt_prop
    , testProperty "=P= Procedure Statements (2 points)"              procStmt_prop
    , testProperty "=P= Call Procedure Statements (6 points)"         callStmt_prop
    ]
  ]

-- ######## 
-- newtypes 
-- ########


----- Operator Strings -------
newtype IntOp = IntOp (String, Int -> Int -> Int)
instance Show IntOp where
    show (IntOp (s,_))  = s

newtype BoolOp = BoolOp (String, Bool -> Bool -> Bool)
instance Show BoolOp where
    show (BoolOp (s,_))  = s

newtype CompOp = CompOp (String, Int -> Int -> Bool)
instance Show CompOp where
    show (CompOp (s,_))  = s
------------------------------
-- Variable names
newtype Param = Param { getString :: String }
    deriving (Show)

newtype ParamList = ParamList { getStrings :: [String] }
instance Show ParamList where
    show (ParamList ps) =  init ( show (take 7 ps) ) ++ "..."

---------------------
--Arbitrary Intances-
---------------------

-- Operator Strings
instance Arbitrary IntOp where
    arbitrary =  IntOp <$> elements (H.toList  intOps)
instance Arbitrary BoolOp where
    arbitrary = BoolOp <$> elements (H.toList boolOps)
instance Arbitrary CompOp where
    arbitrary = CompOp <$> elements (H.toList compOps)

-- Values
instance Arbitrary Val where
  arbitrary = oneof [ IntVal <$> arbitrary
                    , BoolVal <$> arbitrary
                    ]
                
instance Arbitrary Param where
    arbitrary = Param <$> (elements $ map (:"") ['a'..'z'])

instance Arbitrary ParamList where
    arbitrary = ParamList <$> (shuffle $ map (:"") ['a'..'z'])

                
-- Common Exception Values
liftExn = ExnVal "Cannot lift"
divExn  = ExnVal "Division by 0"
varExn  = ExnVal "No match in env"
ifExn   = ExnVal "Condition is not a Bool"
funExn  = ExnVal "Apply to non-closure"



--- Properties
--- ========

--- Lifting Functions
--- -----------------

liftBool_prop :: Int -> Bool -> Bool -> BoolOp -> Property
liftBool_prop i b1 b2 (BoolOp (s,op)) =
    (liftBoolOp op (BoolVal b1) (BoolVal b2) === (BoolVal $ b1 `op` b2)  .&.
     liftBoolOp op (IntVal   i) (BoolVal b2) === liftExn                 .&.
     liftBoolOp op (BoolVal b1) (IntVal   i) === liftExn)


liftComp_prop :: Bool -> Int -> Int -> CompOp -> Property
liftComp_prop b i1 i2 (CompOp (s,op)) =
    (liftCompOp op (IntVal i1) (IntVal i2) === (BoolVal $ i1 `op` i2)  .&.
     liftCompOp op (IntVal i1) (BoolVal b) === liftExn                 .&.
     liftCompOp op (BoolVal b) (IntVal i2) === liftExn)

--- eval
--- ----

--- ### Constants

constExp_prop :: Int -> Bool -> Property
constExp_prop i b = (eval (IntExp i) H.empty  ===  IntVal i) .&.
                    (eval (BoolExp b) H.empty === BoolVal b)



                    
--- ### Variables

varExp_prop :: String -> Val -> Property
varExp_prop var val = (eval (VarExp var) env     === val   ) .&.
                      (eval (VarExp var) H.empty === varExn)
    where env = H.singleton var val



                
-- ### Arithmetic

intOpExp_prop :: Int -> NonZero Int -> IntOp -> IntOp -> Property
intOpExp_prop i1 (NonZero i2) (IntOp (s1,op1)) (IntOp (s2,op2))  =
    ((eval (IntOpExp s1 (IntExp i1) (IntExp i2))                             H.empty === IntVal (i1 `op1` i2) )            .&.
     (eval (IntOpExp s1 (IntOpExp s2 (IntExp i1) (IntExp i2)) (IntExp (i2))) H.empty === IntVal ((i1 `op2` i2 ) `op1` i2)) .&.
     (eval (IntOpExp "/" (IntExp i1) (IntExp 0))                             H.empty === divExn)                           .&.
     (eval (IntOpExp s1 (IntExp i1) (IntOpExp "/" (IntExp i2) (IntExp 0)))   H.empty === liftExn))                         



--- ### Boolean and Comparison Operators

compOpExp_prop :: Int -> Int -> Bool -> CompOp-> Property
compOpExp_prop i1 i2 b (CompOp (s,op)) =
    ((eval (CompOpExp s (IntExp i1) (IntExp i2)) H.empty === BoolVal (i1 `op` i2)) .&.
     (eval (CompOpExp s (BoolExp b) (IntExp i2)) H.empty === liftExn            ))

                                          
boolOpExp_prop :: Int -> Bool -> Bool -> BoolOp-> Property
boolOpExp_prop i b1 b2 (BoolOp (s,op)) =
    ((eval (BoolOpExp s (BoolExp b1) (BoolExp b2)) H.empty === BoolVal (b1 `op` b2)) .&.
     (eval (BoolOpExp s   (IntExp i) (BoolExp b2)) H.empty === liftExn))


    
--- ### If Expressions

ifExp_prop :: Int -> Int -> Bool -> Bool ->  BoolOp -> IntOp -> Property
ifExp_prop i1 i2 b1 b2 (BoolOp (s1,op)) (IntOp (s2,_)) = 
    ((eval (IfExp (BoolOpExp s1 (BoolExp b1) (BoolExp b2)) (IntExp i1) (IntExp i2)) H.empty === if b1 `op` b2
                                                                                                then IntVal i1
                                                                                                else IntVal i2)  .&.
     (eval (IfExp (IntOpExp  s2  (IntExp i1)  (IntExp i2)) (IntExp i1) (IntExp i2)) H.empty === ifExn))


                                  
--- ### Functions

funExp_prop :: NonEmptyList Param -> Int -> IntOp -> Property
funExp_prop (NonEmpty (p:ps)) i (IntOp (s,_))=
    eval (FunExp ss (IntOpExp s (IntExp i) (IntExp 0))) env ===
    CloVal       ss (IntOpExp s (IntExp i) (IntExp 0))  env
    where env = H.singleton (getString p) (IntVal (i+3))
          ss = map getString ps



               
--- ### Function Application

testenv f1 f2 f3 p1 p2 b i1 iopS copS bopS =
    (H.fromList
     [ (f1, CloVal [p1,p2] (IntOpExp iopS (VarExp p1) (VarExp p2)) H.empty)
     , (f2, CloVal [p1,p2] (BoolOpExp bopS (BoolExp b) (CompOpExp copS (VarExp p1) (VarExp p2))) H.empty)
     , (f3, IntVal i1)
     ]
    )


appExp1_prop :: ParamList -> Int -> NonZero Int -> Bool -> IntOp -> CompOp -> BoolOp -> Property
appExp1_prop (ParamList (f1:f2:f3:p1:p2:p0:_)) i1 (NonZero i2) b (IntOp (iopS,iop)) (CompOp (copS,cop)) (BoolOp (bopS,bop)) =
    ((eval (AppExp (VarExp f1) [IntExp i1, IntExp i2]) env ===  IntVal  (i1 `iop` i2))          .&.
     (eval (AppExp (VarExp f2) [IntExp i1, IntExp i2]) env === BoolVal ((i1 `cop` i2) `bop` b)) .&.
     (eval (AppExp (VarExp f3) [IntExp i1])            env === funExn)                          .&.
     (eval (AppExp (FunExp [] (VarExp p0)) [])         env === varExn))
    where env = testenv f1 f2 f3 p1 p2 b i1 iopS copS bopS


appExp2_prop :: ParamList ->  Int -> NonZero Int -> Bool -> IntOp -> CompOp -> BoolOp -> Property
appExp2_prop (ParamList (f1:f2:f3:p1:p2:_)) i1 (NonZero i2) b (IntOp (iopS,iop)) (CompOp (copS,cop)) (BoolOp (bopS,bop)) =
    ((eval (AppExp (VarExp f1) [IntExp i2]) env2            ===  IntVal (i1 `iop` i2)) .&.
     (eval (AppExp (VarExp f2) [VarExp f3]) env2            === liftExn))
    where env1 = testenv f1 f2 f3 p1 p2 b i1 iopS copS bopS
          env2 = (H.fromList
                  [ (f1, CloVal [p1] (AppExp (VarExp f1) [VarExp f3, VarExp p1]) env1)
                  , (f2, CloVal [p2] (IntOpExp iopS (VarExp p2) (IntExp i2))  env1)
                  , (f3, ExnVal "Sad!")
                  ]
                 )

--- ### Let Expressions

letExp1_prop :: ParamList -> Int -> NonZero Int -> Bool -> IntOp -> CompOp -> BoolOp -> Property
letExp1_prop (ParamList (f1:f2:f3:p1:p2:_)) i1 (NonZero i2) b (IntOp (iopS,iop)) (CompOp (copS,cop)) (BoolOp (bopS,bop)) =
    ((eval (LetExp [(f1, IntOpExp iopS (IntExp i1) (IntExp i2)),
                    (f2, IntExp i1)] (CompOpExp copS (VarExp f1) (VarExp f2))) H.empty === BoolVal ((i1 `iop` i2) `cop` i1)) .&.
     (eval (LetExp [(f1, IntExp i1)] (VarExp f1)) env === IntVal i1))
    where env = testenv f1 f2 f3 p1 p2 b i1 iopS copS bopS 

                
letExp2_prop :: ParamList -> Int -> NonZero Int -> IntOp -> Property
letExp2_prop (ParamList (f:p:_)) i1 (NonZero i2) (IntOp (iopS,iop)) =
    (eval (LetExp [(p, FunExp [f, p] (AppExp (VarExp f) [VarExp p]))]
                     (AppExp (VarExp p) [ FunExp [p] (IntOpExp iopS (VarExp p) (IntExp i2))
                                        , IntExp i1
                                        ]
                     )
          ) H.empty === IntVal (i1 `iop` i2))


--- exec
--- ----

--- ### Assignment Statements
setStmt_prop :: ParamList -> NonZero Int -> IntOp -> Property
setStmt_prop (ParamList (f:x:_)) (NonZero i) (IntOp (iopS, iop)) = 
    (exec (SeqStmt [(SetStmt f (FunExp [x] (IntOpExp iopS (VarExp x) (IntExp i))))
                   , PrintStmt (AppExp (VarExp f) [IntExp i])]) H.empty H.empty ===
     (show (i `iop` i), H.empty, H.fromList [(f, CloVal [x] (IntOpExp iopS (VarExp x) (IntExp i)) H.empty)])) .&.
    (exec (SetStmt x (IntExp i)) H.empty H.empty == ("", H.empty, H.fromList [(x, IntVal i)]))


--- ### Sequence Statements

seqStmt_prop :: Int -> Int -> Param -> Property
seqStmt_prop i1 i2 (Param p) =
    ((exec (SeqStmt [PrintStmt (IntExp i1), PrintStmt (IntExp i2)]) H.empty H.empty === (show i1 ++ show i2, H.empty, H.empty)) .&.
     (exec (SeqStmt [PrintStmt (VarExp p), PrintStmt (IntExp i2)]) H.empty H.empty === ("exn: No match in env" ++ show i2, H.empty, H.empty)))



--- ### If Statements

ifStmt_prop :: Int -> Int -> ParamList -> CompOp -> Property
ifStmt_prop i1 i2 (ParamList (f:p1:p2:_)) (CompOp (copS,cop)) =
    (( (exec (IfStmt (AppExp (VarExp f) [IntExp i1, IntExp i2]) (PrintStmt (IntExp i1)) (PrintStmt (IntExp i2))) H.empty env) === (if i1 `cop` i2 then show i1 else show i2, H.empty, env)
     ) .&.
     ( (exec (IfStmt (FunExp [] (IntExp 0)) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty) === ("exn: Condition is not a Bool", H.empty, H.empty)))
    where env = H.fromList [(f, CloVal [p1,p2] (CompOpExp copS (VarExp p1) (VarExp p2)) H.empty)]


--- ### Procedure Declaration

testPenv f1 f2 f3 p1 p2 p3 i1 i2 b =
    (H.fromList
     [ (f1, ProcedureStmt f1 [] (SetStmt p1 (IntExp 5)))
     , (f2, ProcedureStmt f2 [p1] (IfStmt (CompOpExp "<" (VarExp p1) (IntExp i2))
                                   (SeqStmt [PrintStmt (VarExp p1),
                                             SetStmt p1 (IntOpExp "+" (VarExp p1) (IntExp 1)),
                                             CallStmt f2 [VarExp p1]]) (PrintStmt (BoolExp b))))
     , (f3, ProcedureStmt f3 [p1, p2, p3] (SetStmt p3 (AppExp (VarExp p1) [(AppExp (VarExp p2) [VarExp p3])])))
     ]
    )

procStmt_prop :: ParamList -> Int -> Positive Int -> Bool -> Property
procStmt_prop (ParamList (f1:f2:f3:p1:p2:p3:_)) i1 (Positive i2) b =
    ((exec (ProcedureStmt f2 [] (PrintStmt (BoolExp b))) penv H.empty === ("", H.insert f2 (ProcedureStmt f2 [] (PrintStmt (BoolExp b))) penv, H.empty)) .&.
     (exec (SeqStmt [SetStmt p1 (IntExp i1), ProcedureStmt f1 [] (PrintStmt (VarExp p1))]) H.empty H.empty ===
      ("", H.singleton f1 (ProcedureStmt f1 [] (PrintStmt (VarExp p1))), H.singleton p1 (IntVal i1)) )
    )
    where penv = testPenv f1 f2 f3 p1 p2 p3 i1 i2 b


                 
--- ### Procedure Call

callStmt_prop :: ParamList -> Int -> Positive Int -> Bool -> Property
callStmt_prop (ParamList (f1:f2:f3:p1:p2:p3:p0:_)) i1 (Positive i2) b =
    ( (i1 <= i2 ==>
              (exec (CallStmt f2 [IntExp i1]) penv H.empty === (concatMap (show) (init [i1..i2]) ++ show b, penv, H.singleton p1 (IntVal i2)))
      ) .&.
      (exec (CallStmt p0 []) penv H.empty === ("Procedure " ++ p0 ++ " undefined", penv, H.empty))
    )
    where penv = testPenv f1 f2 f3 p1 p2 p3 i1 i2 b
