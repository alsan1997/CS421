
module Common where

import Control.Applicative as X (Applicative)
import Control.Monad (liftM, ap)

import Data.Char
import Data.Map.Strict as H (Map, insert, lookup, union, empty, singleton, fromList, toList, mapWithKey)

  {- language definition -}

data Const =
  IntConst Int
  | BoolConst Bool
  | StringConst String
  | NilConst
  | UnitConst

data Monop = HdOp | TlOp | PrintOp | IntNegOp | FstOp | SndOp | NotOp

data Binop = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
  | ConcatOp | ConsOp | CommaOp | EqOp | GreaterOp

data Exp =
  ConstExp Const
  | VarExp String
  | MonOpExp Monop Exp
  | BinOpExp Binop Exp Exp
  | IfExp Exp Exp Exp
  | AppExp Exp Exp
  | FunExp String Exp
  | LetExp String Exp Exp
  | LetRecExp String String Exp Exp

data Dec =
  AnonDec Exp
  | LetDec String Exp
  | LetRec String String Exp

stringOfConst :: Const -> String
stringOfConst (IntConst i) = show i
stringOfConst (BoolConst b) = if b then "true" else "false"
stringOfConst (StringConst s) = "\"" ++ s ++ "\""
stringOfConst NilConst = "[]"
stringOfConst UnitConst = "()"

stringOfMonop :: Monop -> String
stringOfMonop HdOp = "hd"
stringOfMonop TlOp = "tl"
stringOfMonop PrintOp = "print"
stringOfMonop IntNegOp = "~"
stringOfMonop FstOp = "fst"
stringOfMonop SndOp = "snd"
stringOfMonop NotOp = "not"

stringOfBinop :: Binop -> String
stringOfBinop IntPlusOp = "+"
stringOfBinop IntMinusOp = "-"
stringOfBinop IntTimesOp = "*"
stringOfBinop IntDivOp = "/"
stringOfBinop ConcatOp = "^"
stringOfBinop ConsOp = "::"
stringOfBinop CommaOp = ","
stringOfBinop EqOp = "="
stringOfBinop GreaterOp = ">"

stringOfExp :: Exp -> String
stringOfExp (ConstExp c) = stringOfConst c
stringOfExp (VarExp x) = x
stringOfExp (MonOpExp m e') = "(" ++ (stringOfMonop m) ++ " " ++ (stringOfExp e') ++ ")"
stringOfExp (BinOpExp b e1 e2) =
  "(" ++ (stringOfExp e1) ++ " " ++ (stringOfBinop b) ++ " " ++ (stringOfExp e2) ++ ")"
stringOfExp (IfExp e1 e2 e3) =
  "(if " ++ (stringOfExp e1) ++ " then " ++ (stringOfExp e2) ++ " else " ++ (stringOfExp e3) ++ ")"
stringOfExp (AppExp e1 e2) =
  "(" ++ (stringOfExp e1) ++ " " ++ (stringOfExp e2) ++ ")"
stringOfExp (FunExp x e') =
  "(fun " ++ x ++ " -> " ++ (stringOfExp e') ++ ")"
stringOfExp (LetExp x e1 e2) =
  "(let " ++ x ++ " = " ++ (stringOfExp e1) ++ " in " ++ (stringOfExp e2) ++ ")"
stringOfExp (LetRecExp f x e1 e2) =
  "(let rec " ++ f ++ " " ++ x ++ " = " ++ (stringOfExp e1) ++ " in " ++ (stringOfExp e2) ++ ")"

stringOfDec :: Dec -> String
stringOfDec (AnonDec e') = stringOfExp e'
stringOfDec (LetDec x e') = "let " ++ x ++ " = " ++ (stringOfExp e')
stringOfDec (LetRec f x e') = "let rec " ++ f ++ " " ++ x ++ " = " ++ (stringOfExp e')

  {- type system definition -}

type VarId = Int

data MonoTy = TyVar VarId | TyConst String [MonoTy] deriving Eq
type PolyTy = ([VarId], MonoTy)

intTy = TyConst "int" []
boolTy = TyConst "bool" []
stringTy = TyConst "string" []
unitTy = TyConst "unit" []
listTy tau = TyConst "list" [tau]
pairTy t1 t2 = TyConst "pair" [t1, t2]
funTy t1 t2 = TyConst "fun" [t1, t2]

type TypeEnv = H.Map String PolyTy

type CanonEnv = H.Map VarId String

tVarName :: Int -> String
tVarName i = ['\'', chr (ord 'a' + i)]

canonize :: MonoTy -> CanonEnv
canonize tau =
  let canonRec env total (TyVar i) = case H.lookup i env of
        Nothing -> (H.insert i (tVarName total) env, total + 1)
        Just _ -> (env, total)
      canonRec env total (TyConst c []) = (env, total)
      canonRec env total (TyConst c [tau']) = canonRec env total tau'
      canonRec env total (TyConst c [tau1, tau2]) =
        let (env', total') = canonRec env total tau1
          in canonRec env' total' tau2
    in let (env, _) = canonRec H.empty 0 tau in env

stringOfMonoTy :: CanonEnv -> MonoTy -> String
stringOfMonoTy env (TyVar i) = case H.lookup i env of
  Nothing -> show i
  Just x -> x
stringOfMonoTy env (TyConst "fun" [tau1, tau2]) =
  "(" ++ (stringOfMonoTy env tau1) ++ " -> " ++ (stringOfMonoTy env tau2) ++ ")"
stringOfMonoTy env (TyConst "pair" [tau1, tau2]) =
  "(" ++ (stringOfMonoTy env tau1) ++ " * " ++ (stringOfMonoTy env tau2) ++ ")"
stringOfMonoTy env (TyConst "list" [tau']) =
  (stringOfMonoTy env tau') ++ " list"
stringOfMonoTy env (TyConst c _) = c

  {- error definitions -}

data TypeError =
  UnifError Exp
  | LookupError String

stringOfError :: TypeError -> String
stringOfError (UnifError e) = "Unification error from expression: " ++ (stringOfExp e)
stringOfError (LookupError x) = "Failed lookup of variable: " ++ x

  {- fresh variable monad -}

type FVCounter = Int

data FVRes a =
  ValidRes a FVCounter 
  | ErrorRes TypeError

data FVState a =
  ValidState (FVCounter -> FVRes a)
  | ErrorState TypeError

runFVState :: FVState a -> FVCounter -> FVRes a
runFVState (ErrorState e) _ = ErrorRes e
runFVState (ValidState f) v = f v

instance Functor FVState where
  fmap = liftM

instance X.Applicative FVState where
  pure  = return
  (<*>) = ap

instance Monad FVState where
  (ValidState i) >>= f  = ValidState (\s -> case i s of
      ValidRes v s' -> runFVState (f v) s'
      ErrorRes e -> ErrorRes e
    )
  (ErrorState e) >>= f  = ErrorState e
  return v              = ValidState (\s -> ValidRes v s)

freshTVar :: FVState VarId
freshTVar = ValidState (\i -> ValidRes i (i + 1))

freshTau :: FVState MonoTy
freshTau = freshTVar >>= (\i -> return (TyVar i))

throwError :: TypeError -> FVState a
throwError e = ErrorState e

  {- substitution definitions -}

type SubstEnv = H.Map VarId MonoTy

liftMonoTy :: SubstEnv -> MonoTy -> MonoTy
liftMonoTy sEnv (TyVar i) = case H.lookup i sEnv of
  Nothing -> TyVar i 
  Just tau -> tau
liftMonoTy sEnv (TyConst c tauList) = TyConst c (map (liftMonoTy sEnv) tauList)

liftPolyTy :: SubstEnv -> PolyTy -> PolyTy
liftPolyTy sEnv (qVars, tau) = (qVars, liftMonoTy sEnv tau)

liftEnv :: SubstEnv -> TypeEnv -> TypeEnv
liftEnv sEnv tEnv = H.mapWithKey (\x tau -> liftPolyTy sEnv tau) tEnv

substEmpty :: SubstEnv
substEmpty = H.empty

substInit :: VarId -> MonoTy -> SubstEnv
substInit i tau = H.singleton i tau

substCompose :: SubstEnv -> SubstEnv -> SubstEnv
substCompose sEnv1 sEnv2 = H.union sEnv1 (scRec (toList sEnv2))
  where
    scRec [] = sEnv2
    scRec ((i, tau) : t) = H.insert i (liftMonoTy sEnv1 tau) (scRec t)

stringOfSub :: SubstEnv -> Int -> String
stringOfSub sEnv total =
  let listRec i = if (i == total) then []
                  else case H.lookup i sEnv of
                    Nothing -> listRec (i + 1)
                    Just tau -> (i, tau) : (listRec (i + 1))
    in let sosRec l = case l of
                        [] -> ""
                        (i, tau) : t -> ", " ++ (show i) ++ " -> " ++ (stringOfMonoTy (canonize tau) tau) ++ (sosRec t)
      in case listRec 0 of
          [] -> "{}"
          (i, tau) : t -> "{" ++ (show i) ++ " -> " ++ (stringOfMonoTy (canonize tau) tau) ++ (sosRec t) ++ "}"

  {- auxiliary functions for type inferencing -}

constTySig :: Const -> FVState PolyTy
constTySig (IntConst _) = return ([], intTy)
constTySig (BoolConst _) = return ([], boolTy)
constTySig (StringConst _) = return ([], stringTy)
constTySig NilConst = freshTVar >>= (\i -> return ([i], listTy (TyVar i)))
constTySig UnitConst = return ([], unitTy)

monopTySig :: Monop -> FVState PolyTy
monopTySig HdOp = freshTVar >>= (\i -> return ([i], funTy (listTy (TyVar i)) (TyVar i))) 
monopTySig TlOp = freshTVar >>= (\i -> let lTau = listTy (TyVar i) in return ([i], funTy lTau lTau)) 
monopTySig PrintOp = return ([], funTy stringTy unitTy)
monopTySig IntNegOp = return ([], funTy intTy intTy)
monopTySig FstOp = do
  i <- freshTVar
  j <- freshTVar
  return ([i, j], funTy (pairTy (TyVar i) (TyVar j)) (TyVar i))
monopTySig SndOp = do
  i <- freshTVar
  j <- freshTVar
  return ([i, j], funTy (pairTy (TyVar i) (TyVar j)) (TyVar j)) 
monopTySig NotOp = return ([], funTy boolTy boolTy)

binopTySig :: Binop -> FVState PolyTy
binopTySig IntPlusOp = return ([], funTy intTy (funTy intTy intTy))
binopTySig IntMinusOp = return ([], funTy intTy (funTy intTy intTy))
binopTySig IntTimesOp = return ([], funTy intTy (funTy intTy intTy))
binopTySig IntDivOp = return ([], funTy intTy (funTy intTy intTy))
binopTySig ConcatOp = return ([], funTy stringTy (funTy stringTy stringTy))
binopTySig ConsOp = do
  i <- freshTVar
  let lTau = listTy (TyVar i)
  return ([i], funTy (TyVar i) (funTy lTau lTau))
binopTySig CommaOp = do
  i <- freshTVar
  j <- freshTVar
  return ([i, j], funTy (TyVar i) (funTy (TyVar j) (pairTy (TyVar i) (TyVar j))))
binopTySig EqOp = do
  i <- freshTVar
  return ([i], funTy (TyVar i) (funTy (TyVar i) boolTy))
binopTySig GreaterOp = return ([], funTy intTy (funTy intTy boolTy))

  {- GEN function -}

setInsert :: (Eq a) => [a] -> a -> [a]
setInsert [] v = [v]
setInsert (h:t) v = if v == h then h:t else h:(setInsert t v)

setUnion :: (Eq a) => [a] -> [a] -> [a]
setUnion [] s = s
setUnion (h:t) s = setInsert (setUnion t s) h

setRemove :: (Eq a) => [a] -> a -> [a]
setRemove [] v = []
setRemove (h:t) v = if v == h then t else h:(setRemove t v)

setDiff :: (Eq a) => [a] -> [a] -> [a]
setDiff s [] = s
setDiff s (h:t) = setRemove (setDiff s t) h

freeVarsMonoTy :: MonoTy -> [VarId]
freeVarsMonoTy (TyVar i) = [i]
freeVarsMonoTy (TyConst _ tauL) = foldr (setUnion . freeVarsMonoTy) [] tauL

freeVarsPolyTy :: PolyTy -> [VarId]
freeVarsPolyTy (qVars, tau) = setDiff (freeVarsMonoTy tau) qVars

freeVarsEnv :: TypeEnv -> [VarId]
freeVarsEnv env = foldr (setUnion . freeVarsPolyTy . snd) [] (H.toList env)

quantifyMonoTy :: MonoTy -> PolyTy
quantifyMonoTy tau = (freeVarsMonoTy tau, tau)

gen :: TypeEnv -> MonoTy -> PolyTy
gen env tau = (setDiff (freeVarsMonoTy tau) (freeVarsEnv env), tau)