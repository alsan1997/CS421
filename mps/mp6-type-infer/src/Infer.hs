--partner: Alex zhang, rahul sirasao, yiyang wang
module Infer where

import Common

import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, union, singleton)

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> FVState MonoTy
freshInst (qVars, tau) = 
  do qs <- mapM (const freshTau) qVars
     let sub = fromList (zip qVars qs)
     return $ liftMonoTy sub tau

freshInstFV :: FVState PolyTy -> FVState MonoTy
freshInstFV s = s >>= (\tau -> freshInst tau)

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = i `elem` freeVarsMonoTy tau

  {- question 3: unification -}

sub_st :: VarId -> MonoTy -> [(MonoTy, MonoTy)] -> [(MonoTy, MonoTy)]
sub_st _ _ [] = []
sub_st id t ((a,b):c) = (((helper_sub_st id t a), (helper_sub_st id t b)) : (sub_st id t c))

helper_sub_st :: VarId -> MonoTy -> MonoTy -> MonoTy
helper_sub_st id t tau@(TyVar id') | id == id' = t
                                     | otherwise = tau
helper_sub_st id t (TyConst st tauList) = TyConst st (map (helper_sub_st id t) tauList)

unify :: Exp -> [(MonoTy, MonoTy)] -> FVState SubstEnv
unify _ [] = return substEmpty
unify exp ((s,t) : c') | s == t = unify exp c'
unify exp (((a@(TyConst _ _)), (b@(TyVar _))):c') = unify exp ((b,a) : c')
unify exp ((TyConst s xs, TyConst t ys) : c') | s == t && length xs == length ys = unify exp (c' ++ zip xs ys)
unify exp ((TyVar s,t) : c') | not $ occurs s t = let c'' = sub_st s t c'
                                                  in do phi <- unify exp c''
                                                        return $ H.insert s (liftMonoTy phi t) phi
unify exp _ = throwError $ UnifError exp

  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> MonoTy -> FVState SubstEnv
infer env exp tau = case exp of
  LetRecExp f x e1 e -> do
    tau_1 <- freshTau
    tau_2 <- freshTau
    let p1 = H.singleton x ([],tau_1)
    let p2 = H.singleton f ([], funTy tau_1 tau_2)
    let env_1 = H.union env (H.union p1 p2)
    sub_1 <- infer env_1 e1 tau_2
    let p3 = gen (liftEnv sub_1 env) (liftMonoTy sub_1 (funTy tau_1 tau_2))
    let p4 = liftEnv sub_1 env
    let env_2 = H.union (H.singleton f p3) p4
    sub_2 <- infer env_2 e (liftMonoTy sub_1 tau)
    return $ substCompose sub_2 sub_1

  AppExp e1 e2 -> do
    tau_1 <- freshTau
    sub_1 <- infer env e1 (funTy tau_1 tau)
    let env' = liftEnv sub_1 env
    sub_2 <- infer env' e2 (liftMonoTy sub_1 tau_1)
    return $ substCompose sub_2 sub_1

  FunExp x e -> do
    tau_1 <- freshTau
    tau_2 <- freshTau
    let sc = ([], tau_1)
    let env' = H.union (H.singleton x sc) env
    sub <- infer env' e tau_2
    subU <- unify exp [(liftMonoTy sub tau, liftMonoTy sub (funTy tau_1 tau_2) )]
    return $ substCompose subU sub

  IfExp e1 e2 e3 -> do
    sub_1 <- infer env e1 boolTy
    let env_1 = liftEnv sub_1 env
    sub_2 <- infer env_1 e2 (liftMonoTy sub_1 tau)
    let subT = substCompose sub_2 sub_1
    let env_2 = liftEnv subT env
    let tmpT = liftMonoTy subT tau
    sub3 <- infer env_2 e3 tmpT
    return $ substCompose sub3 (substCompose sub_2 sub_1)

  BinOpExp op e1 e2 -> do
    tau_1 <- freshTau
    tau_2 <- freshTau
    sub_1 <- infer env e1 tau_1
    sub_2 <- infer (liftEnv sub_1 env) e2 tau_2
    let sub_21 = substCompose sub_2 sub_1
    let p1 = liftMonoTy sub_21 (funTy tau_1 (funTy tau_2 tau))
    p2 <- freshInstFV (binopTySig op)
    subU <- unify exp [(p1, p2)]
    return (substCompose subU sub_21)

  MonOpExp op e1 -> do
    tau_1 <- freshTau
    sub  <- infer env e1 tau_1
    let p1 = liftMonoTy sub (funTy tau_1 tau)
    p2 <- freshInstFV (monopTySig op)
    subU <- unify exp [(p1,p2)]
    return $ substCompose subU sub

  LetExp x e1 e -> do
    tau_1 <- freshTau
    sub_1 <- infer env e1 tau_1
    let xVal = gen (liftEnv sub_1 env) (liftMonoTy sub_1 tau_1)
    let env' = H.union (H.singleton x xVal) (liftEnv sub_1 env)
    sub_2 <- infer env' e (liftMonoTy sub_1 tau)
    return $ substCompose sub_2 sub_1

  VarExp x -> case H.lookup x env of
    Nothing -> throwError $ LookupError x
    Just s -> do
      tau' <- freshInst s
      unify exp [(tau,tau')]

  ConstExp c -> do
    tau' <- freshInstFV (constTySig c)
    unify exp [(tau,tau')]
   

inferInit :: TypeEnv -> Exp -> FVState MonoTy
inferInit env e = do
  tau <- freshTau
  sEnv <- infer env e tau
  return (liftMonoTy sEnv tau)

inferDec :: TypeEnv -> Dec -> FVState (TypeEnv, MonoTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x (quantifyMonoTy tau) env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f (quantifyMonoTy tau) env, tau)