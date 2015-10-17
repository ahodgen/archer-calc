{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
    GeneralizedNewtypeDeriving #-}

module Infer
    ( Constraint
    , TypeError(..)
    , Subst(..)
    , inferTop
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.RWS
import           Control.Monad.Identity
import           Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S

import           Env
import           Type
import           Syntax

-- | Inference monad
type Infer a =
    (RWST
        Env                 -- ^ Typing environment
        [Constraint]        -- ^ Generated constraints
        InferState          -- ^ Inference state
        (Except TypeError)  -- ^ Inference errors
        a                   -- ^ Result
    )

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (M.Map TVar Type)
    deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> S.Set TVar

instance Substitutable Type where
    apply _ (TCon a)       = TCon a
    apply (Subst s) t@(TVar a) = M.findWithDefault t a s
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2

    ftv TCon{}         = S.empty
    ftv (TVar a)       = S.singleton a
    ftv (t1 :-> t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
    apply (Subst s) (Forall as t)   = Forall as $ apply s' t
      where
        s' = Subst $ foldr M.delete s as
    ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 `S.union` ftv t2

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr (S.union . ftv) S.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
    ftv (TypeEnv env) = ftv $ M.elems env

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

{-
-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where
            sc = closeOver $ apply subst ty
-}

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty

-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- | Extend type environment
inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case M.lookup x env of
        Nothing   ->  throwError $ UnboundVariable x
        Just s    ->  instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ M.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t  = Forall as t
  where
    as = S.toList $ ftv t `S.difference` ftv env

ops :: M.Map Binop Type
ops = M.fromList
    [ (Add, typeNum  :-> typeNum  :-> typeNum)
    , (Mul, typeNum  :-> typeNum  :-> typeNum)
    , (Sub, typeNum  :-> typeNum  :-> typeNum)
    , (Div, typeNum  :-> typeNum  :-> typeNum)
    , (Exp, typeNum  :-> typeNum  :-> typeNum)
    , (Eql, typeA    :-> typeA    :-> typeBool)
    , (Gt,  typeNum  :-> typeNum  :-> typeBool)
    , (Gte, typeNum  :-> typeNum  :-> typeBool)
    , (Lt,  typeNum  :-> typeNum  :-> typeBool)
    , (Lte, typeNum  :-> typeNum  :-> typeBool)
    , (Neq, typeNum  :-> typeNum  :-> typeBool)
    , (Cat, typeText :-> typeText :-> typeText)
    , (And, typeBool :-> typeBool :-> typeBool)
    , (Or , typeBool :-> typeBool :-> typeBool)
    ]

infer :: Expr -> Infer Type
infer expr = case expr of
    Lit (LNum _)   -> return typeNum
    Lit (LBool _)  -> return typeBool
    Lit (LDate _)  -> return typeDate
    Lit (LText _)  -> return typeText
    Lit (LTimUn _) -> return typeTimeUnit
    Lit (LWkSt _)  -> return typeWeekStart

    Field _ Nothing t -> return t
    Field _ (Just e) t1 -> do
        t2 <- infer e
        uni t1 t2
        return t1

    Var x -> lookupEnv x

    Lam x e -> do
        tv <- fresh
        t <- inEnv (x, Forall [] tv) (infer e)
        return (tv :-> t)

    App e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        uni t1 (t2 :-> tv)
        return tv

    Let x e1 e2 -> do
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        t2 <- inEnv (x, sc) (infer e2)
        return t2

    Op op e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        let u1 = t1 :-> t2 :-> tv
            u2 = ops M.! op
        uni u1 u2
        return tv

    If cond tr fl -> do
        t1 <- infer cond
        t2 <- infer tr
        t3 <- infer fl
        uni t1 typeBool
        uni t2 t3
        return t2

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)  = [a]
    fv (a :-> b) = fv a <> fv b
    fv (TCon _)  = []

    normtype (a :-> b) = normtype a :-> normtype b
    normtype (TCon a)  = TCon a
    normtype (TVar a)  =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (t1 :-> t2) (t3 :-> t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ M.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `S.member` ftv t
