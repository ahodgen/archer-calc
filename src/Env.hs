module Env
    ( Env(..)
    , empty
    , lookup
    , remove
    , extend
    , extends
    , merge
    , mergeEnvs
    , singleton
    , keys
    , fromList
    , toList
    ) where

import           Prelude hiding (lookup)

import           Data.Foldable hiding (toList)
import qualified Data.Map as M
import           Data.Monoid

import           Syntax
import           Type

-- | Typing environment
data Env = TypeEnv { types :: M.Map Name Scheme }
    deriving (Eq, Show)

-- | Empty typing environment
empty :: Env
empty = TypeEnv M.empty

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = M.insert x s (types env) }

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (M.delete var env)

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env { types = M.union (M.fromList xs) (types env) }

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = M.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (M.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv (M.singleton x y)

keys :: Env -> [Name]
keys (TypeEnv env) = M.keys env

fromList :: [(Name, Scheme)] -> Env
fromList xs = TypeEnv (M.fromList xs)

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env) = M.toList env

instance Monoid Env where
    mempty = empty
    mappend = merge
