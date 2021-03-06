{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings,
    TypeSynonymInstances #-}

import Control.Monad.Trans.Class (lift)
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Version
import           Paths_archercalc
import           System.Console.Repline
import           System.Environment
import           System.Exit

import           BuiltIn
import           CodeGen
import qualified Env
import           EvalState
import           Error
import           Eval
import           Infer
import           Optimize
import           Parser
import           Pretty hiding ((<>))
import           Syntax
import           Type
import           Types

data IState = IState
    { tyctx :: Env.Env  -- Type environment
    , tmctx :: TermEnv  -- Value environment
    , syctx :: SynEnv   -- AST environment
    , cgctx :: CgEnv    -- CodeGen environment
    , evst  :: Est      -- Eval state (time, ids, etc.)
    }

initState :: IO IState
initState = do
    est <- eStateInit
    return $ IState envBuiltIn emptyTmenv M.empty M.empty est

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Error e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err)  = do
    liftIO . putStrLn . showError $ err
    abort

evalDef :: TermEnv -> (String, Expr) -> IO (Either EvalError TermEnv)
evalDef env (nm, ex) = do
    st <- get
    (_, tmctx') <- runEval (evst st) env nm ex
    return tmctx'

emitDef :: CgEnv -> (String, Expr) -> Either CodeGenError CgEnv
emitDef env (nm, ex) = do
    ex' <- runExprCg env ex
    return $ M.insert nm ex' env

exec :: Bool -> L.Text -> Repl ()
exec update source = do
    -- Get the current interpreter state
    st <- get

    -- Parse
    md <- hoistErr $ parseModule "<stdin>" source

    -- Type Inference
    tyctx' <- hoistErr $ inferTop (tyctx st) md

    -- Term evaluation
    tmctx' <- hoistErr $ foldM evalDef (tmctx st) md

    -- Code generation
    cgctx' <- hoistErr $ foldM emitDef (cgctx st) md

    -- Create the new environment
    let st' = st { tmctx = tmctx' -- foldl' evalDef (tmctx st) md
                 , tyctx = tyctx' <> tyctx st
                 , syctx = M.union (M.fromList md) (syctx st)
                 , cgctx = cgctx'
                 }

    -- Update the interpreter state
    when update (put st')

    -- If a value is entered, print it.
    case lookup "it" md of
        Nothing -> return ()
        Just ex -> do
            ret <- liftIO $ runEval (evst st) (tmctx st')  "it"  ex
            (val, _) <- hoistErr $ ret
            showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st =
    case Env.lookup "it" (tyctx st)  of
        Just val -> liftIO . putStrLn $ ppsignature (arg, val)
        Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-- :help command
help :: [String] -> Repl ()
help [x] = liftIO $ case M.lookup x builtIns of
    Nothing -> putStrLn $ "No help available for " <> x <> "."
    Just u  -> putStrLn $ ppusage x u
help _ = liftIO $ do
    putStrLn "  Command     Arguments   Purpose"
    putStrLn "  -----------------------------------------------------------"
    putStrLn "  :browse                 Browse type signatures"
    putStrLn "  :emit       <expr>      Emit an expression in Archer format"
    putStrLn "  :list                   List expressions"
    putStrLn "  :load       <filename>  Load a file"
    putStrLn "  :quit                   Exit ArcherCalc"
    putStrLn "  :type       <expr>      Check the type of an expression"
    putStrLn "  :help                   With no argument, shows this help"
    putStrLn "  :help       <builtin>   Show usage for a builtin function"

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
    st <- get
    liftIO . mapM_ putStrLn . Env.ppenv . tyctx $ st

-- :load command
load :: [String] -> Repl ()
load args = do
    contents <- liftIO . L.readFile . unwords $ args
    exec True contents

-- :list command
list :: [String] -> Repl ()
list _ = do
    st <- get
    liftIO . mapM_ (putStrLn . ppdecl) . M.toList . syctx $ st

-- :emit command
emit :: [String] -> Repl ()
emit args = do
    -- Get the current interpreter state
    st <- get

    -- Parse
    md <- hoistErr $ parseModule "<stdin>" (L.pack $ unwords args)

    -- Type Inference
    _ <- hoistErr $ inferTop (tyctx st) md

    a <- case lookup "it" md of
        Just ex -> hoistErr $ codeGen (cgctx st) ex
        Nothing -> return ":emit requires an expression to emit"
    liftIO $ putStrLn a

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
    st <- get
    let arg = unwords args
    case Env.lookup arg (tyctx st) of
        Just val -> liftIO . putStrLn . ppsignature $ (arg, val)
        Nothing -> exec False $ L.pack arg

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
    [ (":load"  , fileCompleter)
    ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
    let cmds = [":load", ":type", ":browse", ":help", ":quit", ":emit", ":list"]
    Env.TypeEnv ctx <- gets tyctx
    let defs = M.keys ctx
    return $ filter (isPrefixOf n) (cmds <> defs)

options :: [(String, [String] -> Repl ())]
options =
    [ ("load"   , load)
    , ("browse" , browse)
    , ("emit"   , emit)
    , ("list"   , list)
    , ("help"   , help)
    , ("quit"   , quit)
    , ("type"   , Main.typeof)
    ]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

banner :: IO ()
banner = mapM_ putStrLn logo
  where
    logo=
        [ "     _             _                ____      _       "
        , "    / \\   _ __ ___| |__   ___ _ __ / ___|__ _| | ___  "
        , "   / _ \\ | '__/ __| '_ \\ / _ \\ '__| |   / _` | |/ __| " <> ver
        , "  / ___ \\| | | (__| | | |  __/ |  | |__| (_| | | (__  " <> hp
        , " /_/   \\_\\_|  \\___|_| |_|\\___|_|   \\____\\__,_|_|\\___| "
        , ""
        , "ArcherCalc is provided with ABSOLUTELY NO WARRANTY."
        ]
    ver = "Version " <> showVersion version
    hp  = "Type :help for help"

shell :: Repl a -> IO ()
shell pre = do
    is <- initState
    flip evalStateT is $
        evalRepl prompt cmd options completer pre

main :: IO ()
main = do
    args <- getArgs
    case args of
        []              -> banner >> shell (return ())
        [fname]         -> shell (load [fname])
        ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
        _               -> putStrLn "invalid arguments"
