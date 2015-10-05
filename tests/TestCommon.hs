module TestCommon where

import           Control.Monad (foldM)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import           Test.Hspec

import           Eval
import           CodeGen
import           Parser
import           Pretty()
import           Types

-- | Built-in check
data BiCheck = BiCheck
    { emit  :: String                 -- ^ Expected code
    , value :: Either EvalError Value -- ^ Value expected
    , expr :: L.Text                  -- ^ Expression to test
    , defs :: L.Text                  -- ^ Field definitions
    }

checkBuiltIn :: BiCheck -> SpecWith ()
checkBuiltIn bi =
    it (emit bi <> " == " <> show (value bi)) $ do
        evTst (defs bi <> expr bi) `oper` value bi
        emTst (defs bi) (expr bi) `shouldBe` Right (emit bi)
  where
    oper = case value bi of
        Right (VNum _) -> circa
        _              -> shouldBe

-- | Just check to 2 decimal places, since the examples usually don't
--   show more precision.
circa :: Either EvalError Value -> Either EvalError Value -> Expectation
circa a b =
    apRound a `shouldBe` apRound b
  where
    apRound :: Either e Value -> Double
    apRound (Right (VNum x)) = fromIntegral (round (x * 100) :: Int) / 100
    apRound _ = 1/0

-- | Test evaluation
evTst :: L.Text -> Either EvalError Value
evTst bl = do
    let Right md = parseModule "test" bl
    tm <- foldM evalDef emptyTmenv md
    let Just ex = Prelude.lookup "it" md
    (val,_) <- runEval tm "it" ex
    Right val
  where
    evalDef en (nm, ex) = do
        (_,ctx) <- runEval en nm ex
        return ctx

-- | Test code generation
emTst :: L.Text -> L.Text -> Either CodeGenError String
emTst pre bl = do
    let Right md = parseModule "test" pre
    tm <- foldM emitDef M.empty md
    let Right md' = parseModule "test" bl
    let Just ex = Prelude.lookup "it" md'
    val <- codeGen tm ex
    Right val
  where
    emitDef en (nm, ex) = do
       ex' <- runExprCg en ex
       return $ M.insert nm ex' en
