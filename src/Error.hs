module Error where

import           Text.Parsec.Pos (sourceLine, sourceColumn, SourcePos)
import qualified Text.Parsec.Error as PE

import           Pretty ()
import           Type
import           Types

data Position = Position
    { line   :: Int
    , column :: Int
    } deriving (Eq, Show)

fromSourcePos :: SourcePos -> Position
fromSourcePos sourcePos =
    Position (sourceLine sourcePos) (sourceColumn sourcePos)

class Error a where
    showError :: a -> String

instance Error PE.ParseError where
    showError x = "Parse error at line " ++ show (line ep :: Int) ++
                " column " ++ show (column ep :: Int) ++ ":\n" ++
                msgs
      where
        ep   = fromSourcePos $ PE.errorPos x
        msgs  = PE.showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (PE.errorMessages x)

instance Error TypeError where
    showError = show

instance Error CodeGenError where
    showError = show

instance Error EvalError where
    showError = show
