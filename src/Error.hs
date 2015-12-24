module Error where

import           Text.Parsec.Pos (sourceLine, sourceColumn, SourcePos)
import qualified Text.Parsec.Error as PE

import           Pretty ()

data Pos = Pos
    { line   :: Int
    , column :: Int
    } deriving (Eq, Ord)

instance Show Pos where
    show x = "line " ++ (show . line) x ++ " column " ++ (show . column) x

fromSourcePos :: SourcePos -> Pos
fromSourcePos sourcePos =
    Pos (sourceLine sourcePos) (sourceColumn sourcePos)

class Error a where
    showError :: a -> String

instance Error PE.ParseError where
    showError x = "Parse error at " ++ show ep ++ ":\n" ++ msgs
      where
        ep   = fromSourcePos $ PE.errorPos x
        msgs  = PE.showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (PE.errorMessages x)
