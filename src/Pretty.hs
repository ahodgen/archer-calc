{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty
    ( prompt
    , parensIf
    , ppcolor
    , Pretty (..)
    , Color (..)
    , ColorIntensity (..)
    , module Text.PrettyPrint
    ) where

import           System.Console.ANSI
import           Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

ppcolor :: ColorIntensity -> Color -> Doc -> Doc
ppcolor insty color x = text (setSGRCode [SetColor Foreground insty color])
                     <> x
                     <> text (setSGRCode [Reset])

prompt :: String
prompt = setSGRCode [SetConsoleIntensity BoldIntensity]
      ++ "ArcherCalc> "
      ++ setSGRCode [Reset]
