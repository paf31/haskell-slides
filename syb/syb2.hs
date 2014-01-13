module Main where

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Pretty.Printer
import Language.JavaScript.Parser.AST

import Data.Data
import Data.Generics

minify input = either id id $ do
  js <- parse input ""
  let min = minify' js
  return $ renderToString min

minify' = everywhere (mkT removeSpaces)
  where
  removeSpaces (NT node _ _) = NT node tokenPosnEmpty []
  removeSpaces other = other 

main = interact minify
