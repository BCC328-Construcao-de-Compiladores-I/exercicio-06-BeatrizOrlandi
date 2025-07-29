module L.L2.Backend.CCodegen (cL2Codegen) where

import L.L2.Frontend.Syntax
import Utils.Pretty

cL2Codegen :: L2 -> String
cL2Codegen p =
  unlines $
    [ "#include <stdio.h>"
    , "int main() {"
    ] ++ map (nest 2) (goProg p)
      ++ [ nest 2 "putchar('\\n');"
         , nest 2 "return 0;"
         , "}"
         ]
  where nest n = (replicate n ' ' ++)

goProg :: L2 -> [String]
goProg (L2 ss) = concatMap goStmt ss

goStmt :: S2 -> [String]
goStmt = \case
  LAssign (Var v) e ->
    [ "int " ++ pretty v ++ " = " ++ goExp e ++ ";" ]
  LPrint e ->
    [ "printf(\"%d\", " ++ goExp e ++ ");" ]
  LRead s (Var v) ->
    [ "printf(\"" ++ s ++ "\");"
    , "scanf(\"%d\", &" ++ pretty v ++ ");" ]
  Def (Var v) e body ->
    [ "{ /* def " ++ pretty v ++ " */"
    , "  const int " ++ pretty v ++ " = " ++ goExp e ++ ";"
    ]
    ++ (map (nest 2) (concatMap goStmt body))
    ++ ["}"]
    where nest n = (replicate n ' ' ++)
goExp :: E2 -> String
goExp = \case
  LVal (VInt n) -> show n
  LVal (VStr s) -> "\"" ++ s ++ "\""
  LVar (Var v)  -> pretty v
  LAdd a b      -> bin "+" a b
  LMinus a b    -> bin "-" a b
  LMul a b      -> bin "*" a b
  LDiv a b      -> bin "/" a b
  where bin op a b = "(" ++ goExp a ++ op ++ goExp b ++ ")"
