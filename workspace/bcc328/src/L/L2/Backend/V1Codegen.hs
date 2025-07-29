{-# LANGUAGE LambdaCase #-}

module L.L2.Backend.V1Codegen (v1Codegen) where

import           L.L2.Frontend.Syntax
import           Utils.Pretty              (pretty)

-- | Convenção: geramos uma lista de strings, cada uma é uma instrução V1
--   (a sintaxe exacta da V1 já é usada no gerador de L1, então copiamos a ideia).
v1Codegen :: L2 -> [String]
v1Codegen (L2 ss) = concatMap genStmt ss

------------------------------------------------------------
-- geração statement / expression
------------------------------------------------------------

genStmt :: S2 -> [String]
genStmt = \case
  LAssign (Var v) e ->
      genExp e ++ ["STORE " ++ pretty v]
  LPrint e ->
      genExp e ++ ["PRINT"]
  LRead msg (Var v) ->
      ["STR \"" ++ msg ++ "\"", "READ", "STORE " ++ pretty v]
  Def (Var v) e body ->
      -- avalia expressão, deixa no topo e salva em endereço temporário;
      -- corpo é compilado normalmente; no fim descartamos valor (POP).
      genExp e
      ++ ["STORE_IMM " ++ pretty v]
      ++ concatMap genStmt body
      ++ ["POP"]  -- elimina o valor imutável da pilha

genExp :: E2 -> [String]
genExp = \case
  LVal  (VInt n)     -> ["PUSH_INT " ++ show n]
  LVal  (VStr s)     -> ["PUSH_STR \"" ++ s ++ "\""]
  LVar  (Var v)      -> ["LOAD " ++ pretty v]
  LAdd a b           -> bin a b "ADD"
  LMinus a b         -> bin a b "SUB"
  LMul a b           -> bin a b "MUL"
  LDiv a b           -> bin a b "DIV"
  where
    bin a b op = genExp a ++ genExp b ++ [op]
