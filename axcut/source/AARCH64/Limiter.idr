module AARCH64.Limiter

import Types
import Typed
import AARCH64.Limited

import Data.String
import Data.Vect


-- TODO here we should enforce the following limits:
-- at most 14 variables in environment
-- at most 16 bit integer literals

limitExtern : {bs : Arguments d} -> {qs : Signature d} -> String -> Either String (Limited.Import bs qs)
limitExtern "restart" {bs = []} {qs = []} = pure Restart
limitExtern "return" {bs = [Ext "Int"]} {qs = []} = pure Return
limitExtern "add" {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} = pure Add
limitExtern "mul" {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} = pure Mul
limitExtern "rem" {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} = pure Rem
limitExtern "ifz" {bs = [Ext "Int"]} {qs = [[], []]} = pure IfZero
limitExtern "ife" {bs = [Ext "Int", Ext "Int"]} {qs = [[], []]} = pure IfEqual
limitExtern "ifl" {bs = [Ext "Int", Ext "Int"]} {qs = [[], []]} = pure IfLess
limitExtern lit {bs = []} {qs = [[Ext "Int"]]} with (unpack lit)
  _ | ('l' :: 'i' :: 't' :: '_' :: rest) =
    case parseInteger (pack rest) of
      Just n => pure (Literal n)
      Nothing => Left "invalid literal"
  limitExtern lit | _ = Left "unknown literal"
limitExtern name = Left ("unknown extern " ++ name)

mutual
  limitStatement : Typed.Statement ts ps as -> Either String (Limited.Statement ts ps as)
  limitStatement (Substitute xs s) = do
    s <- limitStatement s
    pure (Substitute xs s)
  limitStatement (Jump l) = do
    pure (Jump l)
  limitStatement (Let i s) = do
    s <- limitStatement s
    pure (Let i s)
  limitStatement (Switch ss) = do
    ss <- limitClauses ss
    pure (Switch ss)
  limitStatement (New ss s) = do
    ss <- limitMethods ss
    s <- limitStatement s
    pure (New ss s)
  limitStatement (Invoke i) = do
    pure (Invoke i)
  limitStatement (Extern n xs ss) = do
    n <- limitExtern n
    ss <- limitClauses ss
    pure (Extern n xs ss)

  limitClauses : Typed.Clauses ts ps as us -> Either String (Limited.Clauses ts ps as us)
  limitClauses [] = Right []
  limitClauses (s :: ss) = do
    s <- limitStatement s
    ss <- limitClauses ss
    pure (s :: ss)

  limitMethods : Typed.Methods ts ps as us -> Either String (Limited.Methods ts ps as us)
  limitMethods [] = Right []
  limitMethods (s :: ss) = do
    s <- limitStatement s
    ss <- limitMethods ss
    pure (s :: ss)

public export
limit : Table (Typed.Statement ts ps) qs -> Either String (Table (Limited.Statement ts ps) qs)
limit [] = Right []
limit (s :: ss) = do
  s <- limitStatement s
  ss <- limit ss
  pure (s :: ss)

