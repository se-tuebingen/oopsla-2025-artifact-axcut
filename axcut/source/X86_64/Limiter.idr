module X86_64.Limiter

import Types
import Typed
import X86_64.Limited

import Data.String
import Data.Vect


-- TODO here we should enforce the following limits
-- at most 133 variables in environment (unless a syscall is made, then it might be up to 4 less) (can be adapted via `spillNum` in X86_64/Code.idr)
-- at most 1M size of memory (can be adapted via `heapsize` in X86_64-infrastructure/driver.c)


limitExtern : {bs : Arguments d} -> {qs : Signature d} -> String -> Either String (Limited.Import bs qs)
limitExtern "restart" {bs = []} {qs = []} = pure Restart
limitExtern "return" {bs = [Ext "Int"]} {qs = []} = pure Return
limitExtern "add" {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} = pure Add
limitExtern "sub" {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} = pure Sub
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
limitExtern "mmap_anon_page" {bs = []} {qs = [[Ext "Page"]]} = pure MMapAnonymousPage
limitExtern "munmap_page" {bs = [Ext "Page"]} {qs = [[]]} = pure MUnmapPage
limitExtern "get_buffer_byte" {bs = [Ext "Page", Ext "Int"]} {qs = [[Ext "Int"]]} = pure GetBufferByte
limitExtern "set_buffer_byte" {bs = [Ext "Page", Ext "Int", Ext "Int"]} {qs = [[]]} = pure SetBufferByte
limitExtern "read_stdin" {bs = [Ext "Page", Ext "Int"]} {qs = [[Ext "Int"]]} = pure ReadStdin
limitExtern "write_stdout" {bs = [Ext "Page", Ext "Int"]} {qs = [[Ext "Int"]]} = pure WriteStdout
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

