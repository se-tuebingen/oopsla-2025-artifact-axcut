module Semantics

import Types
import Typed

import Data.Vect
import Data.String


mutual
  data Value : Vect d (Signature d) -> List (Arguments d) -> Tpe d -> Type where
    Number : Integer -> Value ts ps (Ext "Int")
    Product : Tag (index t ts) as -> Environment ts ps as -> Value ts ps (Pro t)
    Conduct : Environment ts ps as -> Methods ts ps as (index t ts) -> Value ts ps (Con t)

  public export
  Environment : Vect d (Signature d) -> List (Arguments d) -> Arguments d -> Type
  Environment ts ps as = Table (Value ts ps) as

take : {bs : Arguments d} -> Environment ts ps (bs ++ as) -> Environment ts ps bs
take {bs = []} vs = []
take {bs = b :: bs} (v :: vs) = v :: take vs

drop : {bs : Arguments d} -> Environment ts ps (bs ++ as) -> Environment ts ps as
drop {bs = []} vs = vs
drop {bs = b :: bs} (v :: vs) = drop vs

public export
data Machine : Type where
  Run : Statement ts ps as -> Program ts ps -> Environment ts ps as -> Machine

public export
total
step : Machine -> Machine
step (Run (Substitute e s) ls vs) =
  Run s ls (transform (lookup vs) e)
step (Run (Jump l) ls vs) =
  Run (lookup ls l) ls vs
step (Run (Let i s) ls vs) =
  Run s ls (Product i (take vs) :: drop vs)
step (Run (Switch cs) ls (Product i vs :: ws)) =
  Run (lookup cs i) ls (concat vs ws)
step (Run (New ms s) ls vs) =
  Run s ls (Conduct (take vs) ms :: drop vs)
step (Run (Invoke i) ls (Conduct vs ms :: ws)) =
  Run (lookup ms i) ls (concat vs ws)
step (Run (Extern {bs = []} {qs = [[Ext "Int"]]} lit [] [s]) ls vs) =
 case unpack lit of
  'l' :: 'i' :: 't' :: '_' :: rest => case parseInteger (pack rest) of
    Just n => Run s ls (Number n :: vs)
    Nothing => (Run (Extern {bs = []} {qs = [[Ext "Int"]]} lit [] [s]) ls vs)
  _ => (Run (Extern {bs = []} {qs = [[Ext "Int"]]} lit [] [s]) ls vs)
step (Run (Extern {bs = [Ext "Int", Ext "Int"]} {qs = [[Ext "Int"]]} "add" [x, y] [s]) ls vs) =
  case lookup vs x of
    Number n => case lookup vs y of
      Number m => Run s ls (Number (n + m) :: vs)
step (Run (Extern {bs = [Ext "Int"]} {qs = [[],[]]} "ifz" [x] [z, s]) ls vs) =
  case lookup vs x of
    Number n => if n == 0
      then Run z ls vs
      else Run s ls vs
step (Run (Extern name is bs) ls vs) = (Run (Extern name is bs) ls vs)
