module Types

import Data.Fin


public export
data Tpe : Nat -> Type where
  Pro : Fin d -> Tpe d
  Con : Fin d -> Tpe d
  Ext : String -> Tpe d

public export
Arguments : Nat -> Type
Arguments d = List (Tpe d)

public export
Signature : Nat -> Type
Signature d = List (Arguments d)


public export
Leng : List (List a) -> List Nat
Leng xs = map length xs

public export
data Table : (f : k -> Type) -> List k -> Type where
  Nil : Table f []
  (::) : {0 a : k} -> f a -> Table f as -> Table f (a :: as)

public export
data Symbol : List k -> k -> Type where
  Z : Symbol (a :: as) a
  S : Symbol as a -> Symbol (b :: as) a

public export
length : Table f as -> Nat
length [] = Z
length (_ :: xs) = S (length xs)

public export
lookup : Table f as -> Symbol as a -> f a
lookup (v :: vs) Z = v
lookup (v :: vs) (S x) = lookup vs x

public export
concat : Table f as -> Table f bs -> Table f (as ++ bs)
concat [] ws = ws
concat (v :: vs) ws = v :: concat vs ws

public export
transform : ({0 e : k} -> f e -> g e) -> Table f as -> Table g as
transform g [] =  []
transform g (x :: xs) = g x :: transform g xs

public export
identity : {as : Arguments d} -> Table (Symbol as) as
identity {as = []} = []
identity {as = x :: xs} = Z :: transform S identity

