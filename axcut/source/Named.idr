module Named

import Types

import Data.Vect


public export
Tag : Type
Tag = Nat

mutual
  public export
  data Statement : Nat -> Nat -> Nat -> Nat -> Type where
    Substitute : Vect b (Fin a) -> (r : Nat) -> Statement d i p r -> Statement d i p a
    Jump : Fin p -> Statement d i p a
    Let : {b : Nat} -> Fin d -> Tag -> Statement d i p (S a) -> Statement d i p (b + a)
    Switch : List (Clause d i p a) -> Statement d i p (S a)
    New : {b : Nat} -> Fin d -> List (Method d i p b) -> Statement d i p (S a) -> Statement d i p (b + a)
    Invoke : Tag -> Statement d i p (S a)
    Extern : Fin i -> List (Fin a) -> List (Clause d i p a) -> Statement d i p a

  public export
  data Clause : Nat -> Nat -> Nat -> Nat -> Type where
    ClauseBinder : (r : Nat) -> Statement d i p (r + a) -> Clause d i p a

  public export
  data Method : Nat -> Nat -> Nat -> Nat -> Type where
    MethodBinder : (r : Nat) -> Statement d i p (a + r) -> Method d i p a

public export
data Import : Nat -> Type where
  Interface : String -> Arguments d -> Signature d -> Import d

public export
data Program : Nat -> Nat -> Type where
  Definitions : Vect d (Signature d) -> Vect i (Import d) -> Vect p (Arguments d) -> Vect p (a ** Statement d i p a) -> Program d p

