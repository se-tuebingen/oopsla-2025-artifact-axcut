module Syntax

import Types


public export
Identifier : Type
Identifier = String

public export
Tag : Type
Tag = Nat


mutual
  public export
  data Statement : Type where
    Substitute : List Identifier -> Block -> Statement
    Jump : Identifier -> Statement
    Let : Identifier -> Identifier -> Tag -> List Identifier -> Statement -> Statement
    Switch : Identifier -> List Block -> Statement
    New : Identifier -> Identifier -> List Identifier -> List Block -> Statement -> Statement
    Invoke : Identifier -> Tag -> Statement
    Extern : String -> List Identifier -> List Block -> Statement

  public export
  data Block : Type where
    Binder : List Identifier -> Statement -> Block

public export
data Polarity : Type where
  Pro : Polarity
  Con : Polarity
  Ext : Polarity

public export
data Program : Type where
  End : Program
  Define : Identifier -> List Identifier -> List (Polarity, Identifier) -> Statement -> Program -> Program
  Typ : Identifier -> List (List (Polarity, Identifier)) -> Program -> Program
  Import : Identifier -> List (Polarity, Identifier) -> List (List (Polarity, Identifier)) -> Program -> Program

