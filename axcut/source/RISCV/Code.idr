module RISCV.Code

import Data.Fin
import Data.String

public export
Register : Type
Register = Fin 32

-- x0 is always zero
-- x1 is used for our purposes
-- x2 is a heap pointer to an object which we can directly overwrite
-- x3 is a deferred-free-list pointer to objects which we have to free
public export
reserved : Nat
reserved = 4

public export
zero : Register
zero = 0

public export
temp : Register
temp = 1

public export
heap : Register
heap = 2

public export
free : Register
free = 3


public export
data Assembly : Type -> Type where
  ADD : Register -> Register -> Register -> Assembly a
  ADDI : Register -> Register -> a -> Assembly a
  JAL : Register -> a -> Assembly a
  JALR : Register -> Register -> a -> Assembly a
  LW : Register -> Register -> a -> Assembly a
  SW : Register -> Register -> a -> Assembly a
  BEQ : Register -> Register -> a -> Assembly a
  MUL : Register -> Register -> Register -> Assembly a
  REM : Register -> Register -> Register -> Assembly a
  BLT : Register -> Register -> a -> Assembly a

public export
data Immediate : Nat -> Type where
  Literal : Integer -> Immediate n
  Add : Immediate n -> Immediate n -> Immediate n
  Label : Fin n -> Immediate n
  This : Immediate n

public export
Instruction : Nat -> Type
Instruction n = Assembly (Immediate n)


public export
Code : Type
Code = Assembly Integer


(+) : String -> String -> String
(+) a b = a ++ " " ++ b

prettyRegister : Register -> String
prettyRegister r = "x" ++ show r

public export
prettyCode : Code -> String
prettyCode (ADD x y z) = "ADD" + prettyRegister x + prettyRegister y + prettyRegister z
prettyCode (ADDI x y c) = "ADDI" + prettyRegister x + prettyRegister y + show c
prettyCode (JAL x c) = "JAL" + prettyRegister x + show c
prettyCode (JALR x y c) = "JALR" + prettyRegister x + prettyRegister y + show c
prettyCode (LW x y c) = "LW" + prettyRegister x + show c + prettyRegister y
prettyCode (SW x y c) = "SW" + prettyRegister x + show c + prettyRegister y
prettyCode (BEQ x y c) = "BEQ" + prettyRegister x + prettyRegister y + show c
prettyCode (MUL x y z) = "MUL" + prettyRegister x + prettyRegister y + prettyRegister z
prettyCode (REM x y z) = "REM" + prettyRegister x + prettyRegister y + prettyRegister z
prettyCode (BLT x y c) = "BLT" + prettyRegister x + prettyRegister y + show c

public export
pretty : List Code -> String
pretty is = unlines (map prettyCode is)


