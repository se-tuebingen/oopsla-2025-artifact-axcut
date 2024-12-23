module AARCH64.Code

import Data.Fin
import Data.String

public export
Register : Type
Register = Fin 31

-- x2 is used for our purposes
-- x0 is a heap pointer to an object which we can directly overwrite AND the first part of the return value
-- x1 is a deferred-free-list pointer to objects which we have to free AND the second part of the return value
public export
reserved : Nat
reserved = 3

public export
temp : Register
temp = 2

public export
heap : Register
heap = 0

public export
free : Register
free = 1

public export
return1 : Register
return1 = 0

public export
return2 : Register
return2 = 1


public export
jumpLength : Nat -> Integer
jumpLength n = natToInteger (4 * n)

public export
address : Nat -> Integer
address n = natToInteger (8 * n)


public export
FieldsPerBlock : Nat
FieldsPerBlock = 3

public export
referenceCountOffset : Integer
referenceCountOffset = address 0

public export
nextElementOffset : Integer
nextElementOffset = address 0

public export
fieldOffset1 : Nat -> Integer
fieldOffset1 i = address (2 + 2 * i)

public export
fieldOffset2 : Nat -> Integer
fieldOffset2 i = address (2 + S (2 * i))


public export
data Assembly : Type -> Type where
  ADD : Register -> Register -> Register -> Assembly a
  ADDI : Register -> Register -> a -> Assembly a
  SUBI : Register -> Register -> a -> Assembly a
  MUL : Register -> Register -> Register -> Assembly a
  SDIV : Register -> Register -> Register -> Assembly a
  MSUB : Register -> Register -> Register -> Register -> Assembly a
  B : String -> Assembly a
  BR : Register -> Assembly a
  ADR : Register -> String -> Assembly a
  MOVR : Register -> Register -> Assembly a
  MOVI : Register -> a -> Assembly a
  LDR : Register -> Register -> a -> Assembly a
  STR : Register -> Register -> a -> Assembly a
  CMPR : Register -> Register -> Assembly a
  CMPI : Register -> a -> Assembly a
  BEQ : String -> Assembly a
  BLT : String -> Assembly a
  LAB : String -> Assembly a


public export
Code : Type
Code = Assembly Integer


(+) : String -> String -> String
(+) a b = a ++ " " ++ b

prettyRegister : Register -> String
prettyRegister r = "X" ++ show r

public export
prettyCode : Code -> String
prettyCode (ADD x y z) = "ADD" + prettyRegister x ++ "," + prettyRegister y ++ "," + prettyRegister z
prettyCode (ADDI x y c) = "ADD" + prettyRegister x ++ "," + prettyRegister y ++ "," + show c
prettyCode (SUBI x y c) = "SUB" + prettyRegister x ++ "," + prettyRegister y ++ "," + show c
prettyCode (MUL x y z) = "MUL" + prettyRegister x ++ "," + prettyRegister y ++ "," + prettyRegister z
prettyCode (SDIV x y z) = "SDIV" + prettyRegister x ++ "," + prettyRegister y ++ "," + prettyRegister z
prettyCode (MSUB x y z v) = "MSUB" + prettyRegister x ++ "," + prettyRegister y ++ "," + prettyRegister z + "," + prettyRegister v
prettyCode (B l) = "B" + l
prettyCode (BR x) = "BR" + prettyRegister x
prettyCode (ADR x l) = "ADR" + prettyRegister x ++ "," + l
prettyCode (MOVR x y) = "MOV" + prettyRegister x ++ "," + prettyRegister y
prettyCode (MOVI x c) = "MOV" + prettyRegister x ++ "," + show c
prettyCode (LDR x y c) = "LDR" + prettyRegister x ++ "," + "[" + prettyRegister y ++ "," + (show c) + "]"
prettyCode (STR x y c) = "STR" + prettyRegister x ++ "," + "[" + prettyRegister y ++ "," + (show c) + "]"
prettyCode (CMPR x y) = "CMP" + prettyRegister x ++ "," + prettyRegister y
prettyCode (CMPI x c) = "CMP" + prettyRegister x ++ "," + show c
prettyCode (BEQ l) = "BEQ" + l
prettyCode (BLT l) = "BLT" + l
prettyCode (LAB l) = "\n" ++ l ++ ":"

public export
pretty : List Code -> String
pretty is = unlines (map prettyCode is)

header : String -> String
header name =
  "// To create an executable:\n" ++
  "// $ as -o" + name ++ ".aarch64.o" + name ++ ".aarch64.asm\n" ++
  "// $ gcc -o" + name + "path/to/AARCH64-infrastructure/driver.c" + name ++ ".aarch64.o"

moveParams : Nat -> String
moveParams 0 = ""
moveParams 1 =
  "MOV" + prettyRegister 4 ++ "," + prettyRegister 1
moveParams 2 =
  "MOV" + prettyRegister 6 ++ "," + prettyRegister 2 ++
  "\n" ++
  moveParams 1
moveParams 3 =
  "MOV" + prettyRegister 8 ++ "," + prettyRegister 3 ++
  "\n" ++
  moveParams 2
moveParams 4 =
  "MOV" + prettyRegister 10 ++ "," + prettyRegister 4 ++
  "\n" ++
  moveParams 3
moveParams 5 =
  "MOV" + prettyRegister 12 ++ "," + prettyRegister 5 ++
  "\n" ++
  moveParams 4
moveParams 6 =
  "MOV" + prettyRegister 14 ++ "," + prettyRegister 6 ++
  "\n" ++
  moveParams 5
moveParams 7 =
  "MOV" + prettyRegister 16 ++ "," + prettyRegister 7 ++
  "\n" ++
  moveParams 6
moveParams _ = assert_total (idris_crash "too many arguments for main")

setup : Nat -> String
setup argNum =
  ".text\n" ++
  "  .global asm_main0, _asm_main0\n" ++
  "  .global asm_main1, _asm_main1\n" ++
  "  .global asm_main2, _asm_main2\n" ++
  "  .global asm_main3, _asm_main3\n" ++
  "  .global asm_main4, _asm_main4\n" ++
  "  .global asm_main5, _asm_main5\n" ++
  "  .global asm_main6, _asm_main6\n" ++
  "  .global asm_main7, _asm_main7\n" ++
  "asm_main0:\n" ++
  "_asm_main0:\n" ++
  "asm_main1:\n" ++
  "_asm_main1:\n" ++
  "asm_main2:\n" ++
  "_asm_main2:\n" ++
  "asm_main3:\n" ++
  "_asm_main3:\n" ++
  "asm_main4:\n" ++
  "_asm_main4:\n" ++
  "asm_main5:\n" ++
  "_asm_main5:\n" ++
  "asm_main6:\n" ++
  "_asm_main6:\n" ++
  "asm_main7:\n" ++
  "_asm_main7:\n" ++
  "// setup\n" ++
  "// save registers\n" ++
  "STR X16, [sp, -16]!\n" ++
  "STR X17, [sp, -16]!\n" ++
  "STR X18, [sp, -16]!\n" ++
  "STR X19, [sp, -16]!\n" ++
  "STR X20, [sp, -16]!\n" ++
  "STR X21, [sp, -16]!\n" ++
  "STR X22, [sp, -16]!\n" ++
  "STR X23, [sp, -16]!\n" ++
  "STR X24, [sp, -16]!\n" ++
  "STR X25, [sp, -16]!\n" ++
  "STR X26, [sp, -16]!\n" ++
  "STR X27, [sp, -16]!\n" ++
  "STR X28, [sp, -16]!\n" ++
  "STR X29, [sp, -16]!\n" ++
  "STR X30, [sp, -16]!\n" ++
  "\n" ++
  "// move parameters into place\n" ++
  moveParams argNum ++
  "\n" ++
  "// initialize free pointer\n" ++
  "MOV" + prettyRegister free ++ "," + prettyRegister heap ++
  "\n" ++
  "ADD" + prettyRegister free ++ "," + prettyRegister free ++ "," + show (fieldOffset1 FieldsPerBlock)

cleanup : String
cleanup =
  "// cleanup\n" ++
  "cleanup:" ++
  "\n" ++
  "// restore registers\n" ++
  "LDR X30, [sp], 16\n" ++
  "LDR X29, [sp], 16\n" ++
  "LDR X28, [sp], 16\n" ++
  "LDR X27, [sp], 16\n" ++
  "LDR X26, [sp], 16\n" ++
  "LDR X25, [sp], 16\n" ++
  "LDR X24, [sp], 16\n" ++
  "LDR X23, [sp], 16\n" ++
  "LDR X22, [sp], 16\n" ++
  "LDR X21, [sp], 16\n" ++
  "LDR X20, [sp], 16\n" ++
  "LDR X19, [sp], 16\n" ++
  "LDR X18, [sp], 16\n" ++
  "LDR X17, [sp], 16\n" ++
  "LDR X16, [sp], 16\n" ++
  "RET"

public export
intoAARCH64Routine : String -> String -> Nat -> String
intoAARCH64Routine name prg argNum =
  header name ++
  "\n\n" ++
  setup argNum ++
  "\n\n" ++
  "// actual code" ++
  prg ++
  "\n" ++
  cleanup
