module X86_64.Code

import Data.Fin
import Data.Vect
import Data.String

public export
RegisterNum : Nat
RegisterNum = 16

public export
Register : Type
Register = Fin RegisterNum

-- rsp is the stack pointer we use for register spills
-- rcx is used for our purposes
-- rbx is a heap pointer to an object which we can directly overwrite
-- rbp is a deferred-free-list pointer to objects which we have to free
public export
reserved : Nat
reserved = 4

public export
stack : Register
stack = 0

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
return1 : Register
return1 = 4

public export
return2 : Register
return2 = 5


public export
-- could probably be determined by liveness analysis
SpillNum : Nat
SpillNum = 256

public export
Spill : Type
Spill = Fin SpillNum

public export
spillSpace : Integer
spillSpace = (natToInteger SpillNum) * 8

-- one spot is used for our purposes
public export
reservedSpills : Nat
reservedSpills = 1

public export
spillTemp : Spill
spillTemp = 0

public export
stackOffset : Spill -> Integer
stackOffset position = spillSpace - (8 * ((finToInteger position) + 1))

public export
Temporary : Type
Temporary = Either Register Spill


public export
jumpLength : Nat -> Integer
jumpLength n = 5 * (natToInteger n)

public export
address : Nat -> Integer
address n = 8 * (natToInteger n)


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
sysNR : Register
sysNR = 4

public export
sysNRSpill : Temporary
sysNRSpill = Left 12

public export
arg : Nat -> Register
arg 0 = 7
arg 1 = 6
arg 2 = 5
arg 3 = 10
arg 4 = 8
arg 5 = 9
arg _ = assert_total (idris_crash "syscalls can have 6 arguments at most")

public export
argSpill : Nat -> Temporary
argSpill 0 = Left 13
argSpill 1 = Left 14
argSpill 2 = Left 15
argSpill 3 = Right 1
argSpill 4 = Right 2
argSpill 5 = Right 3
argSpill _ = assert_total (idris_crash "syscalls can have 6 arguments at most")

public export
clobbered : Register
clobbered = 11

public export
registersToSave : Vect 7 Register
registersToSave = clobbered :: (arg 0) :: (arg 1) :: (arg 2) :: (arg 3) :: (arg 4) :: (arg 5) :: []

public export
read : Integer
read = 0

public export
write : Integer
write = 1

public export
mmap : Integer
mmap = 9

public export
munmap : Integer
munmap = 11

public export
stdin : Integer
stdin = 0

public export
stdout : Integer
stdout = 1

public export
pageSize : Integer
pageSize = 4096

public export
protRead : Integer
protRead = 0x1

public export
protWrite : Integer
protWrite = 0x2

public export
mapPrivate : Integer
mapPrivate = 0x02

public export
mapAnonymous : Integer
mapAnonymous = 0x20


public export
fuseFins : {a: Nat} -> Either (Fin a) (Fin b) -> Fin (a + b)
fuseFins (Left x) = weakenN b x
fuseFins (Right y) = shift a y

public export
unfuseFins : {a: Nat} -> Fin (a + b) -> Either (Fin a) (Fin b)
unfuseFins {a = Z} n = Right n
unfuseFins {a = S k} FZ = Left FZ
unfuseFins {a = S k} (FS n) = case unfuseFins {a = k} n of
  Left m => Left (FS m)
  Right m => Right m


public export
data Assembly : Type -> Type where
  ADD : Register -> Register -> Assembly a
  ADDM : Register -> Register -> a -> Assembly a
  --ADDMR : Register -> a -> Register -> Assembly a
  ADDI : Register -> a -> Assembly a
  ADDIM : Register -> a -> a -> Assembly a
  SUB : Register -> Register -> Assembly a
  SUBM : Register -> Register -> a -> Assembly a
  --SUBMR : Register -> a -> Register -> Assembly a
  --NEG : Register -> Assembly a
  --NEGM : Register -> a -> Assembly a
  IMUL : Register -> Register -> Assembly a
  IMULM : Register -> Register -> a -> Assembly a
  IDIV : Register -> Assembly a
  IDIVM : Register -> a -> Assembly a
  CQO : Assembly a
  JMP : Register -> Assembly a
  --JMPI : a -> Assembly a
  JMPL : String -> Assembly a
  JMPLN : String -> Assembly a
  LEAL : Register -> String -> Assembly a
  MOV : Register -> Register -> Assembly a
  MOVS : Register -> Register -> a -> Assembly a
  MOVRB : Register -> Register -> a -> Assembly a
  MOVL : Register -> Register -> a -> Assembly a
  MOVZX : Register -> Register -> a -> Assembly a
  MOVI : Register -> a -> Assembly a
  MOVIM : Register -> a -> a -> Assembly a
  CMP : Register -> Register -> Assembly a
  CMPRM : Register -> Register -> a -> Assembly a
  CMPMR : Register -> a -> Register -> Assembly a
  CMPI : Register -> a -> Assembly a
  CMPIM : Register -> a -> a -> Assembly a
  --JE : a -> Assembly a
  JEL : String -> Assembly a
  JLTL : String -> Assembly a
  LAB : String -> Assembly a
  --PUSH : Register -> Assembly a
  --POP : Register -> Assembly a
  SYSCALL : Assembly a


public export
Code : Type
Code = Assembly Integer


(+) : String -> String -> String
(+) a b = a ++ " " ++ b

prettyRegister : Register -> String
prettyRegister FZ = "rsp"
prettyRegister (FS FZ) = "rcx"
prettyRegister (FS (FS FZ)) = "rbx"
prettyRegister (FS (FS (FS FZ))) = "rbp"
prettyRegister (FS (FS (FS (FS FZ)))) = "rax"
prettyRegister (FS (FS (FS (FS (FS FZ))))) = "rdx"
prettyRegister (FS (FS (FS (FS (FS (FS FZ)))))) = "rsi"
prettyRegister (FS (FS (FS (FS (FS (FS (FS FZ))))))) = "rdi"
prettyRegister r = "r" ++ show r

prettyRegisterByte : Register -> String
prettyRegisterByte FZ = "spl"
prettyRegisterByte (FS FZ) = "cl"
prettyRegisterByte (FS (FS FZ)) = "bl"
prettyRegisterByte (FS (FS (FS FZ))) = "bpl"
prettyRegisterByte (FS (FS (FS (FS FZ)))) = "al"
prettyRegisterByte (FS (FS (FS (FS (FS FZ))))) = "dl"
prettyRegisterByte (FS (FS (FS (FS (FS (FS FZ)))))) = "sil"
prettyRegisterByte (FS (FS (FS (FS (FS (FS (FS FZ))))))) = "dil"
prettyRegisterByte r = "r" ++ show r ++ "b"

public export
prettyCode : Code -> String
prettyCode (ADD x y) = "add" + prettyRegister x ++ "," + prettyRegister y
prettyCode (ADDM x y c) = "add" + prettyRegister x ++ "," + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
--prettyCode (ADDMR x c y) = "add" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + prettyRegister y
prettyCode (ADDI x c) = "add" + prettyRegister x ++ "," + show c
prettyCode (ADDIM x c d) = "add qword" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + show d
prettyCode (SUB x y) = "sub" + prettyRegister x ++ "," + prettyRegister y
prettyCode (SUBM x y c) = "sub" + prettyRegister x ++ "," + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
--prettyCode (SUBMR x c y) = "sub" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + prettyRegister y
--prettyCode (NEG x) = "neg" + prettyRegister x
--prettyCode (NEGM x c) = "neg [" ++ prettyRegister x + "+" + (show c) ++ "]"
prettyCode (IMUL x y) = "imul" + prettyRegister x ++ "," + prettyRegister y
prettyCode (IMULM x y c) = "imul" + prettyRegister x ++ "," + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
prettyCode (IDIV x) = "idiv" + prettyRegister x
prettyCode (IDIVM x c) = "idiv qword" + "[" ++ prettyRegister x + "+" + (show c) ++ "]"
prettyCode CQO = "cqo"
prettyCode (JMP x) = "jmp" + prettyRegister x
--prettyCode (JMPI c) = "jmp near $ +" + show c
prettyCode (JMPL l) = "jmp" + l
prettyCode (JMPLN l) = "jmp near" + l
prettyCode (LEAL x l) = "lea" + prettyRegister x ++ "," + "[rel" + l ++ "]"
prettyCode (MOV x y) = "mov" + prettyRegister x ++ "," + prettyRegister y
prettyCode (MOVS x y c) = "mov" + "[" ++ prettyRegister y + "+" + (show c) ++ "]" ++ "," + prettyRegister x
prettyCode (MOVRB x y c) = "mov byte" + "[" ++ prettyRegister y + "+" + (show c) ++ "]" ++ "," + prettyRegisterByte x
prettyCode (MOVL x y c) = "mov" + prettyRegister x ++ "," + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
prettyCode (MOVZX x y c) = "movzx" + prettyRegister x ++ ", byte" + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
prettyCode (MOVI x c) = "mov" + prettyRegister x ++ "," + show c
prettyCode (MOVIM x c d) = "mov qword" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + show d
prettyCode (CMP x y) = "cmp" + prettyRegister x ++ "," + prettyRegister y
prettyCode (CMPRM x y c) = "cmp" + prettyRegister x ++ "," + "[" ++ prettyRegister y + "+" + (show c) ++ "]"
prettyCode (CMPMR x c y) = "cmp" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + prettyRegister y
prettyCode (CMPI x c) = "cmp" + prettyRegister x ++ "," + show c
prettyCode (CMPIM x c d) = "cmp qword" + "[" ++ prettyRegister x + "+" + (show c) ++ "]" ++ "," + show d
--prettyCode (JE c) = "je near $ +" + show c
prettyCode (JEL l) = "je" + l
prettyCode (JLTL l) = "jl" + l
prettyCode (LAB l) = "\n" ++ l ++ ":"
--prettyCode (PUSH x) = "push" + prettyRegister x
--prettyCode (POP x) = "pop" + prettyRegister x
prettyCode SYSCALL = "syscall"

public export
pretty : List Code -> String
pretty is = unlines (map prettyCode is)

header : String -> String
header name =
  "; asmsyntax=nasm\n" ++
  ";\n" ++
  "; To create an executable:\n" ++
  "; $ nasm -f elf64" + name ++ ".x86.asm\n" ++
  "; $ gcc -o" + name + "path/to/X86_64-infrastructure/driver.c" + name ++ ".x86.o"

moveParams : Nat -> String
moveParams 0 = ""
moveParams 1 =
  "mov" + prettyRegister 5 ++ "," + prettyRegister (arg 1)
moveParams 2 =
  "mov" + prettyRegister 7 ++ "," + prettyRegister (arg 2) ++
  "\n" ++
  moveParams 1
moveParams 3 =
  "mov" + prettyRegister 9 ++ "," + prettyRegister temp ++
  "\n" ++
  moveParams 2
moveParams 4 =
  "mov" + prettyRegister 11 ++ "," + prettyRegister (arg 4) ++
  "\n" ++
  moveParams 3
moveParams 5 =
  "mov" + prettyRegister 13 ++ "," + prettyRegister (arg 5) ++
  "\n" ++
  moveParams 4
moveParams _ = assert_total (idris_crash "too many arguments for main")

setup : Nat -> String
setup argNum =
  "segment .note.GNU-stack noalloc noexec nowrite progbits\n\n" ++
  "segment .text\n" ++
  "  global asm_main0, _asm_main0\n" ++
  "  global asm_main1, _asm_main1\n" ++
  "  global asm_main2, _asm_main2\n" ++
  "  global asm_main3, _asm_main3\n" ++
  "  global asm_main4, _asm_main4\n" ++
  "  global asm_main5, _asm_main5\n" ++
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
  "; setup\n" ++
  "; save registers\n" ++
  "push rbx\n" ++
  "push rbp\n" ++
  "push r12\n" ++
  "push r13\n" ++
  "push r14\n" ++
  "push r15\n" ++
  "\n" ++
  "; reserve space for register spills\n" ++
  "sub rsp," + show spillSpace ++
  "\n" ++
  "; initialize heap pointer\n" ++
  "mov" + prettyRegister heap ++ "," + prettyRegister (arg 0) ++
  "\n" ++
  "; initialize free pointer\n" ++
  "mov" + prettyRegister free ++ "," + prettyRegister heap ++
  "\n" ++
  "add" + prettyRegister free ++ "," + show (fieldOffset1 FieldsPerBlock) ++
  "\n" ++
  "; move parameters into place\n" ++
  moveParams argNum

cleanup : String
cleanup =
  "; cleanup\n" ++
  "cleanup:" ++
  "\n" ++
  "; free space for register spills\n" ++
  "add rsp," + show spillSpace ++
  "\n" ++
  "; restore registers\n" ++
  "pop r15\n" ++
  "pop r14\n" ++
  "pop r13\n" ++
  "pop r12\n" ++
  "pop rbp\n" ++
  "pop rbx\n" ++
  "ret"

public export
intoX86Routine : String -> String -> Nat -> String
intoX86Routine name prg argNum =
  header name ++
  "\n\n" ++
  setup argNum ++
  "\n\n" ++
  "; actual code" ++
  prg ++
  "\n" ++
  cleanup
