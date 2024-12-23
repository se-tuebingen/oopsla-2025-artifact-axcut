module X86_64.Hardware

import X86_64.Code

import Data.Either
import Data.List
import Data.Vect

-- TODO
-- use limited numbers (deal with overflow and underflow)
-- add better simulation for syscalls: read/write are ignored; mmap/munmap only map single pages
-- length of instructions for conditional jumps are longer if jumps are not short but near (This is only an inaccuracy for the `lengthInstruction` function of this simulation. This is because everything except offsets into jump tables is resolved by labels, so that the result of `lengthInstruction` in this simulation is only important for the instructions in the jump tables, i.e. unconditional jumps to labels. Also, the assembler handles this properly anyway.)


oldRegLimit : Register
oldRegLimit = 7

RAX : Register
RAX = 4

RDX : Register
RDX = 5

heapEnd : Integer
heapEnd = 8192

heapStart : Integer
heapStart = fieldOffset1 FieldsPerBlock

freeStart : Integer
freeStart = heapStart + (fieldOffset1 FieldsPerBlock)

isSignedByte : Integer -> Bool
isSignedByte i = i <= 127 && i >= -128

isUnsignedDWord : Integer -> Bool
isUnsignedDWord i = i >= 0 && i <= 4294967295

isNegativeSignedDWord : Integer -> Bool
isNegativeSignedDWord i = i >= -2147483648 && i < 0

castBits8 : Cast a Bits8 => a -> Bits8
castBits8 = cast

intoByte : Integer -> Integer
intoByte n = cast (castBits8 n)

nextPage : List Bool -> Nat -> Nat
nextPage [] previous = S previous
nextPage (True :: ps) previous = nextPage ps (S previous)
nextPage (False :: ps) previous = S previous

freePageAt : List Bool -> Integer -> List Bool
freePageAt [] start = assert_total (idris_crash ("tried to free page at invalid address"))
freePageAt (p :: ps) start = if start == heapEnd
                               then False :: ps
                               else p :: freePageAt ps (start - pageSize)

public export
data Machine : Type where
  State : (code : List Code) -> (counter : Nat) -> (registers : Vect RegisterNum Integer) -> (memory : List Integer) -> (pages : List Bool) -> (spills : List Integer) -> (oflag : Bool) -> (sflag : Bool) -> (zflag : Bool) -> Machine


get : Vect RegisterNum Integer -> Register -> Either Integer Integer
get registers FZ = Right spillSpace
get registers register = Left (index register registers)

set : Vect RegisterNum Integer -> Register -> Integer -> Vect RegisterNum Integer
set registers FZ value = registers
set registers register value = replaceAt register value registers

lengthInstruction : Code -> Nat
lengthInstruction (ADD rd rs2) = 3
lengthInstruction (ADDM rd rs1 imm) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
--lengthInstruction (ADDMR rd imm rs2) =
--    let isSPor12 = if rd == 0 || rd == 12 then Z else S Z in
--    let isBPor13 = if rd == 7 || rd == 13 then Z else S Z in
--      if imm == 0 then 3 + isSPor12 + isBPor13
--      else if isSignedByte imm then 4 + isSPor12
--      else 7 + isSPor12
lengthInstruction (ADDI rd imm) =
    let isRAX = if rd == RAX then S Z else Z in
      if isSignedByte imm then 4
      else minus 7 isRAX
lengthInstruction (ADDIM rs1 imm1 imm2) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
    let lengthImm2 = if isSignedByte imm2 then S Z else 4 in
      if imm1 == 0 then 3 + lengthImm2 + isSPor12 + isBPor13
      else if isSignedByte imm1 then 4 + lengthImm2 + isSPor12
      else 7 + lengthImm2 + isSPor12
lengthInstruction (SUB rd rs2) = 3
lengthInstruction (SUBM rd rs1 imm) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
--lengthInstruction (SUBMR rd imm rs2) =
--    let isSPor12 = if rd == 0 || rd == 12 then Z else S Z in
--    let isBPor13 = if rd == 7 || rd == 13 then Z else S Z in
--      if imm == 0 then 3 + isSPor12 + isBPor13
--      else if isSignedByte imm then 4 + isSPor12
--      else 7 + isSPor12
--lengthInstruction (NEG rd) = 3
--lengthInstruction (NEGM rd imm) =
--    let isSPor12 = if rd == 0 || rd == 12 then Z else S Z in
--    let isBPor13 = if rd == 7 || rd == 13 then Z else S Z in
--      if imm == 0 then 3 + isSPor12 + isBPor13
--      else if isSignedByte imm then 4 + isSPor12
--      else 7 + isSPor12
lengthInstruction (IMUL rd rs2) = 4
lengthInstruction (IMULM rd rs1 imm) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 4 + isSPor12 + isBPor13
      else if isSignedByte imm then 5 + isSPor12
      else 8 + isSPor12
lengthInstruction (IDIV rs) = 3
lengthInstruction (IDIVM rs imm) =
    let isSPor12 = if rs == 0 || rs == 12 then Z else S Z in
    let isBPor13 = if rs == 7 || rs == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
lengthInstruction CQO = 2
lengthInstruction (JMP rs1) =
    let isNewReg = if rs1 <= oldRegLimit then Z else S Z in
      2 + isNewReg
--lengthInstruction (JMPI imm) = 5
lengthInstruction (JMPL l) = 2
lengthInstruction (JMPLN l) = 5
lengthInstruction (LEAL rd l) = 7
lengthInstruction (MOV rd rs1) = 3
lengthInstruction (MOVS rs1 rs2 imm) =
    let isSPor12 = if rs2 == 0 || rs2 == 12 then Z else S Z in
    let isBPor13 = if rs2 == 7 || rs2 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
lengthInstruction (MOVRB rs1 rs2 imm) =
    let isABCD = if rs1 >= 1 && rs2 <= 4 then Z else S Z in
    let isSPor12 = if rs2 == 0 || rs2 == 12 then Z else S Z in
    let isBPor13 = if rs2 == 7 || rs2 == 13 then Z else S Z in
      if imm == 0 then 2 + isABCD + isSPor12 + isBPor13
      else if isSignedByte imm then 3 + isABCD + isSPor12
      else 6 + isABCD + isSPor12
lengthInstruction (MOVL rd rs1 imm) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
lengthInstruction (MOVZX rd rs1 imm) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 4 + isSPor12 + isBPor13
      else if isSignedByte imm then 5 + isSPor12
      else 8 + isSPor12
lengthInstruction (MOVI rd imm) =
    let isNewReg = if rd <= oldRegLimit then Z else S Z in
      if isUnsignedDWord imm then 5 + isNewReg
      else if isNegativeSignedDWord imm then 7
      else 10
lengthInstruction (MOVIM rs1 imm1 imm2) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm1 == 0 then 7 + isSPor12 + isBPor13
      else if isSignedByte imm1 then 8 + isSPor12
      else 11 + isSPor12
lengthInstruction (CMP rs1 rs2) = 3
lengthInstruction (CMPRM rs1 rs2 imm) =
    let isSPor12 = if rs2 == 0 || rs2 == 12 then Z else S Z in
    let isBPor13 = if rs2 == 7 || rs2 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
lengthInstruction (CMPMR rs1 imm rs2) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
      if imm == 0 then 3 + isSPor12 + isBPor13
      else if isSignedByte imm then 4 + isSPor12
      else 7 + isSPor12
lengthInstruction (CMPI rs1 imm) =
    let isRAX = if rs1 == RAX then S Z else Z in
      if isSignedByte imm then 4
      else minus 7 isRAX
lengthInstruction (CMPIM rs1 imm1 imm2) =
    let isSPor12 = if rs1 == 0 || rs1 == 12 then Z else S Z in
    let isBPor13 = if rs1 == 7 || rs1 == 13 then Z else S Z in
    let lengthImm2 = if isSignedByte imm2 then S Z else 4 in
      if imm1 == 0 then 3 + lengthImm2 + isSPor12 + isBPor13
      else if isSignedByte imm1 then 4 + lengthImm2 + isSPor12
      else 7 + lengthImm2 + isSPor12
--lengthInstruction (JE imm) = 6
lengthInstruction (JEL l) = 2
lengthInstruction (JLTL l) = 2
lengthInstruction (LAB l) = 0
--lengthInstruction (PUSH rs1) =
--    let isNewReg = if rs1 <= oldRegLimit then Z else S Z in
--      1 + isNewReg
--lengthInstruction (POP rd1) =
--    let isNewReg = if rd1 <= oldRegLimit then Z else S Z in
--      1 + isNewReg
lengthInstruction SYSCALL = 2

fetchAt : List Code -> Nat -> Code
fetchAt [] counter = assert_total (idris_crash ("jumped beyond instructions " ++ show counter))
fetchAt ((LAB l) :: instructions) counter = fetchAt instructions counter
fetchAt (instruction :: instructions) Z = instruction
fetchAt (instruction :: instructions) counter with (counter >= (lengthInstruction instruction))
  fetchAt (instruction :: instructions) counter | True = fetchAt instructions (minus counter (lengthInstruction instruction))
  fetchAt (instruction :: instructions) counter | False = assert_total (idris_crash ("jumped between instructions " ++ show counter))

fetch : List Code -> Nat -> Code
fetch instructions counter = fetchAt instructions counter

getCounterAcc : List Code -> String -> Nat -> Nat
getCounterAcc [] label acc = assert_total (idris_crash ("label " ++ label ++ " not found"))
getCounterAcc ((LAB l) :: instructions) label acc =
  if l == label then acc
  else getCounterAcc instructions label acc
getCounterAcc (instruction :: instructions) label acc = getCounterAcc instructions label (acc + (lengthInstruction instruction))

getCounter : List Code -> String -> Nat
getCounter code "cleanup" = getCounter code "lab0"
getCounter code label = getCounterAcc code label 0

loadAt : List Integer -> Nat -> Integer
loadAt [] address = 0
loadAt (content :: memory) Z = content
loadAt (content :: memory) (S address) = loadAt memory address

load : List Integer -> Integer -> Integer
load memory address = loadAt memory (integerToNat address)

storeAt : List Integer -> Nat -> Integer -> List Integer
storeAt [] 0 value = value :: []
storeAt [] (S address) value = 0 :: storeAt [] address value
storeAt (_ :: memory) 0 value = value :: memory
storeAt (content :: memory) (S address) value = content :: storeAt memory address value

store : List Integer -> Integer -> Integer -> List Integer
store memory address value = storeAt memory (integerToNat address) value


public export
step : Machine -> Machine
step (State code counter registers memory pages spills oflag sflag zflag) =
  let ins = fetch code counter in
  let len = (lengthInstruction ins) in
  case ins of
  ADD rd rs2 =>
    State code (len + counter) (set registers rd (fromEither (get registers rd) + fromEither (get registers rs2))) memory pages spills oflag sflag zflag
  ADDM rd rs2 imm =>
    case get registers rs2 of
      Left address => State code (len + counter) (set registers rd (fromEither (get registers rd) + (load memory (address + imm)))) memory pages spills oflag sflag zflag
      Right stack => State code (len + counter) (set registers rd (fromEither (get registers rd) + (load spills (stack - imm)))) memory pages spills oflag sflag zflag
  --ADDMR rd imm rs2 =>
  --  case get registers rd of
  --    Left address => State code (len + counter) registers (store memory (address + imm) ((load memory (address + imm)) + (fromEither (get registers rs2)))) pages spills oflag sflag zflag
  --    Right stack => State code (len + counter) registers memory pages (store spills (stack - imm) ((load spills (stack - imm)) + (fromEither (get registers rs2)))) oflag sflag zflag
  ADDI rd imm =>
    State code (len + counter) (set registers rd (fromEither (get registers rd) + imm)) memory pages spills oflag sflag zflag
  ADDIM rs1 imm1 imm2 =>
    case get registers rs1 of
      Left address => State code (len + counter) registers (store memory (address + imm1) ((load memory (address + imm1)) + imm2)) pages spills oflag sflag zflag
      Right stack => State code (len + counter) registers memory pages (store spills (stack - imm1) ((load spills (stack - imm1)) + imm2)) oflag sflag zflag
  SUB rd rs2 =>
    State code (len + counter) (set registers rd (fromEither (get registers rd) - fromEither (get registers rs2))) memory pages spills oflag sflag zflag
  SUBM rd rs2 imm =>
    case get registers rs2 of
      Left address => State code (len + counter) (set registers rd (fromEither (get registers rd) - (load memory (address + imm)))) memory pages spills oflag sflag zflag
      Right stack => State code (len + counter) (set registers rd (fromEither (get registers rd) - (load spills (stack - imm)))) memory pages spills oflag sflag zflag
  --SUBMR rd imm rs2 =>
  --  case get registers rd of
  --    Left address => State code (len + counter) registers (store memory (address + imm) ((load memory (address + imm)) - (fromEither (get registers rs2)))) pages spills oflag sflag zflag
  --    Right stack => State code (len + counter) registers memory pages (store spills (stack - imm) ((load spills (stack - imm)) - (fromEither (get registers rs2)))) oflag sflag zflag
  --NEG rd =>
  --  State code (len + counter) (set registers rd (-(fromEither (get registers rd)))) memory pages spills oflag sflag zflag
  --NEGM rd imm =>
  --  case get registers rd of
  --    Left address => State code (len + counter) registers (store memory (address + imm) (-(load memory (address + imm)))) pages spills oflag sflag zflag
  --    Right stack => State code (len + counter) registers memory pages (store spills (stack - imm) (-(load spills (stack - imm)))) oflag sflag zflag
  IMUL rd rs2 =>
    State code (len + counter) (set registers rd (fromEither (get registers rd) * fromEither (get registers rs2))) memory pages spills oflag sflag zflag
  IMULM rd rs2 imm =>
    case get registers rs2 of
      Left address => State code (len + counter) (set registers rd (fromEither (get registers rd) * (load memory (address + imm)))) memory pages spills oflag sflag zflag
      Right stack => State code (len + counter) (set registers rd (fromEither (get registers rd) * (load spills (stack - imm)))) memory pages spills oflag sflag zflag
  IDIV rs =>
    let divisor = fromEither (get registers rs)
        dividend = fromEither (get registers RAX)
        signDividend = dividend `div` abs dividend
    in
      if divisor == 0
      then assert_total (idris_crash ("SIGFPE: division by 0"))
      else State code (len + counter) (set (set registers RAX (dividend `div` divisor)) RDX (signDividend * ((abs dividend) `mod` divisor))) memory pages spills oflag sflag zflag
  IDIVM rs imm =>
    let divisor = case get registers rs of
                    Left address => load memory (address + imm)
                    Right stack => load spills (stack - imm)
        dividend = fromEither (get registers RAX)
        signDividend = dividend `div` abs dividend
    in
      if divisor == 0
      then assert_total (idris_crash ("SIGFPE: division by 0"))
      else State code (len + counter) (set (set registers RAX (dividend `div` divisor)) RDX (signDividend * ((abs dividend) `mod` divisor))) memory pages spills oflag sflag zflag
  CQO =>
    State code (len + counter) (set registers RDX (if (fromEither (get registers RAX)) >= 0 then -1 else 0)) memory pages spills oflag sflag zflag
  JMP rs1 =>
    State code (integerToNat (fromEither (get registers rs1))) registers memory pages spills oflag sflag zflag
  --JMPI imm =>
  --  State code (len + counter + imm) registers memory pages spills oflag sflag zflag
  JMPL l =>
    State code (getCounter code l) registers memory pages spills oflag sflag zflag
  JMPLN l =>
    State code (getCounter code l) registers memory pages spills oflag sflag zflag
  LEAL rd l =>
    State code (len + counter) (set registers rd (natToInteger (getCounter code l))) memory pages spills oflag sflag zflag
  MOV rd rs1 =>
    State code (len + counter) (set registers rd (fromEither (get registers rs1))) memory pages spills oflag sflag zflag
  MOVS rs1 rs2 imm =>
    case get registers rs2 of
      Left address => State code (len + counter) registers (store memory (address + imm) (fromEither (get registers rs1))) pages spills oflag sflag zflag
      Right stack => State code (len + counter) registers memory pages (store spills (stack - imm) (fromEither (get registers rs1))) oflag sflag zflag
  MOVRB rs1 rs2 imm =>
    case get registers rs2 of
      Left address => State code (len + counter) registers (store memory (address + imm) (intoByte (fromEither (get registers rs1)))) pages spills oflag sflag zflag
      Right stack => State code (len + counter) registers memory pages (store spills (stack - imm) (intoByte (fromEither (get registers rs1)))) oflag sflag zflag
  MOVL rd rs1 imm =>
    case get registers rs1 of
      Left address => State code (len + counter) (set registers rd (load memory (address + imm))) memory pages spills oflag sflag zflag
      Right stack => State code (len + counter) (set registers rd (load spills (stack - imm))) memory pages spills oflag sflag zflag
  MOVZX rd rs1 imm =>
    case get registers rs1 of
      Left address => State code (len + counter) (set registers rd (intoByte (load memory (address + imm)))) memory pages spills oflag sflag zflag
      Right stack => State code (len + counter) (set registers rd (intoByte (load spills (stack - imm)))) memory pages spills oflag sflag zflag
  MOVI rd imm =>
    State code (len + counter) (set registers rd imm) memory pages spills oflag sflag zflag
  MOVIM rd imm1 imm2 =>
    case get registers rd of
      Left address => State code (len + counter) registers (store memory (address + imm1) imm2) pages spills oflag sflag zflag
      Right stack => State code (len + counter) registers memory pages (store spills (stack - imm1) imm2) oflag sflag zflag
  CMP rs1 rs2 =>
    let comparee1 = fromEither (get registers rs1)
        comparee2 = fromEither (get registers rs2)
        result = comparee1 - comparee2
        comparee1Sign = comparee1 < 0
        signResult = result < 0
        overflowResult = comparee1Sign /= signResult
        zeroResult = result == 0
    in
    State code (len + counter) registers memory pages spills overflowResult signResult zeroResult
  CMPRM rs1 rs2 imm =>
    let comparee1 = fromEither (get registers rs1)
        comparee2 = case get registers rs2 of
                      Left address => load memory (address + imm)
                      Right stack => load spills (stack - imm)
        result = comparee1 - comparee2
        comparee1Sign = comparee1 < 0
        signResult = result < 0
        overflowResult = comparee1Sign /= signResult
        zeroResult = result == 0
    in
    State code (len + counter) registers memory pages spills overflowResult signResult zeroResult
  CMPMR rs1 imm rs2 =>
    let comparee1 = case get registers rs1 of
                      Left address => load memory (address + imm)
                      Right stack => load spills (stack - imm)
        comparee2 = fromEither (get registers rs2)
        result = comparee1 - comparee2
        comparee1Sign = comparee1 < 0
        signResult = result < 0
        overflowResult = comparee1Sign /= signResult
        zeroResult = result == 0
    in
    State code (len + counter) registers memory pages spills overflowResult signResult zeroResult
  CMPI rs1 imm =>
    let comparee = fromEither (get registers rs1)
        result = comparee - imm
        compareeSign = comparee < 0
        signResult = result < 0
        overflowResult = compareeSign /= signResult
        zeroResult = result == 0
    in
      State code (len + counter) registers memory pages spills overflowResult signResult zeroResult
  CMPIM rs1 imm1 imm2 =>
    let comparee = case get registers rs1 of
                     Left address => load memory (address + imm1)
                     Right stack => load spills (stack - imm1)
        result = comparee - imm2
        compareeSign = comparee < 0
        signResult = result < 0
        overflowResult = compareeSign /= signResult
        zeroResult = result == 0
    in
      State code (len + counter) registers memory pages spills overflowResult signResult zeroResult
  --JE imm => if zflag
  --  then State code (len + counter + imm) registers memory pages spills oflag sflag zflag
  --  else State code (len + counter) registers memory pages spills oflag sflag zflag
  JEL l => if zflag
    then State code (getCounter code l) registers memory pages spills oflag sflag zflag
    else State code (len + counter) registers memory pages spills oflag sflag zflag
  JLTL l => if oflag /= sflag
    then State code (getCounter code l) registers memory pages spills oflag sflag zflag
    else State code (len + counter) registers memory pages spills oflag sflag zflag
  LAB l =>
    assert_total (idris_crash "jumped between instructions")
  --PUSH rs1 =>
  --  State code (len + counter) registers memory pages spills ((fromEither (get registers rs1)) :: stack) oflag sflag zflag
  --POP rd1 =>
  --  case fromList stack of
  --    Just stack => State code (len + counter) (set registers rd1 stack.head) memory pages spills stack.tail oflag sflag zflag
  --    Nothing => assert_total (idris_crash "tried to pop empty stack")
  SYSCALL =>
    let syscall = fromEither (get registers sysNR) in
    case syscall of
      0 => State code (len + counter) registers memory pages spills oflag sflag zflag
      1 => State code (len + counter) registers memory pages spills oflag sflag zflag
      9 => let pageNum = nextPage pages 0 in
             State code (len + counter) (set registers return1 ((((natToInteger pageNum) - 1) * pageSize) + heapEnd)) memory ((take (minus pageNum 1) pages) ++ True :: (drop (S pageNum) pages)) spills oflag sflag zflag
      11 => State code (len + counter) registers memory (freePageAt pages (fromEither (get registers (arg 0)))) spills oflag sflag zflag
      _ => assert_total (idris_crash "unknown syscall")

public export
initial : List Code -> Machine
initial code = State code 0 (0 :: 0 :: heapStart :: freeStart :: replicate (minus RegisterNum reserved) 0) [] [] [] False False False

public export
run : Machine -> Vect RegisterNum Integer
run m = case step m of
  State code 0 registers memory pages spills oflag sflag zflag => registers
  m => run m

public export
trace : Machine -> IO ()
trace m = case step m of
  State code counter registers memory pages spills oflag sflag zflag => if counter == 0
    then putStrLn (show registers)
    else do
      putStrLn (show counter ++ ": " ++ show registers)
      putStrLn (prettyCode (fetchAt code counter))
      trace (State code counter registers memory pages spills oflag sflag zflag)
