module AARCH64.Hardware

import AARCH64.Code

import Data.List
import Data.Vect

-- TODO use limited numbers (deal with overflow and underflow)
-- TODO in B(R) what happens when the PC becomes negative?


public export
data State : Type where
  Hardware : (code : List Code) -> (counter : Integer) -> (registers : Vect 31 Integer) -> (memory : List Integer) -> (oflag : Bool) -> (sflag : Bool) -> (zflag : Bool) -> State


public export
get : Vect 31 Integer -> Fin 31 -> Integer
get registers register = index register registers

public export
set : Vect 31 Integer -> Fin 31 -> Integer -> Vect 31 Integer
set registers register value = replaceAt register value registers

public export
fetchAt : List Code -> Nat -> Code
fetchAt [] counter = assert_total (idris_crash ("jumped beyond instructions " ++ show counter))
fetchAt ((LAB l) :: instructions) counter = fetchAt instructions counter
fetchAt (instruction :: instructions) Z = instruction
fetchAt (instruction :: instructions) (S (S (S (S counter)))) = fetchAt instructions counter
fetchAt (_ :: _) counter = assert_total (idris_crash ("jumped between instructions " ++ show counter))

public export
fetch : List Code -> Integer -> Code
fetch instructions counter = fetchAt instructions (integerToNat counter)

getCounterAcc : List Code -> String -> Nat -> Nat
getCounterAcc [] label acc = assert_total (idris_crash ("label " ++ label ++ " not found"))
getCounterAcc ((LAB l) :: instructions) label acc =
  if l == label then acc
  else getCounterAcc instructions label acc
getCounterAcc (instruction :: instructions) label acc = getCounterAcc instructions label (acc + 4)

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
next : State -> State
next (Hardware code counter registers memory oflag sflag zflag) = case fetch code counter of
  ADD rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 + get registers rs2)) memory oflag sflag zflag
  ADDI rd rs1 imm =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 + imm)) memory oflag sflag zflag
  SUBI rd rs1 imm =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 - imm)) memory oflag sflag zflag
  MUL rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 * get registers rs2)) memory oflag sflag zflag
  SDIV rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 `div` get registers rs2)) memory oflag sflag zflag
  MSUB rd rs1 rs2 rs3 =>
    Hardware code (4 + counter) (set registers rd (get registers rs3 - get registers rs1 * get registers rs2)) memory oflag sflag zflag
  B l =>
    Hardware code (natToInteger (getCounter code l)) registers memory oflag sflag zflag
  BR r =>
    Hardware code (get registers r) registers memory oflag sflag zflag
  ADR rd l =>
    Hardware code (4 + counter) (set registers rd (natToInteger (getCounter code l))) memory oflag sflag zflag
  MOVR rd rs =>
    Hardware code (4 + counter) (set registers rd (get registers rs)) memory oflag sflag zflag
  MOVI rd imm =>
    Hardware code (4 + counter) (set registers rd imm) memory oflag sflag zflag
  LDR rd rs1 imm =>
    Hardware code (4 + counter) (set registers rd (load memory (get registers rs1 + imm))) memory oflag sflag zflag
  STR rs1 rs2 imm =>
    Hardware code (4 + counter) registers (store memory (get registers rs2 + imm) (get registers rs1)) oflag sflag zflag
  CMPR rs1 rs2 =>
    let comparee1 = get registers rs1
        comparee2 = get registers rs2
        result = comparee1 - comparee2
        comparee1Sign = comparee1 < 0
        signResult = result < 0
        overflowResult = comparee1Sign /= signResult
        zeroResult = result == 0
    in
    Hardware code (4 + counter) registers memory overflowResult signResult zeroResult
  CMPI rs imm =>
    let comparee = get registers rs
        result = comparee - imm
        compareeSign = comparee < 0
        signResult = result < 0
        overflowResult = compareeSign /= signResult
        zeroResult = result == 0
    in
    Hardware code (4 + counter) registers memory overflowResult signResult zeroResult
  BEQ l => if zflag
    then Hardware code (natToInteger (getCounter code l)) registers memory oflag sflag zflag
    else Hardware code (4 + counter) registers memory oflag sflag zflag
  BLT l => if oflag /= sflag
    then Hardware code (natToInteger (getCounter code l)) registers memory oflag sflag zflag
    else Hardware code (4 + counter) registers memory oflag sflag zflag
  LAB _ => assert_total (idris_crash "jumped between instructions")

public export
initial : List Code -> State
initial code = Hardware code 0 (64 :: 128 :: 0 :: replicate 28 0) [] False False False

public export
run : State -> Vect 31 Integer
run m = case next m of
  Hardware code 0 registers memory oflag sflag zflag => registers
  m => run m

public export
trace : State -> IO ()
trace m = case next m of
  Hardware code counter registers memory oflag sflag zflag => if counter == 0
    then putStrLn (show registers)
    else do
      putStrLn (show counter ++ ": " ++ show registers)
      putStrLn (prettyCode (fetchAt code (integerToNat counter)))
      trace (Hardware code counter registers memory oflag sflag zflag)

