module RISCV.Hardware

import RISCV.Code

import Data.List
import Data.Vect

-- TODO use limited numbers (deal with overflow and underflow)
-- TODO in JAL(R) what happens when the PC becomes negative?


public export
data State : Type where
  Hardware : (code : List Code) -> (counter : Integer) -> (registers : Vect 32 Integer) -> (memory : List Integer) -> State


public export
get : Vect 32 Integer -> Fin 32 -> Integer
get registers FZ = 0
get registers register = index register registers

public export
set : Vect 32 Integer -> Fin 32 -> Integer -> Vect 32 Integer
set registers FZ value = registers
set registers register value = replaceAt register value registers

public export
fetchAt : List Code -> Nat -> Code
fetchAt [] counter = assert_total (idris_crash ("jumped beyond instructions " ++ show counter))
fetchAt (instruction :: instructions) Z = instruction
fetchAt (instruction :: instructions) (S (S (S (S counter)))) = fetchAt instructions counter
fetchAt (_ :: _) counter = assert_total (idris_crash ("jumped between instructions " ++ show counter))

public export
fetch : List Code -> Integer -> Code
fetch instructions counter = fetchAt instructions (integerToNat counter)

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
next (Hardware code counter registers memory) = case fetch code counter of
  ADD rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 + get registers rs2)) memory
  JALR rd rs1 imm =>
    Hardware code (get registers rs1 + imm) (set registers rd (4 + counter)) memory
  LW rd rs1 imm =>
    Hardware code (4 + counter) (set registers rd (load memory (get registers rs1 + imm))) memory
  SW rs1 rs2 imm =>
    Hardware code (4 + counter) registers (store memory (get registers rs2 + imm) (get registers rs1))
  ADDI rd rs1 imm =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 + imm)) memory
  JAL rd imm =>
    Hardware code (counter + imm) (set registers rd (4 + counter)) memory
  BEQ rs1 rs2 imm => if get registers rs1 == get registers rs2
    then Hardware code (counter + imm) registers memory
    else Hardware code (4 + counter) registers memory
  MUL rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (get registers rs1 * get registers rs2)) memory
  REM rd rs1 rs2 =>
    Hardware code (4 + counter) (set registers rd (if get registers rs2 == 0 then get registers rs1 else get registers rs1 `mod` get registers rs2)) memory
  BLT rs1 rs2 imm => if get registers rs1 < get registers rs2
    then Hardware code (counter + imm) registers memory
    else Hardware code (4 + counter) registers memory

public export
initial : List Code -> State
initial code = Hardware code 0 (0 :: 0 :: 64 :: 128 :: replicate 28 0) []

public export
run : State -> Vect 32 Integer
run m = case next m of
  Hardware code 0 registers memory => registers
  m => run m

public export
trace : State -> IO ()
trace m = case next m of
  Hardware code counter registers memory => if counter == 0
    then putStrLn (show registers)
    else do
      putStrLn (show counter ++ ": " ++ show registers)
      putStrLn (prettyCode (fetchAt code (integerToNat counter)))
      trace (Hardware code counter registers memory)

