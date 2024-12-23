module Main

import Types
import Reader
import Tree
import Parser
import Syntax
import Namer
import Named
import Typer
import Typed
import AARCH64.Limiter
import AARCH64.Limited
import AARCH64.Coder
import AARCH64.Code
import AARCH64.Hardware
import RISCV.Limiter
import RISCV.Limited
import RISCV.Coder
import RISCV.Code
import RISCV.Hardware
import X86_64.Limiter
import X86_64.Limited
import X86_64.Coder
import X86_64.Code
import X86_64.Hardware

import Data.Fin
import Data.List
import Data.Vect
import System
import System.File


compileAARCH64 : String -> Either String ((List AARCH64.Code.Code), Nat)
compileAARCH64 source = do
  trees <- read source
  parsed <- parse trees
  (_ ** _ ** named) <- name parsed
  (_ ** _ ** typed) <- check named
  limited <- limit typed
  pure (compile limited)

compileRISCV : String -> Either String (List RISCV.Code.Code)
compileRISCV source = do
  trees <- read source
  parsed <- parse trees
  (_ ** _ ** named) <- name parsed
  (_ ** _ ** typed) <- check named
  limited <- limit typed
  pure (compile limited)

compileX86_64 : String -> Either String ((List X86_64.Code.Code), Nat)
compileX86_64 source = do
  trees <- read source
  parsed <- parse trees
  (_ ** _ ** named) <- name parsed
  (_ ** _ ** typed) <- check named
  limited <- limit typed
  pure (compile limited)


codeGenAARCH64 : String -> IO ()
codeGenAARCH64 name = do
  let sourcePath = name ++ ".axcut"
  let assemblyPath = name ++ ".aarch64.asm"
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right (target, argNum) <- pure (compileAARCH64 source) | Left error => putStrLn ("error compiling: " ++ error)
  Right _ <- writeFile assemblyPath (intoAARCH64Routine name (pretty target) argNum) | Left _ => putStrLn ("error writing file: " ++ assemblyPath)
  putStrLn (assemblyPath ++ " generated")

codeGenRISCV : String -> IO ()
codeGenRISCV name = do
  let sourcePath = name ++ ".axcut"
  let assemblyPath = name ++ ".riscv.asm"
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right target <- pure (compileRISCV source) | Left error => putStrLn ("error compiling: " ++ error)
  Right _ <- writeFile assemblyPath (pretty target) | Left _ => putStrLn ("error writing file: " ++ assemblyPath)
  putStrLn (assemblyPath ++ " generated")

codeGenX86_64 : String -> IO ()
codeGenX86_64 name = do
  let sourcePath = name ++ ".axcut"
  let assemblyPath = name ++ ".x86.asm"
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right (target, argNum) <- pure (compileX86_64 source) | Left error => putStrLn ("error compiling X86: " ++ error)
  Right _ <- writeFile assemblyPath (intoX86Routine name (pretty target) argNum) | Left _ => putStrLn ("error writing file: " ++ assemblyPath)
  putStrLn (assemblyPath ++ " generated")


testCaseAARCH64 : String -> String -> IO ()
testCaseAARCH64 path name = do
  let sourcePath = path ++ name ++ ".axcut"
  let outputPath = path ++ name ++ ".aarch64.output"
  putStr ("Testing " ++ name ++ " for aarch64 ... ")
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right (target, argNum) <- pure (compileAARCH64 source) | Left error => putStrLn ("error compiling: " ++ error)
  Right _ <- writeFile outputPath (show (run (initial target))) | Left _ => putStrLn ("error writing file: " ++ outputPath)
  putStrLn "successful!"

testCaseRISCV : String -> String -> IO ()
testCaseRISCV path name = do
  let sourcePath = path ++ name ++ ".axcut"
  let outputPath = path ++ name ++ ".riscv.output"
  putStr ("Testing " ++ name ++ " for RISC-V ... ")
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right target <- pure (compileRISCV source) | Left error => putStrLn ("error compiling: " ++ error)
  Right _ <- writeFile outputPath (show (run (initial target))) | Left _ => putStrLn ("error writing file: " ++ outputPath)
  putStrLn "successful!"

testCaseX86_64 : String -> String -> IO ()
testCaseX86_64 path name = do
  let sourcePath = path ++ name ++ ".axcut"
  let outputPath = path ++ name ++ ".x86.output"
  putStr ("Testing " ++ name ++ " for x86-64 ... ")
  Right source <- readFile sourcePath | Left _ => putStrLn ("error reading file: " ++ sourcePath)
  Right (target, argNum) <- pure (compileX86_64 source) | Left error => putStrLn ("error compiling X86: " ++ error)
  Right _ <- writeFile outputPath (show (run (initial target))) | Left _ => putStrLn ("error writing file: " ++ outputPath)
  putStrLn "successful!"

testExampleAARCH64 : String -> IO ()
testExampleAARCH64 name = testCaseAARCH64 "./examples/" name

testExampleRISCV : String -> IO ()
testExampleRISCV name = testCaseRISCV "./examples/" name

testExampleX86_64 : String -> IO ()
testExampleX86_64 name = testCaseX86_64 "./examples/" name

testExample : String -> IO ()
testExample name = do
  testExampleAARCH64 name
  testExampleRISCV name
  testExampleX86_64 name

test : IO ()
test = do
  testExampleX86_64 "buffer"
  putStrLn ""
  testExample "closure"
  putStrLn ""
  testExample "count"
  putStrLn ""
  testExample "either"
  putStrLn ""
  testExample "erase_unused"
  putStrLn ""
  testExample "factorial_accumulator"
  putStrLn ""
  testExample "fibonacci_recursive"
  putStrLn ""
  testExample "iterate_increment"
  putStrLn ""
  testExample "lookup_tree"
  putStrLn ""
  testExample "match_options"
  putStrLn ""
  testExample "mult_list"
  putStrLn ""
  testExample "objects"
  putStrLn ""
  testExample "pair"
  putStrLn ""
  testExample "sum_range"
  putStrLn ""
  testExample "with"

main : IO ()
main = do
  args <- getArgs
  let mFileName = do
    argsWithoutSelfName <- tail' args
    head' argsWithoutSelfName
  case mFileName of
    Just fileName => do 
      codeGenAARCH64 fileName
      codeGenRISCV fileName
      codeGenX86_64 fileName
    Nothing => test
