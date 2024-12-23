# AxCut

This directory contains the implementation of the AxCut compiler.

## Structure

The implementation is structured as follows

``` console
├── AARCH64-infrastructure      Minimal C driver for aarch64
├── examples                    Example Programs
├── source
│   ├── AARCH64                 aarch64 backend
│   ├── Main.idr
│   ├── Named.idr               Name-resoluted representation
│   ├── Namer.idr               Name resolution
│   ├── Parser.idr              Tree parser
│   ├── Reader.idr              S-Expression parser
│   ├── RISCV                   RISC-V backend
│   ├── Semantics.idr           Abstract machine semantics
│   ├── Syntax.idr              AxCut AST representation
│   ├── Tree.idr                General tree representation
│   ├── Typed.idr               Intrinsically-typed representation
│   ├── Typer.idr               Type checking
│   ├── Types.idr               AxCut types
│   └── X86_64                  x86-64 backend
└── X86_64-infrastructure       Minimal C driver for x86-64
```

Each backend is structured as follows

```console
├── Code.idr                    Basic definitions of instructions, registers and scaffolding
├── Coder.idr                   Code generation
├── Hardware.idr                Simulation of machine instructions
├── Limited.idr                 Representation with extern operations properly resolved
├── Limiter.idr                 Resolution of extern operations
└── Substitution.idr            Parallel-Moves algorithm
```

Extern operations are resolved separately for each backend because there can be different extern operations for each backend.

## Syntax

The parser understands an S-Expression representation, given by the grammar described here (also cf. the example programs).

Programs `P` consist of imports of extern operations, definitions of type signatures and definitions of top-level labels

```
P ::= (import m (targs) sig)
  | (signature T sig)
  | (define l (args) (targs) s)
```

Here `l` is a name for a label and `T` is a name for a type signature, where names are strings without spaces and parentheses.
`m` is the name of an external operation which can be one of the following (note that the only backend where all of them are implemented is the x86-64 backend, see the corresponding `Limited.idr` files)

```
m ::= restart
  | return
  | add
  | sub
  | mul
  | rem
  | ifz
  | ife
  | ifl
  | lit_n
  | mmap_anon_page
  | munmap_page
  | get_buffer_byte
  | set_buffer_byte
  | read_stdin
  | write_stdout
```

where `n` in `lit_n` is an integer.
See the example programs and the `Limited.idr` files for the correct `targs`s and `sig`s.

`args` is a list of names v (where ε denotes the empty string)

```
args :: = v args | ε
```

`targs` is a list of type judgments

```
targs ::= chi T targs | ε
```

where `chi` is the chirality which is one of

```
chi ::= ext | pro | con
```

Note that in the definition of top-level labels the `args` and `targs` have to match in length.

`sig` is a list of `targs` (one entry per constructor/destructor)

```
sig :: = (targs) sig | ε
```

Statements `s` consist of

```
s ::= (jump l)
  | (substitute (args) ((args) s))
  | (extern m (args) brs)
  | (let v T n (args)) s
  | (new v T (args) brs) s
  | (switch v brs)
  | (invoke v n)
```

In `substitute`, the length of the two `args`s has to match.
In `let` and `invoke`, the `n` is the position of the constructor/destructor in the signatur of `T`.
`brs` is a list of branches

```
brs ::= ((args) s) brs | ε
```

Finally, to save some parentheses and ease the representation a bit, there is a `do`-notation which desugars as follows

```
(do t l1) l2 == (l1 (t l2))
```

where `t` is a tree, which is either a name or a parenthesized list of trees `(l)`, and `l1` and `l2` are lists of trees.

## Building

To build the compiler, run

```
make build
```

This results in an executable `axcutc` in directory `build/exec`.

## Compiling Programs

To generate assembly code for a program, run the compiler with the path for this program as argument, without the file extension `.axcut`

```
build/exec/axcutc path/to/program
```

where the program is named `program.axcut`.
This generates files `program.BACKEND.asm` in the directory where the source program is located, where `BACKEND` is one of
- aarch64
- riscv
- x86

To create a binary from the x86-64 assembly code, assemble it with

```
nasm -f elf64 path/to/program.x86.asm
```

and link it with the minimal C driver by running

```
gcc -o program X86_64-infrastructure/driver.c path/to/program.x86.o
```

To create a binary from the aarch64 assembly code, assemble it with

```
as -o path/to/program.aarch64.o path/to/program.aarch64.asm
```

and link it with the minimal C driver by running

```
gcc -o program AARCH64-infrastructure/driver.c path/to/program.aarch64.o
```

In either case, the result is an executable `program`.
The entry point of this executable is the first top-level label in the program which can have up to 6 (x86-64) or 8 (aarch64) integer command-line arguments.

If the compiler is invoked without a command-line argument

```
build/exec/axcutc
```

it generates code for the example programs in directory `examples` for all backends (aarch64, RISC-V, x86-64) and simulates execution of the generated code.
The state of the registers at the end of this execution is written into the file `examples/example_name.output` where `example_name` is the name of the example program.

## Cleanup

To clean up build artifacts, run

```
make clean
```
