## Reusability

The [STEP-BY-STEP.md](./STEP-BY-STEP.md) guide explains how to run and examine the artifact and where to find what.
Here, we give some notes on reusability.

### Writing more programs and extending benchmarks

It is of course possible to adapt the example programs given in the [compiler](./axcut) or write new programs and compile them.
The compiler parses a simple S-Expression representation which is described in the [AxCut README](./axcut/README.md).
In particular, one can write more benchmark programs and add them in the corresponding [benchmark directory](./benchmark-programs/axcut).
(The programs must of course also be added for the other languages in their directories.)
To make the extended benchmark suite run with the [`Makefile`](./benchmark-programs/Makefile), extend the `PROGRAM` variable and add a new section there, following the structure for the existing programs.
It is also possible to add a new language by adding programs for it, extending the `LANGS` variable in the [`Makefile`](./benchmark-programs/Makefile) and adding a target there, following the targets for the other languages.

### Exploring and adapting the AxCut compiler

The [AxCut README](./axcut/README.md) describes the structure of the compiler, explaining what components exist where.
It moreover, describes the syntax the parser understands.
Unfortunately, the code of the compiler itself is poorly documented which probably makes it somewhat difficult to understand.
Hence, adapting the compiler for reuse likely requires substantial effort.

### Running on aarch64

The artifact will not work out of the box on arm-based architechtures.
The Dockerfile might be made to work by changing `amd64` to `arm64` in the `Dockerfile` (this was not tested properly).
The compilation of AxCut programs has to be adapted as well (also in the [benchmark `Makefile`](./benchmark-programs/Makefile)).
Instead of assembling with `nasm`, one has to use an appropriate assembler such as GNU `as`:

```
as -o examples/example_name.aarch64.o examples/example_name.aarch64.asm
```

Moreover, for the benchmarks the aarch64 assembly code has to be used of course (after generating it with the compiler as described in [STEP-BY-STEP.md](./STEP-BY-STEP.md)), replacing the one for x86-64 in the [directory for axcut programs](./benchmark-programs/axcut).
However, the aarch64 backend currently only allows 16-bit literals, so the `factorial_accumulator` program in AxCut will not directly work, unless an assembler is used which desugars `MOV` instructions appropriately.
As a workaround, one can dissect the large magic number in the benchmark program `examples/factorial_accumulator.axcut` into `1000000007 = 1000 * 1000 * 1000 + 7` (in the correct AxCut syntax of course).
