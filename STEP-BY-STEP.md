## Step-By-Step

The [GETTING-STARTED.md](./GETTING-STARTED.md) guide explains how to set up the artifact.
Here, we describe in more detail, how the artifact supports the claims.

### Running the benchmarks
To conduct the benchmarks (described in Section 5.2), run

```
docker exec -it axcut-container bash -c "cd benchmark-programs && make bench && make show"
```

This will take roughly 90 minutes (again, depending on your computer power).
To obatin a shorter running time, the [benchmarks `Makefile`](./benchmark-programs/Makefile) can be adapted, for example, by removing all runs except the ones for the largest input for each benchmark which will cut down the running time to about 15-20 minutes.
For even shorter running times one can of course use smaller inputs.

In the end, the above command will print a somewhat prettified version of the results for the last input in Markdown-style tables, with one table per language, containing the mean, standard deviation and median in seconds and a final table containing the mean times for all languages.
The tables are available inside the container in the file `/home/sudouser/benchmark-programs/results-last.md` and can also be copied out of the container with

```
docker cp axcut-container:/home/sudouser/benchmark-programs/results-last.md /path/to/destination
```

The raw results are also available inside the container in the files `/home/sudouser/benchmark-programs/LANG/PROG.csv` where `LANG` is one of
- `axcut`
- `mlton`
- `ocaml`
- `koka`
- `koka_opt`
- `rust`
- `rust_opt`
and `PROG` is one of
- `factorial_accumulator`
- `fibonacci_recursive`
- `sum_range`
- `iterate_increment`
- `match_options`
- `lookup_tree`
- `erase_unused`
For comparison, the raw results from the paper are contained in the `benchmark-programs/results` directory.
While the numbers of the benchmarks can vary due the virtualization overhead of Docker, the relative results shown in Table 1 in the paper should still be mostly visible.

We suggest that the reviewers:
1. Familiarize themselves with the benchmark programs by reading the [descriptions](./benchmark-programs/descriptions).
2. Run the benchmarks.
3. Compare the mean-time results of the benchmarks with the numbers given in the paper, verifying that the relative results are mostly reproducible despite the virtualization via Docker.
   The results from the paper are included in this repository in the file [`results-paper.md`](results-paper.md) as a table with the mean and standard deviation in milliseconds in one column per language.

### Testing the compiler implementation
To test the implementation of the compiler for AxCut (described in Section 4), enter the container (as described in the [GETTING-STARTED.md](./GETTING-STARTED.md) guide) and enter the corresponding directory

```
cd axcut
```

The compiler can be built by running

```
make build
```

resulting in an executable `axcutc` in directory `build/exec`.
Running this executable without arguments (as done by `make test`)

```
build/exec/axcutc
```

generates code for the example programs in directory `examples` for all backends (aarch64, RISC-V, x86-64) and simulates execution of the generated code.
The state of the registers at the end of this simulation is written into the file `examples/example_name.output` where `example_name` is the name of the example program.
The `examples` directory in particular contains all benchmark programs for AxCut in `examples/PROG.axcut`, where `PROG` is as in the above section on benchmarks.
The container has vim installed as an editor to inspect and edit files, but it runs Ubuntu and other editors can be installed in the usual way for Ubuntu (but note that no graphical environment is installed).

To write the assembly code for a program into files, run the compiler with the path for this program as argument, without the file extension `.axcut`, e.g.,

```
build/exec/axcutc examples/sum_range
```

This generates files `sum_range.BACKEND.asm`, where `BACKEND` is one of
- aarch64
- riscv
- x86

To create a binary from the x86-64 assembly code, assemble it with

```
nasm -f elf64 examples/sum_range.x86.asm
```

and link it with the minimal C driver by running

```
gcc -o sum_range X86_64-infrastructure/driver.c examples/sum_range.x86.o
```

The result is an executable `sum_range` which can be executed with (this is one of the benchmark programs which all take one command-line argument; the other examples do not)

```
./sum_range 10
```

This should print `45`.
We suggest to try this with some of the examples (for `buffer`, `cat_push_pull` and `stdrw` code generation only works for x86-64 and the errors for the other backends can be ignored).
The expected results are in `examples/example_name.expected` (the benchmark programs should be called with argument `10`), except for `stdrw` which expects up to five characters input from stdin and `cat_push_pull` which expect input from stdin and an EOF (CTRL+d) at the end.

We further suggest to generate code for the benchmark programs and confirm that the resulting code for x86-64 is the same as the one inlcuded in the benchmarks directory, for example, by diffing

```
diff examples/PROG.x86.asm ..benchmark-programs/axcut/PROG.x86.asm
```

### Confirming the correctness of theorems
The correctness of the typeability preservation of our normalization procedure (described in Section 2.2) and the type safety of AxCut with respect to the abstract machine semantics (stated in Section 3.3) are proven by their intrinsically typed implementations in Idris 2 and totality of the corresponding function.

For the normalization, enter

```
cd normalization
```

and build the implementation with `make build`.
It can be inspected in

```
vim AxCutNormalForm.idr
```

We suggest to confirm that the `transformCommand` function which implements the transformation is indeed annotated with `total`.

Similarly, for the AxCut operational semantics, enter

```
cd axcut
```

and run `make build`.
This builds the whole compiler (as described in the compiler section above) which includes the machine semantics in file `source/Semantics.idr`.
We suggest to confirm that the `step` function which implements the abstract machine is indeed annotated with `total`.
Note that this implementation of the abstract machine only supports the externs `lit`, `add` and `ifz` and simply loops in all other cases.
