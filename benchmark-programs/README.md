# Benchmark Programs

This directory contains the code for the benchmark programs and the raw results.
There is a short description for each benchmark in the [`descriptions` directory](./descriptions).
The results are in the directory [`results`](./results) and the source code for each language is in the directory with the corresponding name.
For AxCut we have included the source code together with the assembly code for x86-64 that we have generated.

The benchmark programs can be tested by running

```
make test
```

given that all languages as well as `hyperfine` are installed.
For AxCut, `gcc` and `nasm` are required.

The benchmarks can be run with

```
make bench
```

It is also possible to test and run the benchmarks for a single language by appending a suffix with `_LANG` where `LANG` is the name of the language, e.g.

```
make bench_axcut
```

To clean up, run

```
make clean
```
