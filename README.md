# Artifact of the paper 'Compiling Classical Sequent Calculus to Stock Hardware: The Duality of Compilation'

> **Availability**: Our artifact is available via Zenodo

[![DOI](https://zenodo.org/badge/?????????.svg)](https://zenodo.org/badge/latestdoi/?????????)

This GitHub repository constitutes the artifact of our paper

> Compiling Classical Sequent Calculus to Stock Hardware: The Duality of Compilation.
> Philipp Schuster, Marius Müller, Klaus Ostermann, Jonathan Immanuel Brachthäuser.
> Conditionally Accepted at OOPSLA 2025.

## Overview

The artifact consists of
- an intrinsically-typed [implementation in Idris 2 of the AxCut language](./axcut) and the abstract machine semantics, along with a simple parser, a type checker and code generation for aarch64, RISC-V and x86-64.
- an intrinsically-typed [implementation in Idris 2 of the normalization procedure](./normalization) transforming standard linear sequent calculus terms into AxCut.
- the benchmarks conducted for the evaluation of the compilation approach.
  This repository contains the sources of the [benchmark programs](./benchmark-programs) for all languages we have benchmarked against.
  There are [descriptions](./benchmark-programs/descriptions) of what each benchmark program does.
  For comparison, there are also two files containing results:
  - [`results-paper.md`](./results-paper.md) contains the results for the largest input from the paper.
  - [`results-example.md`](./results-example.md) contains the results for the largest input from a run of the benchmarks in the below Docker container on an Intel(R) Core(TM) i5-8265U.

This repository contains a [`Dockerfile`](./Dockerfile) which can be used to build a Docker image for a container with all necessary languages installed.
Everything can be run inside this container.

## Paper Claims

- We claim that the normalization procedure in Section 2.2 is typeability preserving, which is supported by the intrinsically-typed implementation provided here.
  The collapsing of the polarity and hence the trivial renaming to the unified syntax is not included.
  The implementation further shows that this normalization can be implemented efficiently in a single pass which is compositional in the sense that recursive calls are only done on subterms, and in fact only once for each subterm. 
  It does not show how explicit substitutions can be inserted when starting from a non-linear term in standard sequent calculus.
- We claim that AxCut with the operational semantics in terms of the abstract machine given in Section 3.3 satisfies type safety, which is supported by the intrinsically-typed implementation provided here.
- Our implementation further shows that compilation of AxCut programs to machine code indeed leads to executable programs.
- We claim that our compilation of sequent calculus to machine code is comparable to existing compilers when it comes to performance which is supported by the provided benchmarks.
  The goal of these benchmarks is not to show raw performance, but only that our approach is viable.

## Hardware Dependencies

The artifact has been tested on x86-64 with Linux.
See [`REUSABILITY.md`](./REUSABILITY.md) for some notes on aarch64.

## Additional Instructions

Please find additional instructions in the following two files:

- [`GETTING-STARTED.md`](./GETTING-STARTED.md) for the kick-the-tires phase.
- [`STEP-BY-STEP.md`](./STEP-BY-STEP.md) for suggestions on how to proceed with the evaluation of the artifact.
- [`REUSABILITY.md`](./REUSABILITY.md) for information about the reusability of the artifact.
