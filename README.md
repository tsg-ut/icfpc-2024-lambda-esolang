# ICFPC 2024 Team TSG codes

## Solutions

Solutions of my team exists in [solutions](solutions).

## ICFP Language Parser and Evaluator

The OCaml part of the code is based on https://github.com/satos---jp/lambda_esolang

### Setup

1. install opam
2. `$ opam switch create . && opam install .`

### How to use

```
$ cat i
B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK
$ dune exec ./bin/icfpc.exe < i
```

### How to Lambda Pachinko

```
$ dune exec -- ./bin/icfpc_lambdaman.exe -i 17
```

## ICFPC Web UI (hakatashi waiwai tool)

This tool implements the following functionarity.

* Universe Communitation Tool
* ICFP Language Parser and Evaluator (in TypeScript)
* Naive compression algorithm for several type of inputs
* Testcase and AC results viewer
* 3d language simulator
* 3d language visualiser

### How to run

```
cd tools
npm install
npx tsc
npx tsx server.ts
```

## Solvers

### Lambdaman Dumb Solver

```
cd solvers
g++ lambdaman_dumb.cpp -o lambdaman_dumb
./lambdaman_dumb < ../testcases/lambdaman20 > out.txt
```

### Spaceship Beam Search Solver

[solvers/spaceship](solvers/spaceship)

## Spaceship Visualizer

[tools/spaceship-visualiver](tools/spaceship-visualizer)
