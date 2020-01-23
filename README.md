# ring_exp: A Lean tactic for normalising ring expressions with exponents

This repository contains additional source code for the short paper "A Lean tactic for normalising ring expressions with exponents".
The tactic has been merged into [mathlib](https://github.com/leanprover-community/mathlib) and the latest version can be found at https://github.com/leanprover-community/mathlib/blob/master/src/tactic/ring_exp.lean

## Setting up Lean and mathlib

The code has been tested using [Lean 3.4.2](https://github.com/leanprover/lean).
The most flexible way to install Lean is to use [elan](https://github.com/khoek/elan/).
More general installation instructions can be found in [the Lean reference manual](https://leanprover.github.io/reference/using_lean.html).

Within an editor, Lean can be used interactively with [lean-mode](https://github.com/leanprover/lean-mode) for Emacs or the [Lean extension](https://marketplace.visualstudio.com/items?itemName=jroesch.lean) for Visual Studio Code.

This repository depends on parts of [mathlib](https://github.com/leanprover-community/mathlib).
To make working with mathlib easier, you can follow the [installation instructions for Mathlib](https://github.com/leanprover-community/mathlib#Installation), then run `leanpkg configure; update_mathlib` in this repository.
Alternatively, if you don't mind waiting a few minutes or the cached builds of `mathlib` are unavailable, you can run `leanpkg build` in this repository to compile all dependencies yourself.

## Running test cases

The test cases for `ring_exp` are automatically run by continuous integration.
They are also included in this repository in the file `test/ring_exp.lean`. 
You can open this in your editor to interactively inspect the tests.
To execute the tests, run `lean --make test/ring_exp.lean` in this repository.

## Running benchmarks

The two benchmarks for `ring_exp` are implemented in Haskell.
Apart from GHC 8.6, you will need to install the following dependencies: `bytestring-0.10.8.2`, `extra-1.6.18`, `QuickCheck-2.13.2`, `text-1.2.3.1`, `typed-process-0.2.6.0`.
To execute the arbitrary expression benchmark, run `ghc ArbitraryExpr.hs; ./ArbitraryExpr > output/benchmark-expr-user.csv`.
To execute the linear expression benchmark, run `ghc ArbitraryLinexpr.hs; ./ArbitraryLinexpr > output/benchmark-linexpr-user.csv`.
The files `output/benchmark-expr.csv` and `output/benchmark-linexpr.csv` contain the results for previous runs.

The Gnuplot scripts to analyse the benchmark output and plot the times in a graph are `linexpr.gnuplot` (for linear expressions), `expr-large.gnuplot` (for arbitrary expressions) and `expr-small.gnuplot` (zooms in on the lower running times for arbitrary expressions).
The running times are plotted as scatter plots, and can be found in `output/{expr-large,expr-small,linexpr}.png`.
