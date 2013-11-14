Clojure core.logic quines.
==========================

This is translation to clojure core.logic for scheme relational interpreter in miniKanren.
Using [@webyrd quines repo](https://github.com/webyrd/quines/) as source.

- warmup.clj is to get feel of miniKanren/core.logic
- numbers.clj — translated [numbers.scm](https://github.com/webyrd/quines/blob/master/numbers.scm)
- interp.clj — Non-relational λ-calculus interpreter (from quines paper, TODO: write a name)
- core.clj — Relational λ-calculus interpreter

The final purpose is to learn how Will did that and to make similar interpreter for λ-calculus.
